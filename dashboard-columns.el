;;; dashboard-columns.el --- Columns for emacs dashboard.   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require  'cl-lib)
(require 'dashboard)
(require 'all-the-icons)

(defvar dashboard-columns-old-items nil
  "Store dashboard-items when columns are activated.")

(defun dashboard-columns--insert-section (title list config shortcut action)
  "Add a section with TITLE, take CONFIG items from LIST if CONFIG  is a number.
CONFIG could also be a pair (ITEMS . COLUMNS) where ITEMS is the number of items
to take from LIST and COLUMNS is the number of columns to use in that section.
Add SHORTCUT to reach section and ACTION is for the widget action of each item."
  (let* ((size (or (and (numberp config) config)
                   (car config)))
         (columns (or (and (numberp config) 2) ;; Default number of columns.
                      (cdr config)))
         (items (cl-subseq list 0 (min (length list) size))))
    (progn
      (dashboard-columns--insert-heading title shortcut)
      (dashboard-columns--insert title items columns action shortcut))))

(defun dashboard-columns--insert-heading (name shortcut)
  "Insert a heading section with NAME and a SHORTCUT."
  (insert (dashboard-columns--insert-heading-icon shortcut))
  (insert (propertize name 'face 'dashboard-heading))
  (dashboard-columns--insert-shortcut shortcut))

(defun dashboard-columns--insert-heading-icon (shortcut)
  "Insert heading icon for SHORTCUT."
  (format "%s " (all-the-icons-octicon
                 (cdr (assoc shortcut dashboard-heading-icons)))))

(defun dashboard-columns--insert-shortcut (shortcut)
  "Insert SHORTCUT into dashboard.
Define a function `dashboard-go-to-<section>'"
  (let* ((shortcut-symbol (dashboard-get-shortcut shortcut))
         (shortcut-point (point))
         (docstring (format "Shortcut for dashboard-columns section: %s"
                            shortcut)))
    (insert (concat " (" shortcut-symbol ")\n\n"))
    (define-key dashboard-mode-map shortcut-symbol
      (lambda () docstring
        (interactive)
        (goto-char shortcut-point) (forward-line)))))

(defun dashboard-columns--insert (name widget columns action section)
  "Insert WIDGET NAME with ACTION in buffer splited in COLUMNS.
WIDGET is a list of widget-buttons that are basically strings."
  (let ((column-length (ceiling (- (frame-width) (* 4 columns)) columns)))
    (dolist (group (reverse (dashboard-columns--slice widget columns)))
      (dolist (item (reverse group))
        (let* ((item-width (string-width item))
               ;; Truncate element TAG
               (tag (if (< item-width column-length) item
                      (truncate-string-to-width item (- column-length 1))))
               (format-padding (if (< item-width column-length)
                                   (format "%%%ss" (- column-length item-width))
                                 "%1s"))
               (padding (format format-padding "")))
          (add-text-properties 0 (string-width tag)
                               (list 'dashboard-section section)
                               tag)
          (widget-create 'item
                         :tag tag
                         :action action
                         :value tag
                         :button-face 'dashboard-items-face
                         :mouse-face 'highlight
                         :button-prefix " "
                         :button-suffix padding
                         :format "%[%t%]")
          ))
      (insert "\n"))))

(defun dashboard-columns--slice (list columns)
  ;; TODO: Generate columns horizontal or vertical
  "Slice LIST in sublist of COLUMNS size."
  (dashboard-columns--group list columns (list) (list)))

(defun dashboard-columns--group (list columns group result)
  "Group the LIST in COLUMNS using GROUP and RESULT as to carry results."
  (cond
   ((null list) (cons group result))
   ((length< group columns)
    (dashboard-columns--group (cdr list) columns
                              (cons (car list) group) result))
   (t (dashboard-columns--group (cdr list) columns
                                (list (car list)) (cons group result)))))


(defun dashboard-columns--action-on-item (widget action &rest params)
  "Call ACTION with WIDGET item as param."
  (let* ((item (widget-value widget))
         (action-params (dashboard-columns--attributes item params)))
    (apply action action-params)))

(defun dashboard-columns--attributes (item params)
  "Get text properites in PARAMS list from ITEM."
  (let* ((dashboard-prefix "dashboard-")
         (properties
          (mapcar (lambda (property)
                    (concat dashboard-prefix (symbol-name property)))
                  params)))
    (mapcar (lambda (property)
              (get-text-property 0 (intern property) item))
            properties)))

;; Overwrite dashboard inserts

;; Agenda
(defun dashboard-columns--insert-agenda (config)
  "Alias for agenda using CONFIG."
  (require 'org-agenda)
  (dashboard-columns--insert-section
   (if dashboard-week-agenda "Agenda for the coming week:" "Agenda for today:")
   (dashboard-agenda--sorted-agenda)
   config
   'agenda
   (lambda (widget &rest _)
     (dashboard-columns--action-on-item widget dashboard-agenda-action
                                        'agenda-file 'agenda-loc))))

(defun dashboard-columns--remove-agenda ()
  "Call for `dashboard-columns--remove-item' over an agenda item."
  (dashboard-columns--action-on-item
   (widget-at (point))
   ;; This could be remove-agenda-item function
   (lambda (file point)
     (with-current-buffer (find-file-noselect file)
       (goto-char point)
       (call-interactively 'org-todo)))
   'agenda-file 'agenda-loc))

;; Projects
(defun dashboard-columns--insert-projects (config)
  "Overwrite `dashboard-insert-projects' pass CONFIG as argument."
  (dashboard-columns--insert-section
   "Projects:"
   (dashboard-columns--list-projects)
   config
   'projects
   (lambda (widget &rest _)
     (dashboard-columns--action-on-item widget
                                        (dashboard-projects-backend-switch-function)
                                        'project-path))))

(defun dashboard-columns--remove-projects ()
  "Call for `dashboard-columns--remove-item' over a project item."
  (dashboard-columns--action-on-item
   (widget-at (point))
   'dashboard-columns--remove-project
   'project-path))

(defun dashboard-columns--remove-project (path)
  "Call corresponding backend removing function with PATH as argument."
  (dashboard-mute-apply
    (cl-case dashboard-projects-backend
      (`projectile (projectile-remove-known-project path))
      (`project-el (project-forget-projects-under path)))))

(defun dashboard-columns--list-projects ()
  "List the projects for columns."
  (let ((projects (dashboard-projects-backend-load-projects)))
    (mapcar 'dashboard-columns--format-project projects)))

(defun dashboard-columns--format-project (project)
  "Format PROJECT for dashboard, includes properties."
  (let* ((path (expand-file-name project))
         (name (file-name-nondirectory (directory-file-name project)))
         (project-format (format "%s %s - %s"
            (all-the-icons-icon-for-dir project :heigth 1.2 :v-adjust 0.0 )
            name project)))
    (add-text-properties 0 (length project-format)
                         (list 'dashboard-project-name name
                               'dashboard-project-path path)
                         project-format)
    project-format))

;; Bookmarks
(defun dashboard-columns--insert-bookmarks (config)
  "Use CONFIG to insert bookmarks in dashboard."
  (require 'bookmark)
  (dashboard-columns--insert-section
   "Bookmarks:"
   (dashboard-columns--bookmarks)
   config
   'bookmarks
   (lambda (widget &rest _)
     (dashboard-columns--action-on-item (widget-at (point))
                                        'bookmark-jump
                                        'filename))))

(defun dashboard-columns--remove-bookmarks ()
  "Call `dashboard-columns--remove-bookmark' with widget at point."
  (dashboard-columns--action-on-item (widget-at (point))
                                     'bookmark-delete
                                     'filename))

(defun dashboard-columns--bookmarks ()
  "Return a list of formatted bookmarks."
  (mapcar 'dashboard-columns--bookmarks-format (bookmark-all-names)))

(defun dashboard-columns--bookmarks-format (bookmark)
  "Format a BOOKMARK."
  (let ((filename bookmark)
        (path (expand-file-name bookmark))
        (bookmark-format (format "%s %s"
                                 (all-the-icons-icon-for-file bookmark :heigth 1.2 :v-adjust 0.0)
                                 bookmark)))
    (add-text-properties 0 (length bookmark-format)
                         (list 'dashboard-path path
                               'dashboard-filename filename)
                         bookmark-format)
    bookmark-format))

(defun dashboard-columns--remove-bookmark (bookmark)
  "Remove BOOKMARK from dashboard.")

;; Remove items
(defun dashboard-columns--remove-item ()
  "Overwrite `dashboard-remove-item-under'."
  (interactive)
  (let* ((section (get-text-property 0 'dashboard-section
                                     (widget-value (widget-at (point)))))
         (section-name (symbol-name section))
         (command (concat "dashboard-columns--remove-" section-name))
         (remove-command (intern command)))
    (funcall remove-command)))

;;;###autoload;
(defun dashboard-columns-activate (items)
  "Define alias for `dashboard-insert' to insert ITEMS."
  (interactive)
  (setq dashboard-columns-old-items dashboard-items)
  (setq dashboard-items items)
  (advice-add 'dashboard-remove-item-under :override
              'dashboard-columns--remove-item)
  (advice-add 'dashboard-insert-agenda :override
              'dashboard-columns--insert-agenda)
  (advice-add 'dashboard-insert-projects :override
              'dashboard-columns--insert-projects)
  (advice-add 'dashboard-insert-bookmarks :override
              'dashboard-columns--insert-bookmarks))

;;;###autoload;
(defun dashboard-columns-deactivate ()
  "Undefined the aliases for dashboards insert's."
  (interactive)
  (setq dashboard-items dashboard-columns-old-items)
  (advice-remove 'dashboard-insert-agenda
                 'dashboard-columns--insert-agenda)
  (advice-remove 'dashboard-insert-projects
                 'dashboard-columns--insert-projects)
  (advice-remove 'dashboard-insert-bookmarks
                 'dashboard-columns--insert-bookmarks)
  (advice-remove 'dashboard-remove-item-under
                 'dashboard-columns--remove-item))

(provide 'dashboard-columns)
;;; dashboard-columns.el ends here
