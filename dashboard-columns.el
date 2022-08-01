;;; dashboard-columns.el --- Columns for emacs dashboard.   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require  'cl-lib)
(require 'dashboard)
(require 'all-the-icons)

(defvar dashboard-columns-old-items nil
  "Store dashboard-items when columns are activated.")

(defun dashboard-columns--insert-section (name list config shortcut action)
  "Add a section with NAME, take CONFIG items from LIST if CONFIG  is a number.
CONFIG could also be a pair (ITEMS . COLUMNS) where ITEMS is the number of items
to take from LIST and COLUMNS is the number of columns to use in that section.
Add SHORTCUT to reach section and ACTION is for the widget action of each item."
  (let ((size (or (and (numberp config) config)
                  (car config)))
        (columns (or (and (numberp config) 2) ;; Defult number of columns.
                     (cdr config))))
    (progn
      (dashboard-columns--insert-heading name shortcut)
      (dashboard-columns--insert
       (cl-subseq list 0 (min (length list) size)) columns action))))

(defun dashboard-columns--insert-heading (name shortcut)
  "Insert a heading section with NAME and a SHORTCUT."
  (insert (format "%s " (all-the-icons-octicon
                         (cdr (assoc shortcut dashboard-heading-icons))
                         :height 1.2 :v-adjust 0.0 :face 'dashboard-heading)))
  (insert (propertize name 'face 'dashboard-heading))
  (dashboard-columns--insert-shortcut shortcut))

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

(defun dashboard-columns--insert (widget columns action)
  "Insert WIDGET with ACTION in buffer splited in COLUMNS.
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

          (widget-create 'item
                         :tag tag
                         :action action
                         :value tag
                         :button-face 'dashboard-items-face
                         :mouse-face 'highlight
                         :button-prefix "  "
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
     (let* ((item (widget-value widget))
            (file (get-text-property 0 'dashboard-agenda-file item))
            (point (get-text-property 0 'dashboard-agenda-loc item)))
       (funcall dashboard-agenda-action file point)))))

;; Projects
(defun dashboard-columns--insert-projects (config)
  "Overwrite `dashboard-insert-projects' pass CONFIG as argument."
  (dashboard-columns--insert-section
   "Projects:"
   (dashboard-columns--list-projects)
   config
   'projects
   (lambda (widget &rest _)
     (let ((action (dashboard-projects-backend-switch-function))
           ;; TODO: ensure get property from correct position
           (file (get-text-property 4 'dashboard-project-path
                                    (widget-value widget))))
       (funcall action file)))))

(defun dashboard-columns--list-projects ()
  "List the projects for columns."
  (let ((projects (dashboard-projects-backend-load-projects)))
    (mapcar 'dashboard-columns--format-project projects)))

(defun dashboard-columns--format-project (project)
  "Format PROJECT for dashboard, includes properties."
  (let* ((path (expand-file-name project))
         (name (file-name-nondirectory (directory-file-name project))))
    (add-text-properties 0 (length name)
                         (list 'dashboard-project-name name
                               'dashboard-project-path path)
                         name)
    (format "%s %s - %s"
            (all-the-icons-icon-for-dir project :heigth 1.2 :v-adjust 0.0 )
            name project)))

;; Bookmarks
(defun dashboard-columns--insert-bookmarks (config)
  "Use CONFIG to insert bookmarks in dashboard."
  (require 'bookmark)
  (dashboard-columns--insert-section
   "Bookmarks:"
   (dashboard-columns--bookmarks)
   config
   'bookmarks
   (lambda (widget &rest _) (bookmark-jump (widget-value widget)))))

(defun dashboard-columns--bookmarks ()
  "Return a list of formatted bookmarks."
  (mapcar 'dashboard-columns--bookmarks-format (bookmark-all-names)))

(defun dashboard-columns--bookmarks-format (bookmark)
  "Format a BOOKMARK."
  (let ((filename bookmark)
        (path (expand-file-name bookmark)))
    (add-text-properties 0 (length bookmark)
                         (list 'dashboard-path path
                               'dashboard-filename filename)
                         bookmark)
    (format "%s %s"
            (all-the-icons-icon-for-file bookmark :heigth 1.2 :v-adjust 0.0)
            bookmark)))

;;;###autoload;
(defun dashboard-columns-activate (items)
  "Define alias for `dashboard-insert' to insert ITEMS."
  (interactive)
  (setq dashboard-columns-old-items dashboard-items)
  (setq dashboard-items items)
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
                 'dashboard-columns--insert-bookmarks))

(provide 'dashboard-columns)
;;; dashboard-columns.el ends here
