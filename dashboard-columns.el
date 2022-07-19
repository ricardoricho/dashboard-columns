;;; dashboard-columns.el --- Columns for emacs dashboard.   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require  'cl-lib)
(require 'dashboard)

(defface dashboard-columns-items-face
  '((t (:inherit dashboard-items-face :height 1.0)))
  "Face used for column items."
  :group 'dashboard)

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
  (insert " ")
  ;; INSERT ICON
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
                      (truncate-string-to-width item column-length)))
               (format-padding (if (< item-width column-length)
                                   (format "%%%ss" (- column-length item-width))
                                 "%1s"))
               (padding (format format-padding "")))

          (widget-create 'item
                         :tag tag
                         :action action
                         :value tag
                         :button-face 'dashboard-columns-items-face
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
   (dashboard-projects-backend-load-projects)
   config
   'projects
   (lambda (widget &rest _)
     (let ((action (dashboard-projects-backend-switch-function))
           (file (widget-value widget)))
       (funcall action file)))))

;; ;; Recents
;; (defun dashboard-columns--insert-recents (config)
;;   "Insert recent files widget using CONFIG.  Overwrite `dashboard-insert-recents'."
;;   (dashboard-columns--insert-section
;;    "Recent Files:"
;;    (dashboard-grid--recent-files)
;;    config
;;    'recents
;;    (lambda (widget &rest _)
;;      (find-file-existing (dashboard-grid-expand-path widget)))))

;; (defun dashboard-grid-expand-path (widget)
;;   "Expand file path stor in WIDGET properties."
;;   (let ((widget-file (get-text-property 0 'file-path (widget-value widget))))
;;     widget-file))

;; (defun dashboard-grid--recent-files ()
;;   "Get recent files."
;;   (mapcar 'dashboard-grid--recents-format recentf-list))

;; (defun dashboard-grid--recents-format (recent)
;;   "Format RECENT file."
;;   (let ((filename (format "%s" recent))
;;         (recent-data (list 'file-path (format "%s" recent))))
;;     (add-text-properties 0 (length filename) recent-data filename)
;;     filename))

;;;###autoload;
(defun dashboard-columns-activate ()
  "Define alias for dashboard insert's."
  (interactive)
  (advice-add 'dashboard-insert-agenda :override
              'dashboard-columns--insert-agenda)
  (advice-add 'dashboard-insert-projects :override
              'dashboard-columns--insert-projects))

;;;###autoload;
(defun dashboard-columns-deactivate ()
  "Undefined the aliases for dashboards insert's."
  (interactive)
  (advice-remove 'dashboard-insert-agenda 'dashboard-columns--insert-agenda)
  (advice-remove 'dashboard-insert-projects
                 'dashboard-columns--insert-projects))

(provide 'dashboard-columns)
;;; dashboard-columns.el ends here
