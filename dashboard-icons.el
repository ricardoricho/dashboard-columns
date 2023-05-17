;;; dashboard-icons.el -- Manage icons for dashboard. -*- lexical-binding: t -*-
;; Copyright (c) 2023 emacs-dashboard maintainers
;;
;; Author     : Ricardo Arredondo <ricardo.richo@gmail.com>
;; Maintainer : Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL        : https://github.com/emacs-dashboard/emacs-dashboard
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: April 22, 2023
;; Package-Version: 1.8.0-SNAPSHOT
;; Keywords: startup, screen, tools, dashboard
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:

;; This file manage icons, could be loadad from different external
;; packages as all-the-icons or nerd-icons

;;; Code:

(declare-function nerd-icons-icon-for-dir "ext:nerd-icons.el")
(declare-function nerd-icons-icon-for-file "ext:nerd-icons.el")
(declare-function nerd-icons-sucicon "ext:nerd-icons.el")
(declare-function nerd-icons-octicon "ext:nerd-icons.el")

(declare-function all-the-icons-icon-for-dir "ext:all-the-icons.el")
(declare-function all-the-icons-icon-for-file "ext:all-the-icons.el")
(declare-function all-the-icons-fileicon "ext:all-the-icons.el")
(declare-function all-the-icons-octicon "ext:all-the-icons.el")

(defcustom dashboard-icons-provider nil
  "Library that provides icons."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Nerd icons" nerd-icons)
                 (const :tag "All the icons" all-the-icons))
  :group 'dashboard)

(defun dashboard-icons-octicon (icon &rest options)
  "Call octicon with ICON passing OPTIONS as rest to icon provider."
  (cl-case dashboard-icons-provider
    (`nerd-icons (apply 'nerd-icons-octicon (concat "nf-oct-" icon) options))
    (`all-the-icons (apply 'all-the-icons-octicon icon options))
    (otherwise "")))

(defun dashboard-icons-icon-for-remote (&optional options)
  "Return a remote icon passing OPTIONS to icon provider."
  (cl-case dashboard-icons-provider
    (`nerd-icons (apply 'nerd-icons-codicon "nf-cod-radio_tower" options))
    (`all-the-icons (apply 'all-the-icons-octicon "radio_tower" options))
    (otherwise "")))

(defun dashboard-icons-icon-for-dir (dir &rest options)
  "Return icon for DIR passing OPTIONS to icon provider."
  (cl-case dashboard-icons-provider
    ((nerd-icons) (apply 'nerd-icons-icon-for-dir dir options))
    ((all-the-icons) (apply 'all-the-icons-icon-for-dir dir options))
    (t "")))

(defun dashboard-icons-icon-for-file (file &rest options)
  "Return icon for FILE passing OPTIONS to icon provider."
  (cl-case dashboard-icons-provider
    ((nerd-icons) (apply 'nerd-icons-icon-for-file file options))
    ((all-the-icons) (apply 'all-the-icons-icon-for-file file options))
    (t "")))

(defun dashboard-icons-icon-for-file-or-dir (file-or-dir &rest options)
  "Return an icon for FILE-OR-DIR with OPTIONS."
  (cond
   ((file-remote-p file-or-dir)
    (dashboard-icons-icon-for-remote options))
   ((file-directory-p file-or-dir)
    (dashboard-icons-icon-for-dir file-or-dir options))
   (t (dashboard-icons-icon-for-file file-or-dir options))))

(defun dashboard-icon-footer ()
  "Get the footer icon if DISPLAY-ICONS.
Could be text, all-the-icons or nerd-icon."
  (cl-case dashboard-icons-provider
    ((nerd-icons) (apply 'nerd-icons-sucicon "nf-custom-emacs"))
    ((all-the-icons) (apply 'all-the-icons-fileicon "emacs"))
    (t ">")))

(provide 'dashboard-icons)
;;; dashboard-icons.el ends here
