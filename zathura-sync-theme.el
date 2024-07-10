;;; zathura-sync-theme.el --- Synchronize Zathura's look and feel with Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Amol Vaidya

;; Author: Amol Vaidya
;; Version: 20240710.1000
;; Keywords: faces
;; URL: https://github.com/amolv06/zathura-sync-theme
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This package is inspired by the blog post at
;; https://blog.akaisuisei.org/communicating-with-zathura-via-dbus.html
;; written by mafty.

;;; Code:

(require 'cl-lib)
(require 'dbus)

(defgroup zathura-sync-theme nil
  "Synchronize Zathura's look and feel with Emacs."
  :prefix "zathura-sync-theme-"
  :group 'applications)

(defcustom zathura-theme-config "~/.config/zathura/theme"
  "Config location to put colors into."
  :type 'file
  :group 'zathura-sync-theme)

(defun zathura-write-config ()
  (interactive)
  (with-temp-file zathura-theme-config
    (insert "# synced with emacs theme by zathura-sync-theme"
            "\nset recolor-darkcolor \\" (face-attribute 'default :foreground)
            "\nset recolor-lightcolor \\" (face-attribute 'default :background)
            "\nset default-fg \\" (face-attribute 'default :foreground)
            "\nset default-bg \\" (face-attribute 'default :background)
            "\nset statusbar-bg \\" (face-attribute 'default :background nil 'default)
            "\nset statusbar-fg \\" (face-attribute 'default :foreground nil 'default)
            "\nset recolor true")))

(defun zathura-set (&rest _args)
  "Set colors in Zathura.  `_ARGS' is ignored."
  (let ((zathura-services (cl-remove-if-not (lambda (x) (cl-search "zathura" x))
					    (dbus-list-names :session)))
	(zathura-path "/org/pwmt/zathura")
	(zathura-interface "org.pwmt.zathura")
	(zathura-method "SourceConfig"))

    (zathura-write-config)
    (dolist (svc zathura-services)
      (dbus-call-method-asynchronously :session
                                       svc
                                       zathura-path
                                       zathura-interface
                                       zathura-method
                                       nil))))

;;;###autoload
(define-minor-mode zathura-sync-theme-mode
  "Synchronize the look and feel of Zathura with Emacs."
  :global t
  :group 'zathura-sync-theme
  :init-value nil
  :lighter "Zathura"
  (cond
   (zathura-sync-theme-mode
    (zathura-write-config)
    (advice-add 'enable-theme :after #'zathura-set))

   (t
    (delete-file zathura-theme-config)
    (advice-remove 'enable-theme #'zathura-set))))

(provide 'zathura-sync-theme)
;;; zathura-sync-theme.el ends here
