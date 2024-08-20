;;; zathura-sync-theme.el --- Synchronize Zathura's look and feel with Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Amol Vaidya

;; Author: Amol Vaidya
;; Version: 20240608.1444
;; Keywords: zathura, theming
;; URL: https://github.com/amolv06/zathura-sync-theme

;;; Commentary:

;; This package is inspired by the blog post at
;; https://blog.akaisuisei.org/communicating-with-zathura-via-dbus.html
;; written by mafty.

;;; Code:

(require 'cl-lib)

(defun zathura-set (&rest _args)
  (let ((zathura-services (cl-remove-if-not (lambda (x) (cl-search "zathura" x))
					    (dbus-list-names :session)))
	(zathura-path "/org/pwmt/zathura")
	(zathura-interface "org.pwmt.zathura")
	(zathura-method "ExecuteCommand")
	(zathura-timeout 3000)
	(zathura-message-alist `(,(cons 'main-fg
				      (concat "set recolor-darkcolor "
					      "\""
					      (face-attribute 'default :foreground)
					      "\""))
				,(cons 'main-bg
				      (concat "set recolor-lightcolor "
					      "\""
					      (face-attribute 'default :background)
					      "\""))
				,(cons 'default-fg
				      (concat "set default-fg "
					      "\""
					      (face-attribute 'default :foreground)
					      "\""))
				,(cons 'default-bg
				      (concat "set default-bg "
					      "\""
					      (face-attribute 'default :background)
					      "\""))
				,(cons 'mode-line-bg
				      (concat "set statusbar-bg "
					      "\""
					      (face-attribute 'default :background nil 'default)
					      "\""))
				,(cons 'mode-line-fg
				      (concat "set statusbar-fg "
					      "\""
					      (face-attribute 'default :foreground nil 'default)
					      "\""))
				,(cons 'recolor (concat "set recolor true")))))
    (dolist (svc zathura-services)
      (dolist (msg zathura-message-alist)
	(dbus-call-method :session
			  svc
			  zathura-path
			  zathura-interface
			  zathura-method
			  :timeout zathura-timeout
			  (cdr msg))))))

;;;###autoload
(define-minor-mode zathura-sync-theme-mode
  "Synchronize the look and feel of Zathura with Emacs"
  :global t
  :init-value nil
  :lighter "Zathura"
  (if zathura-sync-theme-mode
      (advice-add 'enable-theme :after #'zathura-set)
    (advice-remove 'enable-theme #'zathura-set)))

(provide 'zathura-sync-theme)
;;; zathura-sync-theme.el ends here
