;;; zathura-sync-theme.el --- Synchronize Zathura's look and feel with Emacs 

;; Copyright (C) 2024 Amol Vaidya

;; Author: Amol Vaidya
;; Version: 20240608.1444
;; Keywords: zathura, theming
;; URL: https://github.com/amolv06/zathura-sync-theme

;;; Commentary:

;; This package is inspired by the blog post at
;; https://blog.akaisuisei.org/communicating-with-zathura-via-dbus.html
;; written by mafty.

(require 'cl-lib)
(require 'dbus)
(require 'subr-x)

(defgroup zathura-sync-theme nil
  "Synchronize Zathura's look and feel with Emacs."
  :prefix "zathura-"
  :group 'applications)

(defun zathura-set (&rest _args)
  "Set colors in Zathura.  `_ARGS' is ignored."
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
				,(cons 'bg-bg
				      (concat "set default-bg "
					      "\""
					      (face-attribute 'shadow :foreground)
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

(defvar zathura-sync-theme--dbus-watch-token nil
  "Signal registration token for watching Zathura D-Bus names.")

(defun zathura-sync-theme--zathura-service-p (name)
  "Return non-nil if NAME looks like a Zathura D-Bus service."
  (and (stringp name)
       ;; Typical name: org.pwmt.zathura.PID-12345
       (string-prefix-p "org.pwmt.zathura.PID-" name)))

(defun zathura-sync-theme--name-owner-changed (name old-owner new-owner)
  "Handle new D-Bus names; theme Zathura when it appears."
  (when (and (zathura-sync-theme--zathura-service-p name)
             (stringp new-owner) (not (string-empty-p new-owner))
             (stringp old-owner) (string-empty-p old-owner))
    ;; Name appeared. Zathura may not be ready immediately, so retry briefly.
    (let ((tries 0)
          (max-tries 30))
      (cl-labels
          ((attempt ()
             (setq tries (1+ tries))
             (condition-case _err
                 (zathura-set)
               (error
                (when (< tries max-tries)
                  (run-at-time 0.05 nil #'attempt))))))
        (attempt)))))

(defun zathura-sync-theme--watch-start ()
  "Start watching for new Zathura instances on D-Bus."
  (unless zathura-sync-theme--dbus-watch-token
    (setq zathura-sync-theme--dbus-watch-token
          (dbus-register-signal
           :session
           "org.freedesktop.DBus"
           "/org/freedesktop/DBus"
           "org.freedesktop.DBus"
           "NameOwnerChanged"
           #'zathura-sync-theme--name-owner-changed))))

(defun zathura-sync-theme--watch-stop ()
  "Stop watching for new Zathura instances on D-Bus."
  (when zathura-sync-theme--dbus-watch-token
    (dbus-unregister-object zathura-sync-theme--dbus-watch-token)
    (setq zathura-sync-theme--dbus-watch-token nil)))

;;;###autoload
(define-minor-mode zathura-sync-theme-mode
  "Synchronize the look and feel of Zathura with Emacs"
  :global t
  :init-value nil
  :lighter "Zathura"
  (if zathura-sync-theme-mode
      (progn (advice-add 'enable-theme :after #'zathura-set)
	     (zathura-sync-theme--watch-start)
	     (zathura-set))
    (progn (zathura-sync-theme--watch-stop)
	   (advice-remove 'enable-theme #'zathura-set))))

(provide 'zathura-sync-theme)

;;; zathura-sync-theme.el ends here
