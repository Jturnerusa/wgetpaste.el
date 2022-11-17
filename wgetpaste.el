(defgroup wgetpaste nil
  "Wgetpaste Emacs functions")

(defcustom wgetpaste-executable
  "wgetpaste"
  "Path to wgetpaste executable")

(defcustom wgetpaste-buffer "*wgetpaste*"
  "Buffer name used for wgetpaste output")

(defun wgetpaste-list-services ()
  (let ((output (shell-command-to-string (format "%s --completions --list-services"
                                                 wgetpaste-executable))))
    (string-lines output t)))

(defun wgetpaste-list-expirations (service)
  (let ((output (shell-command-to-string (format "%s --service %s --completions --list-expiration"
                                                 wgetpaste-executable
                                                 service))))
    (string-lines output t)))

(defun wgetpaste-list-languages (service)
  (let ((output (shell-command-to-string (format "%s --service %s --completions --list-languages"
                                                 wgetpaste-executable
                                                 service))))
    (string-lines output t)))

(provide 'wgetpaste)
