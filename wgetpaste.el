;;; wgetpaste.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022  

;; Author: John Turner <jturner.usa@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

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

(defun wgetpaste-build-args (&rest args)
  (let* ((symbols (seq-filter 'symbolp args))
         (flags (seq-reduce (lambda (flags symbol)
                               (pcase symbol
                                 (:language
                                  (append flags (list "--language" (plist-get args :language))))
                                 (:description
                                  (append flags (list "--description" (plist-get args :description))))
                                 (:nick
                                  (append flags (list "--nick" (plist-get args :nick))))
                                 (:service
                                  (append flags (list "--service" (plist-get args :service))))
                                 (:expiration
                                  (append flags (list "--expiration" (plist-get args :expiration))))
                                 (:ignore-configs
                                  (append flags (list "--ignore-configs")))
                                 (error "unknown argument %s" symbol)))
                            symbols
                            nil)))
    flags))

(defun wgetpaste-parse-url-from-process-output (process-output)
  (let* ((split-by-newline (string-lines process-output))
         (first-line (car split-by-newline))
         (split-by-delimiter (string-split first-line ": " t))
         (url (car (last split-by-delimiter))))
    url))

(defun wgetpaste-process-success (process-output)
  (kill-new (wgetpaste-parse-url-from-process-output process-output))
  (message "%s (url added to kill ring)" process-output))

(defun wgetpaste-process-sentinel (process event)
  (unless (process-live-p process)
    (let ((process-output (with-current-buffer (process-buffer process)
                            (buffer-string))))
      (if (zerop (process-exit-status process))
          (wgetpaste-process-success process-output)
        (error "%s" output)))))

(defun wgetpaste-process-start (executable &rest args)
  (if (not (xor (plist-get args :stdin) (plist-get args :files)))
      (error "must provide either stdin or files for wgetpaste to operate on")
    (let ((process (make-process
                    :name "wgetpaste"
                    :buffer (with-current-buffer (get-buffer-create wgetpaste-buffer)
                              (erase-buffer)
                              (current-buffer))
                    :command `(,executable
                               ,@(plist-get args :arguments)
                               ,@(plist-get args :files))                               
                    :connection-type 'pipe
                    :sentinel 'wgetpaste-process-sentinel)))
      (when-let ((stdin (plist-get args :stdin)))
        (process-send-string process stdin)
        (unless (string-suffix-p "\n" stdin)
          (process-send-string process "\n"))
        (process-send-eof process)))))

(provide 'wgetpaste)

;;; wgetpaste.el ends here
