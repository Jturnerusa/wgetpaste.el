;;; wgetpaste.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John Turner

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

(defgroup wgetpaste nil
  "Wgetpaste interface for emacs")

(defcustom wgetpaste-stdout-buffer "*wgetpaste stdout*"
  "Wgetpaste stdout buffer")

(defcustom wgetpaste-stderr-buffer "*wgetpaste stderr*"
  "Wgetpaste stderr buffer")

(defcustom wgetpaste-executable "wgetpaste"
  "Wgetpaste executable")

(defcustom wgetpaste-args nil
  "Wgetpaste arguments")

(defcustom wgetpaste-install-hooks t
  "Install default wgetpaste hooks")

(defcustom wgetpaste-before-upload-hook nil
  "Hooks to run before uploading a paste")

(defcustom wgetpaste-after-upload-hook nil
  "Hooks to run after wgetpaste process exits successfully")

(defcustom wgetpaste-upload-failure-hook nil
  "Hooks to run after wgetpaste process exits unsuccessfully")

(defun wgetpaste-buffer ()
  (run-hooks 'wgetpaste-before-upload-hook)
  (let ((process (make-process
                  :name "wgetpaste"
                  :command `(,wgetpaste-executable ,@wgetpaste-args)
                  :connection-type 'pipe
                  :buffer (get-buffer-create wgetpaste-stdout-buffer)
                  :stderr (get-buffer-create wgetpaste-stderr-buffer)
                  :sentinel (lambda (process event)
                              (unless (process-live-p process)
                                (when (zerop (process-exit-status process))
                                  (run-hooks 'wgetpaste-after-upload-hook)))))))
    (process-send-region process (point-min) (point-max))
    (process-send-eof process)
    process))

(defun wgetpaste ()
  (interactive)
  (let* ((region (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (cons (point-min) (point-max))))
         (work-buffer (generate-new-buffer " *wgetpaste work buffer*" t)))
    (copy-to-buffer work-buffer (car region) (cdr region))
    (with-current-buffer work-buffer
      (wgetpaste-buffer))))

;; hooks

(defun wgetpaste-clear-stdout-buffer ()
  (with-current-buffer wgetpaste-stdout-buffer
    (erase-buffer)))

(defun wgetpaste-save-url-to-clipboard ()
  (with-current-buffer wgetpaste-stdout-buffer
    (let ((url (buffer-substring (point-min) (point-max))))
      (message "%s saved to kill ring" url)
      (kill-ring-save (point-min) (point-max)))))

(defun wgetpaste-failed ()
  (message "wgetpaste failed, see wgetpaste stderr buffer for error information"))

(when wgetpaste-install-hooks
  (add-hook 'wgetpaste-before-upload-hook 'wgetpaste-clear-stdout-buffer)
  (add-hook 'wgetpaste-after-upload-hook 'wgetpaste-save-url-to-clipboard)
  (add-hook 'wgetpaste-upload-failure-hook 'wgetpaste-failed))

(provide 'wgetpaste)
;;; wgetpaste.el ends here
