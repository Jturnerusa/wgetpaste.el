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

(require 'ansi-color)

(defgroup wgetpaste nil
  "Wgetpaste interface for emacs")

(defcustom wgetpaste-stdout-buffer "*wgetpaste stdout*"
  "Wgetpaste stdout buffer"
  :type '(string)
  :group 'wgetpaste)

(defcustom wgetpaste-stderr-buffer "*wgetpaste stderr*"
  "Wgetpaste stderr buffer"
  :type '(string)
  :group 'wgetpaste)

(defcustom wgetpaste-executable "wgetpaste"
  "Wgetpaste executable"
  :type '(string)
  :group 'wgetpaste)

(defcustom wgetpaste-args nil
  "Wgetpaste arguments"
  :type '(repeat string)
  :group 'wgetpaste)

(defcustom wgetpaste-install-hooks t
  "Install default wgetpaste hooks"
  :type '(boolean)
  :group 'wgetpaste)

(defcustom wgetpaste-before-upload-hook nil
  "Hooks to run before uploading a paste"
  :type '(hook)
  :group 'wgetpaste)

(defcustom wgetpaste-after-upload-hook nil
  "Hooks to run after wgetpaste process exits successfully"
  :type '(hook)
  :group 'wgetpaste)

(defcustom wgetpaste-upload-failure-hook nil
  "Hooks to run after wgetpaste process exits unsuccessfully"
  :type '(hook)
  :group 'wgetpaste)

(defcustom wgetpaste-sentinel (lambda (process _)
                                (unless (process-live-p process)
                                  (run-hooks (if (zerop (process-exit-status process))
                                                 'wgetpaste-after-upload-hook
                                               'wgetpaste-upload-failure-hook))))
  "Sentinel function to install to wgetpaste process"
  :type '(function)
  :group 'wgetpaste)

(defun wgetpaste-buffer ()
  (run-hooks 'wgetpaste-before-upload-hook)
  (let ((process (make-process
                  :name "wgetpaste"
                  :command `(,wgetpaste-executable ,@wgetpaste-args)
                  :connection-type 'pipe
                  :buffer (get-buffer-create wgetpaste-stdout-buffer)
                  :stderr (get-buffer-create wgetpaste-stderr-buffer)
                  :sentinel wgetpaste-sentinel)))
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

(defun wgetpaste-file (file)
  (interactive (list (read-file-name "wgetpaste file: ")))
  (with-temp-buffer
    (insert-file-contents-literally file)
    (wgetpaste-buffer)))

;; hooks

(defun wgetpaste-ansifilter ()
  (ansi-color-filter-region (point-min) (point-max)))

(defun wgetpaste-clear-stdout-buffer ()
  (with-current-buffer (get-buffer-create wgetpaste-stdout-buffer)
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
  (add-hook 'wgetpaste-before-upload-hook 'wgetpaste-ansifilter)
  (add-hook 'wgetpaste-after-upload-hook 'wgetpaste-save-url-to-clipboard)
  (add-hook 'wgetpaste-upload-failure-hook 'wgetpaste-failed))

(provide 'wgetpaste)
;;; wgetpaste.el ends here
