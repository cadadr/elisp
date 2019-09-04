;;; onchange.el --- watch and respond to file system notifications  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: files, processes, tools

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

;;; Commentary:

;; onchange.el features an Emacs Lisp filesystem watcher utility for
;; responding to changes to files and directories by executing a
;; command, be it an Emacs Lisp command or a shell command.

;; There are alternatives which are separate executables which are
;; usable within (using ‘shell-mode’, subprocesses or similar) or
;; without Emacs; this program differs from those in (1) using
;; ‘filenotify.el’ from Emacs proper and (2) being more tightly
;; integrated (and, conversely, coupled) with Emacs given it’s
;; implemented in pure Emacs Lisp.

;; TODO:
;;
;; - filtering
;;
;; - when a file created under recursive directory watch, add that
;;   file to the watch
;;
;; - Display a widget buffer where user can add, remove watchers; or
;;   stop the task
;;
;; - Store interactive calls in a file so that they can be recalled
;;   later

;;; Code:



;;;; Prelude:

(require 'cl-lib)
(require 'filenotify)

;;;; Variables:

(defvar onchange--basic-buffer-name "*Onchange*")

(defvar onchange--filenotify-flags '(change))



;;;; Helper procedures:

(defun onchange--stop-all-watchers ()
  (interactive)
  ;; Development helper: just forcibly stop all watchers.
  (mapcar #'file-notify-rm-watch (hash-table-keys file-notify-descriptors)))

;; XXX(2019-09-04): This only handles cases where the command is
;; waited in Emacs, e.g. a synchronous shell command or and Emacs
;; function that does not run an asynchronous subprocess.  M-x
;; compile, for example, wouldn’t work with this, there needs to be a
;; way to signal the completion of the task with this sort of
;; commands.
(defun onchange--with-notifications-paused (callback)
  (let* ((watchers (buffer-local-value 'onchange--watchers (current-buffer)))
         ;; Remove watchers for the duration of the execution of the body...
         descriptors)
    (dolist (w watchers)
      (push
       (let ((d (gethash w file-notify-descriptors)))
         (cons (file-notify--watch-filename d)
               (file-notify--watch-callback d)))
       descriptors))
    (mapc #'file-notify-rm-watch watchers)
    (setq-local onchange--watchers nil)
    (funcall callback)
    ;; ... and then reinstantiate them using the data retrieved from
    ;; the hashmap.
    (pcase-dolist (`(,file . ,callback) descriptors)
      (setq-local onchange--watchers
                  (cons
                   (file-notify-add-watch file
                                          onchange--filenotify-flags
                                          callback)
                   onchange--watchers)))))

(defun onchange--make-callback (buffer watch-for-creations)
  (lambda (event)
    (pcase event
      (`(,descriptor ,action ,file . ,maybe-file1)
       (unless (or (memq action '(stopped attribute-changed))
                   (s-starts-with-p ".#" (file-name-base file)) ;emacs swap file
                   (and watch-for-creations
                        (memq action '(created renamed))))
         (message "Onchange caught notification: action: %S; File: %s" action file)
         (with-current-buffer buffer
           (onchange--with-notifications-paused
            (lambda ()
              (let ((cmd (buffer-local-value 'onchange--command buffer)))
                (pcase cmd
                  ;; Shell command
                  ((pred stringp)
                   (let ((output-buffer (generate-new-buffer
                                         " *onchange process output*")))
                     (with-current-buffer output-buffer
                       (setq-local
                        header-line-format
                        (concat
                         (format-time-string "Onchange process output (%FT%T%z) ")
                         cmd))
                       (shell-command cmd output-buffer))))
                  ;; Emacs Lisp form
                  ((pred listp)
                   (eval cmd))
                  ;; Interactive Emacs command
                  ((pred symbolp)
                   (call-interactively cmd)
                   )
                  (_ (user-error
                      "Bad onchange command: not a string, Lisp form, \
or symbol"))))))))))))

(defun onchange--add-watch (buffer file-or-directory recursive watch-for-creations)
  (if (and recursive (file-directory-p file-or-directory))
      (cl-loop for f in (directory-files-recursively file-or-directory "")
               with result = nil
               collect (onchange--add-watch-1 buffer f watch-for-creations)
               into result
               finally return result)
    (list (onchange--add-watch-1 buffer file-or-directory watch-for-creations))))

(defun onchange--add-watch-1 (b f w)
  "Subroutine of ‘onchange--add-watch’."
  (file-notify-add-watch
   f onchange--filenotify-flags (onchange--make-callback b w)))

(defun onchange--file-list-effectively-empty-p (files)
  "Whether all filenames, if any, are empty strings. "
  (null (cl-remove-if #'string-empty-p (mapcar #'string-trim files))))



;;;; Entry point:

;;;###autoload
(cl-defun onchange (files
                    shell-command-or-lisp-form
                    &optional
                    (watch-for-creations t)
                    (recursive t))
  (interactive (onchange--1))
  (let ((buffer (generate-new-buffer onchange--basic-buffer-name))
        watchers)
    (with-current-buffer buffer
      (condition-case err
          (progn
            ;; If no files were specified, watch the current directory
            (when (onchange--file-list-effectively-empty-p files)
              (setf files (list default-directory)))
            (cl-loop
             for file-or-directory in files
             nconc (onchange--add-watch
                      buffer file-or-directory recursive watch-for-creations)
             into watchers
             finally do (progn
                          (setq-local onchange--watchers watchers)
                          (setq-local onchange--command
                                      shell-command-or-lisp-form))))
        ('file-notify-error
         (mapc #'file-notify-rm-watch watchers)
         (error "Failed adding watchers: %S" err))))))

(defun onchange--1 ()
  "Subroutine of ‘onchange’."
  (list
   ;; files
   (cl-loop with insert-default-directory = nil
            with result = nil
            collect (condition-case err
                        (expand-file-name
                         (read-file-name
                          (substitute-command-keys
                           "Add file or directory to be watched, \
\\[keyboard-quit] to terminate: ")))
                      ('quit (cl-return result)))
            into result)

   ;; shell-command-or-lisp-form
   (read--expression "Command (symbol for interactive command, \
sexp for Elisp form, string for command): ")

   ;; watch-for-creations
   (y-or-n-p "Watch for new files that get created? ")

   ;; recursive
   (y-or-n-p "Watch directories recursively? ")))

(provide 'onchange)
;;; onchange.el ends here
