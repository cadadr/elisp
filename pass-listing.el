;;; pass-listing.el --- Listing UI for password-store.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017, 2018, 2019, 2020  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Maintainer: Göktuğ Kayaalp <self@gkayaalp.com>
;; Version: 0.1.0
;; Keywords: unix
;; URL: https://dev.gkayaalp.com/elisp/index.html#pass-listing-el
;; Package-Requires: ((password-store "0.1") (emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Lists passwords in pass(1), through ‘password-store.el’, where
;; hitting RET or clicking on an item expands it to show the contents
;; of the related file in pass database.

;; Run M-x passwords to bring it up.

;; See docstring for ‘pass-listing-mode’ for more info.

;;; Code:
(require 'password-store)

;;; The Major Mode:
(defgroup pass-listing '()
  "Emacs mode for pass-listing-mode."
  :prefix "pass-listing-"
  :group 'pass-listing)

;;;; Keymap:
(progn                                  ; Make sure C-M-x always gets both...
  (defvar pass-listing-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [?g] 'pass-listing-mode)
      (define-key map [?q] (lambda () (interactive) (kill-buffer (current-buffer))))
      (define-key map [?d] 'pass-listing--remove)
      (define-key map [?e] 'pass-listing--edit)
      (define-key map [?i] 'pass-listing--insert)
      (define-key map [?+] 'pass-listing--generate)
      (define-key map [?r] 'pass-listing--regenerate)
      (define-key map [?R] 'pass-listing--rename)
      (define-key map [?y] 'pass-listing--copy)
      (define-key map [?n] 'widget-forward)
      (define-key map [?p] 'widget-backward)
      (define-key map [?s] 'isearch-forward)
      (define-key map [?/] 'isearch-forward)
      map)
    "Keymap for `pass-listing-mode'")

  (defvar pass-listing-mode--composed-map
    (make-composed-keymap pass-listing-mode-map widget-keymap)))

(defvar pass-listing--buffer-name
  "*Passwords*"
  "Base buffer name for `pass-listing-mode'.")

;;;; Mode definition:
(defvar-local pass-listing--passwords nil)
(defvar-local pass-listing--marked nil)
(defvar-local pass-listing--open nil)

(define-derived-mode pass-listing-mode
  special-mode "Passwords"
  "Expandable list UI for password-store.
\\<pass-listing-mode--composed-map>

List all passwords, one per line, where hitting
\\[widget-button-press] on a line inserts below that line the
contents of the related file.\n

Keybindings for `pass-listing-mode':\n
\\{pass-listing-mode--composed-map}"
  :group 'pass-listing
  ;;; Setup.
  ;; Prepare buffer.
  (buffer-disable-undo)
  (setq-local default-directory (password-store-dir))
  (setq-local word-wrap t)
  (setq-local show-paren-mode nil)
  (hl-line-mode)
  (use-local-map pass-listing-mode--composed-map)
  ;; Insert passwords.
  (setq pass-listing--passwords
        (sort (password-store-list)
              (lambda (s1 s2) (string-collate-lessp s1 s2 nil t))))
  (save-excursion
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (format
        (substitute-command-keys
         (concat
          "This is the password listing for the database at ‘%s’.\n"
          "Hit \\[widget-button-press] or \\[widget-button-click]"
          " on any button in order to toggle"
          " entries, the contents of the related passwords entry will"
          " be listed below the button.\n\n"))
        (password-store-dir)))
      (dolist (each pass-listing--passwords)
        (pass-listing--insert-item each)))
    (widget-setup)))

;;; Interface:
;;;###autoload
(defun pass-listing ()
  "List passwords in pass(1).
See ‘pass-listing-mode’ for more information."
  (interactive)
  (let ((buffer (get-buffer-create
                 pass-listing--buffer-name)))
    (with-current-buffer buffer
      (pass-listing-mode))
    (switch-to-buffer buffer)))

(defalias 'passwords 'pass-listing)
(defalias 'pass 'pass-listing)

;;; Functions:
(defmacro pass-listing--defcmd1 (name docstring &rest body)
  "Define a command that acts on an entry.
You will have the lexical variable ‘w’ available to you as the
widget's plist's cdr; see ‘widget-at’.  Also, ‘it’ and ‘p’ are
available, the former is the ‘:value’ field from the plist, and
the latter is the value of ‘point’."
  (declare (indent defun))
  `(pass-listing--defcmd2 ,name ,docstring
     (if-let* ((w (cdr (widget-at (point)))))
         (let ((it (plist-get w :value))
               (p (point)))
           ,@body)
       (user-error "Point is not on a password entry!"))))

(defmacro pass-listing--defcmd2 (name docstring &rest body)
  "Define a generic command."
  (declare (indent defun))
  `(defun ,(intern (concat "pass-listing--" (symbol-name name))) ()
     ,docstring
     (interactive)
     ,@body))

(pass-listing--defcmd1 remove
  "Remove the entry under point; see ‘password-store-remove’."
  (when (yes-or-no-p (format "Really remove %s? " it))
    (password-store-remove it)))

(pass-listing--defcmd1 edit
  "Edit the entry under point; see ‘password-store-edit’."
  (password-store-edit it))

(pass-listing--defcmd2 insert
  "Insert an entry; see ‘password-store-insert’."
  (password-store-insert (read-string "Name for the new entry (inserting): ")
                         (read-string "Contents: ")))

(pass-listing--defcmd2 generate
  "Generate an entry; see ‘password-store-generate’."
  (let ((name (read-string "Name for the new entry (generating): ")))
    (password-store-generate name (read-number "Password length: "))
    (when (y-or-n-p "Copy new password to clipboard?")
      (password-store-copy name))))

(pass-listing--defcmd1 regenerate
  "Regenerate an entry.
Useful for resetting passwords.  "
  (password-store-generate
   it
   (read-number
    (eval-when-compile
      (concat
       "Warning: This will reset all the contents of the "
       "password file, though git history can be used to "
       "retrieve any past information.\nPassword length: "))))
  (when (y-or-n-p "Copy new password to clipboard?")
    (password-store-copy it)))

(pass-listing--defcmd1 rename
  "Rename an entry."
  (password-store-rename it (read-string "Rename entry to: ")))

(pass-listing--defcmd1 copy
  "Copy an entry."
  (password-store-copy it))

(defun pass-listing--insert-item (item)
  (widget-create 'push-button
                 :notify #'pass-listing--toggle item)
  (insert "\n"))

(defun pass-listing--toggle (widget &rest ignore)
  (let* ((w (cdr widget))
         (name (plist-get w :value))
         (there (plist-get w :to))
         str)
    (if (member name pass-listing--open)
        (progn
          (let ((inhibit-read-only t))
            (delete-region there (1- (next-button (point)))))
          (setq pass-listing--open
                (remove name pass-listing--open)))
      (progn
        (push name pass-listing--open)
        (setq str (password-store--run-show name))
        (save-excursion
          (goto-char there)
          (widget-insert "\n" str))))))

(provide 'pass-listing)
;;; pass-listing.el ends here
