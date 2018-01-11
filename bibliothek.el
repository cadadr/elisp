;;; bibliothek.el --- Managing a digital library of PDFs  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Version: 0.1.0
;; Keywords: tools
;; URL: http://gkayaalp.com/emacs.html#bibliothek.el
;; Package-Requires: ((emacs "24.4") (pdf-tools "0.70"))

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

;; Bibliothek.el is a management tool for a library of PDFs.  Or it
;; aspires to be so.  It's quite fresh ATM, it'll grow as it gets
;; used more.

;; Presently, bibliothek.el displays PDF files from directories in
;; ‘bibliothek-path’ in a tabulated list [1], where the user can view the
;; file related to row under point (RET on the button or ‘f’ anywhere
;; on the row by default) or bring up a buffer detailed with PDF
;; metadata of that file (bound to ‘i’ by default).  Also,
;; ‘tabulated-list-mode’ provides an interactive buffer header, where
;; by clicking the column headers, the table can be sorted.

;; Some more documentation may be present in the docstring for the
;; ‘bibliothek’ command.



;;;; Installation:

;; Bibliothek.el depends on ‘pdf-tools’.

;; After putting a copy of bibliothek.el in a directory in the
;; ‘load-path’, bibliothek.el can be configured as such:

;; (require 'bibliothek)
;; (setq bibliothek-path (list "~/Documents"))

;; Then, the Bibliothek interface can be brought up via M-x bibliothek RET.

;; [1] (see (info "(elisp) Tabulated List Mode"))

;;; Code:

(require 'pdf-info)
(require 'goto-addr)
(require 'cl-lib)
(require 'tabulated-list)
(require 'button)



;;;; Customisables:

(defgroup bibliothek
  nil
  "Customisations for bibliothek.el, digital PDF library manager."
  :group 'emacs
  :prefix "bibliothek-")

(defvar bibliothek-path nil "A list of paths to look for PDF files.")



;;;; Helper functions:

(defun bibliothek--assoca (keyseq list)
  "Arbitrary depth multi-level alist query."
  (let ((ks (if (listp keyseq) keyseq (list keyseq)))
        (ret list))
    (dolist (k ks ret)
      (setq ret (cdr (assoc k ret))))))

(defun bibliothek--items ()
  "Extract all the PDF files from each directory in ‘bibliothek-path’."
  (let (items)
    (dolist (directory bibliothek-path items)
      (dolist (file (directory-files directory t "\\.pdf$" t))
        (cl-pushnew (cons (cons 'bibliothek--filename file)
                          (pdf-info-metadata file))
                    items)))))

(defun bibliothek--find (&optional marker)
  "Open the PDF file for the row under point.
Optional argument MARKER is passed in when this function is
called from a button."
  ;; This function caters to two cases: 1) when it's called from the
  ;; ‘tabulated-list-mode’ mechanism, where ‘marker’ will be set; and
  ;; 2) when it's called from a keyboard command, where it won't be,
  ;; and the point may or may not be on a button.  But then we can use
  ;; the row id.
  (interactive)
  (find-file
   (if marker
       (button-get (button-at marker) 'file)
     (tabulated-list-get-id))))

;; Adapted from ‘pdf-misc-display-metadata’.
(defun bibliothek--display-metadata (path)
  "Display PDF metadata for file at PATH."
  (interactive)
  (let* ((md (pdf-info-metadata path)))
    (switch-to-buffer
     (with-current-buffer (get-buffer-create "*Bibliothek Item Metadata*")
       (let* ((inhibit-read-only t)
              (pad (apply'
                    max (mapcar (lambda (d) (length (symbol-name (car d)))) md)))
              (fmt (format "%%%ds : %%s\n" pad)))
         (erase-buffer)
         (special-mode)
         (setq header-line-format path)
         (font-lock-mode 1)
         (font-lock-add-keywords
          nil '(("^ *\\(\\(?:\\w\\|-\\)+\\) :"
                 (1 font-lock-keyword-face))))
         (dolist (d md)
           (let ((key (car d))
                 (val (cdr d)))
             (cl-case key
               (keywords
                (setq val (mapconcat 'identity val ", "))))
             (let ((beg (+ (length (symbol-name key)) (point) 3))
                   (fill-prefix (make-string (+ pad 3) ?\s)))
               (insert (format fmt key val))
               (fill-region beg (point))))))
       (goto-char 1)
       (current-buffer)))
    nil))

(defun bibliothek--info ()
  "Show the metadata buffer for current row."
  (interactive)
  (bibliothek--display-metadata (tabulated-list-get-id)))



;;;; The major mode:

(define-derived-mode bibliothek-mode tabulated-list-mode
  "Bibliothek"
  "Bibliothek listing."
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (hl-line-mode))

(defvar bibliothek-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'bibliothek--find)
    (define-key map "g" #'bibliothek)
    (define-key map "i" #'bibliothek--info)
    map))

;;;###autoload
(defun bibliothek ()
  "Show the library contents.

This is the main entry point to the Bibliothek package.  It shows a
tabulated list of metadata for all the PDF files found in the
directories under ‘bibliothek-path’.

The keybindings are as follows:

\\{bibliothek-mode-map}"
  (interactive)
  (switch-to-buffer
   (with-current-buffer (get-buffer-create "*Bibliothek*")
     (let ((items (bibliothek--items))
           (f (lambda (item)
                (list (bibliothek--assoca 'bibliothek--filename item)
                      (let ((title (bibliothek--assoca 'title item))
                            (path (bibliothek--assoca 'bibliothek--filename item)))
                        (vector
                         (cons title
                               `(action bibliothek--find file ,path))
                         (bibliothek--assoca 'author item)
                         path))))))
       (setf tabulated-list-entries
             (mapcar f items)
             tabulated-list-format [("Title"  40 t)
                                    ("Author" 20 t)
                                    ("Path"   20 t)])
       (bibliothek-mode)
       (setq-local truncate-lines t)
       (current-buffer)))))



;;; Footer:
(provide 'bibliothek)
;;; bibliothek.el ends here
