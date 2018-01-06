;;; library.el --- Managing a digital library of PDFs  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Version: 0.1.0
;; Keywords: tools
;; URL: http://gkayaalp.com/emacs.html#library.el
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

;; Library.el is a management tool for a library of PDFs.  Or it
;; aspires to be so.  It's quite fresh ATM, it'll grow as it gets
;; used more.

;; Presently, library.el displays PDF files from directories in
;; ‘library-path’ in a tabulated list [1], where the user can view the
;; file related to row under point (RET on the button or ‘f’ anywhere
;; on the row by default) or bring up a buffer detailed with PDF
;; metadata of that file (bound to ‘i’ by default).  Also,
;; ‘tabulated-list-mode’ provides an interactive buffer header, where
;; by clicking the column headers, the table can be sorted.

;; Some more documentation may be present in the docstring for the
;; ‘library’ command.



;;;; Installation:

;; Library.el depends on ‘pdf-tools’.

;; After putting a copy of library.el in a directory in the
;; ‘load-path’, library.el can be configured as such:

;; (require 'library)
;; (setq library-path (list "~/Documents"))

;; Then, the Library interface can be brought up via M-x library RET.

;; [1] (see (info "(elisp) Tabulated List Mode"))

;;; Code:

(require 'pdf-info)
(require 'goto-addr)
(require 'cl-lib)
(require 'tabulated-list)
(require 'button)



;;;; Customisables:

(defgroup library
  nil
  "Customisations for library.el, digital PDF library manager."
  :group 'emacs
  :prefix "library-")

(defvar library-path nil "A list of paths to look for PDF files.")



;;;; Helper functions:

(defun library--assoca (keyseq list)
  "Arbitrary depth multi-level alist query."
  (let ((ks (if (listp keyseq) keyseq (list keyseq)))
        (ret list))
    (dolist (k ks ret)
      (setq ret (cdr (assoc k ret))))))

(defun library--items ()
  "Extract all the PDF files from each directory in ‘library-path’."
  (let (items)
    (dolist (directory library-path items)
      (dolist (file (directory-files directory t "\\.pdf$" t))
        (cl-pushnew (cons (cons 'library--filename file)
                          (pdf-info-metadata file))
                    items)))))

(defun library--find (&optional marker)
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
(defun library--display-metadata (path)
  "Display PDF metadata for file at PATH."
  (interactive)
  (let* ((md (pdf-info-metadata path)))
    (switch-to-buffer
     (with-current-buffer (get-buffer-create "*Library Item Metadata*")
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

(defun library--info ()
  "Show the metadata buffer for current row."
  (interactive)
  (library--display-metadata (tabulated-list-get-id)))



;;;; The major mode:

(define-derived-mode library-mode tabulated-list-mode
  "Library"
  "Library listing."
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (hl-line-mode))

(defvar library-mode-map (make-sparse-keymap))

(let ((map library-mode-map))
  (define-key map "f" 'library--find)
  (define-key map "g" 'library)
  (define-key map "i" 'library--info))

;;;###autoload
(defun library ()
  "Show the library contents.

This is the main entry point to the Library package.  It shows a
tabulated list of metadata for all the PDF files found in the
directories under ‘library-path’.

The keybindings are as follows:

\\{library-mode-map}"
  (interactive)
  (switch-to-buffer
   (with-current-buffer (get-buffer-create "*Library*")
     (let ((items (library--items))
           (f (lambda (item)
                (list (library--assoca 'library--filename item)
                      (let ((title (library--assoca 'title item))
                            (path (library--assoca 'library--filename item)))
                        (vector
                         (cons title
                               `(action library--find file ,path))
                         (library--assoca 'author item)
                         path))))))
       (setf tabulated-list-entries
             (mapcar f items)
             tabulated-list-format [("Title"  40 t)
                                    ("Author" 20 t)
                                    ("Path"   20 t)])
       (library-mode)
       (setq-local truncate-lines t)
       (current-buffer)))))



;;; Footer:
(provide 'library)
;;; library.el ends here
