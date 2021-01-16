;;; org-linguistics.el --- special blocks for linguistics  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: outlines, tex, wp

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

;; 

;;; Code:
(require 'cl)
(require 'ob-core)
(require 'rx)


(defvar org-linguistics-latex-packages
  (list "qtree" "lingmacros")
  "Packages ‘org-linguistics’ depends on.

These are injected into the copy buffer during the Org export
process so that using ‘org-linguistics’ does not require
modifying ‘org-latex-classes’ or
‘org-latex-default-packages-alist’.")


(defvar org-linguistics-tree-template
  "#+begin_export latex
\\enumsentence{
\\Tree %s
%s}
#+end_export"
  "Base template for syntax trees.")


(defvar org-linguistics-barlevel-template "{%s'}"
  "How to render bar levels in the tree graph.

Bar levels are generally indicated either with a macron above the
symbol or an apostrophe or a prime sign following it.  The
default is to use the apostrophe.")


(defvar org-linguistics-gloss-template
  "#+begin_export latex
\\enumsentence{
%s%s
}
#+end_export"
  "Base template for glosses.")


(defvar org-linguistics-enum-template-single-sentence
  "#+begin_export latex
\\enumsentence{
%s%s
}
#+end_export"
  "Base template for enumerated sentence block that has a single sentence.")


(defvar org-linguistics-enum-template-multiple-sentences
  "#+begin_export latex
\\eenumsentence{%s}
#+end_export"
  "Base template for enumerated sentence block that has multiple sentences.")


(defvar org-linguistics-gloss-fill-column 60
  "Number of characters above which a gloss is wrapped.")


(defun org-linguistics--tree (list elem)
  (format
   org-linguistics-tree-template
   (org-linguistics--tree--1 list)
   (if-let* ((name (org-element-property :name elem)))
       (format "\n\\label{%s}" name)
     "")))


(defun org-linguistics--tree--1 (list)
  "Subroutine of ‘org-linguistics--tree’."
  (let ((head (car list))
        (tail (cdr list)))
    (cond
     ((null list) "")
     ;; org list type, descend
     ((symbolp head)
      (mapconcat #'org-linguistics--tree--1 tail " "))
     ;; triangle
     ((and (stringp head)
           (save-excursion (string-match " :: " head)))
      (let ((parts (split-string head " :: ")))
        (apply #'format " \\qroof{ %s }.%s " (reverse parts))))
     ;; terminal node
     ((and (stringp head)
           (null tail))
      head)     ;; phrase or bar level
     (t
      (format "[ .%s %s ]"
              (if-let* ((where (save-match-data (string-match "['’]$" head))))
                  (format org-linguistics-barlevel-template (substring head 0 where))
                head)
              (mapconcat #'org-linguistics--tree--1 tail " "))))))


;; https://www.eva.mpg.de/lingua/resources/glossing-rules.php

(defun org-linguistics--gloss (list elem)
  (let* ((trim        (lambda (s) (string-trim (car s))))
         (example     (mapcar trim (cdadr (assoc "example" list #'string=))))
         (gloss       (mapcar trim (cdadr (assoc "gloss" list #'string=))))
         (translation (cadadr (assoc "translation" list #'string=)))
         line1 line2 parts ret)
    (unless example
      (user-error "Can’t have a gloss without the example"))
    (unless (or gloss translation)
      (user-error "Need to have at least one of gloss or translation"))
    (when gloss
      (unless (= (length example) (length gloss))
        (user-error "Example and gloss need to have the same number of items")))

    (while example
      (setq line1 (format "%s \\bf %s" (if line1 (concat line1 (if gloss " & " " ")) "") (pop example)))
      (when gloss (setq line2 (format "%s%s" (if line2 (concat line2 " & ") "") (pop gloss))))
      (when (or (> (length line1) org-linguistics-gloss-fill-column)
                (and line2 (> (length line2) fill-column))
                (null example))
        (setq parts (append parts (list (list line1 line2)))
              line1 nil line2 nil)))

    (format org-linguistics-gloss-template
            (dolist (p parts ret)
              (setq ret (concat ret "\\shortex" (unless translation "nt")
                                (format "{%d}" (length (split-string (car p) (if (cadr p) "&" " "))))
                                "{" (car p) "}"
                                (when (cadr p)    (concat "{" (cadr p) "}"))
                                (when translation (concat "{`" (pop translation) "'}"))
                                "\n")))
            (if-let* ((name (org-element-property :name elem)))
                (concat "\n\\label{" name "}")
              ""))))


(defun org-linguistics--enum (elem)
  (cl-case (car elem)
    ('paragraph
     (format org-linguistics-enum-template-single-sentence
             (string-trim
              (buffer-substring (org-element-property :contents-begin elem)
                                (org-element-property :contents-end elem)))
             (if-let* ((name (org-element-property :name (org-element-property :parent elem))))
                 (concat "\\label{" name "}")
               "")))
    ('plain-list
     (format org-linguistics-enum-template-multiple-sentences
             (let ((items (cdr (org-list-to-lisp)))
                   ret)
               (dotimes (i (length items) ret)
                 (setq ret (format "%s\n\\item %s %s"
                                   (or ret "")
                                   (car (nth i items))
                                   (if-let* ((name (org-element-property :name (org-element-property :parent elem))))
                                       (concat "\\label{" name (format "-%02d}" (1+ i)))
                                     "")))))))))


(defun org-linguistics--before-parsing-hook (backend)
  (cl-case backend
    ('latex
     (save-excursion
       (save-match-data
         (goto-char (point-min))
         (insert
          (mapconcat (lambda (pkg) (format "#+latex_header_extra: \\usepackage{%s}" pkg))
                     org-linguistics-latex-packages
                     "\n")
          "\n\n")
         (while (re-search-forward (rx (and bol "#+begin_" (or "tree" "gloss" "enum"))) nil t)
           (let ((elem (org-element-at-point))
                 replacement)
             (goto-char (org-element-property :contents-begin elem))
             (cl-case (intern (org-element-property :type elem))
               ('tree
                (setq replacement (org-linguistics--tree (org-list-to-lisp) elem)))
               ('gloss
                (setq replacement (org-linguistics--gloss (cdr (org-list-to-lisp)) elem)))
               ('enum
                (setq replacement (org-linguistics--enum (org-element-at-point)))))
             (delete-region (org-element-property :begin elem)
                            (1- (org-element-property :end elem)))
             (insert replacement))))))))


(add-hook 'org-export-before-parsing-hook #'org-linguistics--before-parsing-hook)



(provide 'org-linguistics)
;;; org-linguistics.el ends here
