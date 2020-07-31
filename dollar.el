;;; dollar.el --- Shorthand lambda notation          -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019  Göktuğ Kayaalp
;; Copyright (C) 2020  Adrián Medraño Calvo

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: lisp
;; Version: 0
;; URL: https://dev.gkayaalp.com/elisp/index.html#dollar-el
;; Package-Requires: ((emacs "25"))

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

;; This package provides a macro named $ where in its body symbols in
;; the form $N where N is a positive integer are to stand for
;; positional arguments to the generated lambda.  If the symbol $_ is used, it
;; is added to the argument list as a &rest argument.  If the symbol $* is used,
;; it collects all arguments, both positional and &rest.  See the docstring of
;; `$' for details.

;; If the car of the body is a vector though, that vector becomes the
;; argument list of the new lambda.



;;; Code:
(require 'rx)
(require 'seq)

(defvar $--arg-regexp
  (rx string-start
      "$" (or (+ digit)
              "*"
              "_")
      string-end)
  "Regular expression matching arguments.")

(defun $--normalize-args (args)
  "Sort arguments ARGS."
  (seq-sort
   (lambda (sym1 sym2)
     (< (string-to-number (substring (symbol-name sym1) 1))
        (string-to-number (substring (symbol-name sym2) 1))))
   (seq-uniq args)))

(defun $--find-args (form)
  "Find arguments recursively in expression FORM.
Arguments are symbols starting with the $ character followed by a number."
  (cond
   ((and (listp form) (eq (car form) 'quote))
    nil)
   ((consp form)
    (nconc ($--find-args (car form))
           ($--find-args (cdr form))))
   ((and (symbolp form)
         (equal 0 (string-match $--arg-regexp (symbol-name form))))
    (list form))))

(defmacro $ (&optional args &rest body)
  "Shortcut for lambdas.
Inside this form symbols in the form $N where N is a positive
integer are to stand for positional arguments to the generated
lambda.

If the ARGS is a vector, that vector becomes the argument list of
the new lambda.  Otherwise ARGS is prepended to BODY.

When present within the body, $_ stands for arguments not used
within the body, i.e. the argument list is the N args used within
the body plus \"&rest $_\".

When present within the body, $* contains the entire argument
list, including what $_ matches, as a cons cell whose car is a
vector of positional arguments and whose cdr is the value of $_."
  (if (vectorp args)
      ;; Convert ARGS to a list.
      (setq args (seq-into args 'list))
    ;; ARGS is not a named argument list, but part of body.
    (push args body)
    ;; Find the arguments.
    (setq args ($--normalize-args
                ($--find-args body)))
    ;; Handle $* variable.
    (when (member '$* args)
      (setq args (delq '$* args))
      (setq body `(let (($* (cons (vector ,@args) $_)))
                    ,@body))
      ;; $* needs $_.  Add it if necessary.
      (unless (member '$_ args)
        (push '$_ args)))
    ;; Handle $_ variable.
    (when (member '$_ args)
      (setq args (nconc (delq '$_ args) '(&rest $_)))))
  (if (equal body (list nil))
      `(lambda (,@args))
    `(lambda (,@args) ,@body)))



;;; Footer:

(provide 'dollar)
;;; dollar.el ends here
