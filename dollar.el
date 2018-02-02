;;; dollar.el --- Shorthand lambda notation          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: lisp

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
;; positional arguments to the generated lambda.

;; If the car of the body is a vector though, that vector becomes the
;; argument list of the new lambda.



;;; Code:

(defun $--find-args (seq)
  (seq-sort
   (lambda (sym1 sym2)
     (< (string-to-number (substring (symbol-name sym1) 1))
        (string-to-number (substring (symbol-name sym2) 1))))
   (seq-filter
    (lambda (x)
      (and (symbolp x)
           (equal 0 (string-match "\\$[0-9]+" (symbol-name x)))))
    (seq-uniq (-flatten seq)))))

(defmacro $ (&rest body)
  "Shortcut for lambdas.

Inside this form symbols in the form $N where N is a positive
integer are to stand for positional arguments to the generated
lambda.

If the car of the BODY is a vector though, that vector becomes
the argument list of the new lambda."
  (let ((head (car body))
        (tail (cdr body))
        args the-body)
    (if (vectorp head)
        ;; Convert it to a list.
        (setf args (seq-into head 'list)
              the-body tail)
      (setf args ($--find-args body)
            the-body body))
    `(lambda ,args ,@the-body)))



;;; Footer:

(provide 'dollar)
;;; dollar.el ends here
