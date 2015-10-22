;; paper-theme.el -- A minimal Emacs colour theme. -*- lexical-binding: t; -*-
;; Copyright (C) 2015 Göktuğ Kayaalp
;;
;; Author: Göktuğ Kayaalp
;; Keywords: theme paper
;; Package-Version: 0.1.0
;; Package-Requires: (("hexrgb" . "0"))
;; URL: http://gkayaalp.com/emacs.html#paper
;;
;; Permission  is  hereby  granted,  free of  charge,  to  any  person
;; obtaining  a copy  of  this software  and associated  documentation
;; files   (the  "Software"),   to  deal   in  the   Software  without
;; restriction, including without limitation  the rights to use, copy,
;; modify, merge, publish, distribute,  sublicense, and/or sell copies
;; of the  Software, and  to permit  persons to  whom the  Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The  above copyright  notice and  this permission  notice shall  be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE  IS PROVIDED  "AS IS", WITHOUT  WARRANTY OF  ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY,   FITNESS    FOR   A   PARTICULAR    PURPOSE   AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT,  TORT OR OTHERWISE, ARISING FROM, OUT  OF OR IN
;; CONNECTION WITH  THE SOFTWARE OR THE  USE OR OTHER DEALINGS  IN THE
;; SOFTWARE.
;; 
;;; Commentary:
;;
;;; Code:
;;

(require 'hexrgb)

(deftheme paper
  "An Emacs colour theme that resembles the look of paper.")

(defvar paper-colours-alist nil
  "The colours used in Paper theme.

The alist of colours where for each pair p (car p) is a
symbol identifying the colour and (cdr p) is the string, the
hexedecimal notation of the colour (i.e. #RRGGBB where R, G and B
are hexedecimal digits).")

(setq paper-colours-alist
      '((text "#070A01")
        (paper "#FAFAFA")
        (white "#EEEEEE")
        (magenta "#8C0D40")
        (pen "#000F55")
        (light-shadow "#D9DDD9")))

(defun paper-colour (colour-identifier)
  (cadr (assoc colour-identifier paper-colours-alist)))

(defconst paper-normal-face
  `((t (:foreground ,(paper-colour 'text) :background ,(paper-colour 'paper))))
  "The base colours of Paper theme.")

(defconst paper-inverse-face
  `((t (:foreground ,(paper-colour 'paper) :background ,(paper-colour 'text))))
  "The inverse of base colours of Paper theme.")

(defconst paper-pen-face
  `((t (:foreground ,(paper-colour 'pen) :background ,(paper-colour 'paper))))
  "Colour couple that resembles pen colour on paper.")

(defconst paper-light-shadow-face
  `((t (:foreground ,(paper-colour 'text) :background ,(paper-colour 'light-shadow))))
  "Colour couple that resembles a light shadow")

(defconst paper-italicised-pen-face
  `((t (:foreground ,(paper-colour 'pen) :background ,(paper-colour 'paper)
                    :slant italic)))
  "Colour couple that resembles pen colour on paper, italicised.")

(defconst paper-magenta-on-paper-face
  `((t (:foreground ,(paper-colour 'magenta) :background ,(paper-colour 'paper)))))

(defun paper-tints (hex factor n darken)
  (let ((tints))
    (reverse
     (dotimes (i n tints)
       (push (hexrgb-increment-equal-rgb
              hex 2
              (* i (if darken (- factor) factor))
              t)
             tints)))))

(eval
 (let* ((b 100)                       ; base
        (f 0.1)                       ; factor
        (tf 70)
        (o "org-level-")
        (org-faces)
        (n 8)
        (tints (paper-tints (paper-colour 'magenta) tf n nil)))
   (dolist (n (number-sequence 1 n))
     (push 
      `(quote
        (,(intern
           (concat o (number-to-string n)))
         ((t (:family
              "Sans Serif"
              :slant normal
              :weight light
              :foreground ,(pop tints)
              :height
              ,(truncate (+ b (- (* b (+ 1 f))
                                 (* b (* f n))))))))))
      org-faces))

   `(custom-theme-set-faces
     (quote paper)
     ;; === Frame ===
     (quote (default ,paper-normal-face))
     (quote (cursor ,paper-inverse-face))
     (quote (mode-line ((t (:foreground ,(paper-colour 'white)
                                        :background ,(paper-colour 'magenta)
                                        :box nil)))))
     (quote (mode-line-inactive ,paper-light-shadow-face))
     (quote (mode-line-highlight ((t (:foreground ,(paper-colour 'text)
                                                  :box nil)))))
     (quote (fringe ,paper-normal-face))
     (quote (region ((t (:background ,(paper-colour 'magenta)
                                     :foreground ,(paper-colour 'white))))))

     ;; === Syntax ===
     (quote (font-lock-builtin-face        ,paper-normal-face))
     (quote (font-lock-comment-face        ,paper-italicised-pen-face))
     (quote (font-lock-string-face         ,paper-pen-face))
     (quote (font-lock-function-name-face  ,paper-pen-face))
     (quote (font-lock-variable-name-face  ,paper-pen-face))
     (quote (font-lock-keyword-face        ,paper-magenta-on-paper-face))
     (quote (font-lock-type-face           ,paper-magenta-on-paper-face))
     (quote (font-lock-constant-face       ,paper-magenta-on-paper-face))

     ;; === Org titles ===
     (quote (org-tag ((t (:height 90 :weight light)))))
     (quote (org-hide ((t (:height 0.1 :weight light :width extracondensed)))))
     ,@org-faces)))

(require 'org)

(setq org-hide-leading-stars t)

(provide-theme 'paper)
;;; paper-theme.el ends here
