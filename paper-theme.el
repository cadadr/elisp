;;; paper-theme.el --- A minimal Emacs colour theme. -*- lexical-binding: t; -*-
;; Copyright (C) 2015 Göktuğ Kayaalp
;;
;; Author: Göktuğ Kayaalp
;; Keywords: theme paper
;; Package-Version: 0.2.0
;; Package-Requires: ((emacs "24") (hexrgb "0"))
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
;; Paper is a  little, minimal emacs theme that is  meant to be simple
;; and consistent.
;;
;; It  was first  intended  to resemble  the look  of  paper, but  has
;; diverged from  that objective.   Still, though,  I keep  calling it
;; Paper, as I like that name.
;;
;; Paper  uses a  small colour  palette  over all  the elements.   Org
;; headings  are  specially  treated  with a  palette  of  equidistant
;; colours.  The colours  and heading font sizes  are calculated using
;; base and factor values which can be edited.  See source.
;;
;; It's most adapted for ELisp-Org users, as I'm one such user, though
;; it  works fine  with Markdown,  Textile, Python,  JavaScript, Html,
;; Diff, Magit, etc.
;;
;;; Installation:
;;
;; Install it into a directory that's in the `custom-theme-load-path'.
;; I recommend  that you  put that directory  also in  `load-path', so
;; that you can `require' the  `paper-theme'.  Then adapt this snippet
;; to your configuration.
;;
;;   ;; Not necessary, but silences flycheck errors for referencing free
;;   ;; variables.
;;   (require 'paper-theme)
;;   ;; It's not necessary to modify these variables, they all have sane
;;   ;; defaults.
;;   (setf paper-paper-colour 'paper-parchment ; Custom background.
;;         paper-tint-factor 45)      ; Tint factor for org-level-* faces
;;   ;; Activate the theme.
;;   (load-theme 'paper t)
;;
;;; Customisation:
;;
;; It is possible to modify the  base font size and the scaling factor
;; for `org-level-faces' via  the variables `paper-base-font-size' and
;; `paper-font-factor' respectively.
;;
;; The factor  for org-level-*  colours are also  configurable, adjust
;; the variable `paper-tint-factor'.
;;
;; Various background colours  are provided, see the  docstring of the
;; variable `paper-paper-colour'  in order to  find out how  to switch
;; them.   You  can add  your  custom  colour for  background  without
;; modifying this module:
;;
;;   (push (list 'my-bgcolour "#000000") paper-colours-alist)
;;   (setf paper-paper-colour 'my-bgcolour)
;;
;; The following snippet will modify org-level-* faces so that initial
;; stars in  org headings are  hidden and  a Sans-serif font  is used.
;; Because  the combination  of heading  font sizes  and colours  make
;; levels  obvious, it  may be  considered superfluous  to have  stars
;; indicating depth:
;;
;;   (setq org-hide-leading-stars nil)
;;   (set-face-attribute
;;    'org-hide nil
;;    :height 0.1 :weight 'light :width 'extracondensed)
;;   (dolist (face org-level-faces)
;;     (set-face-attribute
;;      face nil
;;      :family "Sans Serif"))
;;
;;; Code:
;;
(require 'cl-lib)
(require 'hexrgb)

(deftheme paper
  "An Emacs colour theme that resembles the look of paper.")

(defvar paper-colours-alist
  '((text "#070A01")
    (paper-grey "#FAFAFA")
    (paper-old-dark "#F8ECC2")
    (paper-parchment "#F1F1D4")
    (paper-old-light "#F2EECB")
    (white "#EEEEEE")
    (magenta "#8C0D40")
    (pen "#000F55")
    (light-shadow "#D9DDD9"))
  "The colours used in Paper theme.
The alist of colours where for each pair p (car p) is a
symbol identifying the colour and (cdr p) is the string, the
hexedecimal notation of the colour (i.e. #RRGGBB where R, G and B
are hexedecimal digits).")

(defvar paper-paper-colour 'paper-grey
  "Which paper colour to use.
The variable `paper-colours-alist' contains a suit of colours
with prefix `paper-'.  This variable's value is supposed to be
set to one of those symbols to specify the colour used for
background.")

(defvar paper-use-varying-heights-for-org-title-headlines nil
  "Whether to use varying heights for Org headlines.")

(defvar paper-base-font-size 100
  "The base size for fonts.")

(defvar paper-font-factor 0.1
  "The font factor for calculating level fonts from base.")

(defvar paper-tint-factor 70
  "The factor for computing tints for org levels.")

(defun paper-colour (colour-identifier)
  "Get colour for COLOUR-IDENTIFIER."
  (cadr (assoc colour-identifier paper-colours-alist)))

(defun paper-colour-paper ()
  "Get the colour for paper.
See `paper-paper-colour' and `paper-colours-alist'."
  (paper-colour paper-paper-colour))

(defconst paper-normal-face
  `((t (:foreground ,(paper-colour 'text) :background ,(paper-colour-paper))))
  "The base colours of Paper theme.")

(defconst paper-inverse-face
  `((t (:foreground ,(paper-colour-paper) :background ,(paper-colour 'text))))
  "The inverse of base colours of Paper theme.")

(defconst paper-pen-face
  `((t (:foreground ,(paper-colour 'pen) :background ,(paper-colour-paper))))
  "Colour couple that resembles pen colour on paper.")

(defconst paper-light-shadow-face
  `((t (:foreground ,(paper-colour 'text) :background ,(paper-colour 'light-shadow))))
  "Colour couple that resembles a light shadow.")

(defconst paper-italicised-pen-face
  `((t (:foreground ,(paper-colour 'pen) :background ,(paper-colour-paper)
                    :slant italic)))
  "Colour couple that resembles pen colour on paper, italicised.")

(defconst paper-magenta-on-paper-face
  `((t (:foreground ,(paper-colour 'magenta) :background ,(paper-colour-paper)))))

(defun paper-tints (hex n &optional darken)
  "Compute equidistant tints of a given colour.
HEX is the hexedecimal RRGGBB string representation of the colour.
N is an integer denoting how many tints to compute.
If DARKEN is non-nil, compute darker tints, otherwise, lighter."
  (cl-loop
   for i from 0 to n
   collect (hexrgb-increment-equal-rgb
            hex 2
            (* i
               (funcall
                (if darken #'- #'identity)
                paper-tint-factor)))))

(defun paper--set-faces ()
  "Set up faces.

May be used to refresh after tweaking some variables."
  (eval
   (let* ((b paper-base-font-size)     ; base
          (f paper-font-factor)        ; factor
          (o "org-level-")
          (org-faces)
          (n 8)
          (tints (paper-tints (paper-colour 'magenta) n)))
     (dolist (n (number-sequence 1 n))
       (push
        `(quote
          (,(intern
             (concat o (number-to-string n)))
           ((t (:slant normal
                :weight light
                :foreground ,(pop tints)
                ,@(when paper-use-varying-heights-for-org-title-headlines
                    (list
                     :height
                     (truncate (+ b (- (* b (+ 1 f)) (* b (* f n))))))))))))
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
       ,(when paper-use-varying-heights-for-org-title-headlines
          (quote (quote (org-tag ((t (:height 90 :weight light)))))))
       ,@org-faces))))

(paper--set-faces)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'paper-theme)
(provide-theme 'paper)
;;; paper-theme.el ends here
