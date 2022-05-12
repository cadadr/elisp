;;; gk-unilat.el -- Unified input method for variants of the Latin alphabet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018, 2019, 2020, 2021, 2022 Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: input
;; Version: 1.6
;; URL: https://github.com/cadadr/elisp/#gk-unilatel

;; Permission  is  hereby  granted,  free of  charge,  to  any  person
;; obtaining  a copy  of  this software  and associated  documentation
;; files   (the  "Software"),   to  deal   in  the   Software  without
;; restriction, including without limitation  the rights to use, copy,
;; modify, merge, publish, distribute,  sublicense, and/or sell copies
;; of the  Software, and  to permit  persons to  whom the  Software is
;; furnished to do so, subject to the following conditions:

;; The  above copyright  notice and  this permission  notice shall  be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE  IS PROVIDED  "AS IS", WITHOUT  WARRANTY OF  ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY,   FITNESS    FOR   A   PARTICULAR    PURPOSE   AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT,  TORT OR OTHERWISE, ARISING FROM, OUT  OF OR IN
;; CONNECTION WITH  THE SOFTWARE OR THE  USE OR OTHER DEALINGS  IN THE
;; SOFTWARE.


;;; Commentary:

;; This is the Universal Latin Emacs Input Method.

;; It  aims to  provide  comprehensive support  for typing  characters
;; found in  different European versions  of the Latin alphabet,  in a
;; unified, predictable way.

;; See [ C-h I unilat-gk ] for a comprehensive listing of keymaps, but
;; also the definitions below as they are logically grouped.

;; See  `gk-unilat-languages' for  a list  of intentionally  supported
;; languages.

;; Contributions are welcome, please follow the instructions at
;; https://github.com/cadadr/elisp/#contributing



;;; Code:
(require 'seq)
(require 'quail)

;; Keep this sorted alphabetically
(defconst gk-unilat-languages
  (list "Dutch" "English" "French" "German" "Italian" "Kurdish"
        "Latin" "Norwegian" "Portuguese" "Spanish" "Swedish"
        "Turkish" "UTF-8" "Welsh")
  "List of languages that  `unilat-gk' input-method supports.")



;;; Unified latin input method:
(quail-define-package
 "unilat-gk" "Universal Latin (GK)" "☉" nil
 "A universal Latin input method.
Defines key combos for inputting languages listed in
‘gk-unilat-languages’.")

(defvar gk-unilat--mappings
  '(
    ;; Turkish I and umlauts, cedillas circumflecis
    ("i;" ?ı) ("o;" ?ö) ("u;" ?ü) ("c;" ?ç) ("g;" ?ğ) ("s;" ?ş)

    ("I;" ?İ) ("O;" ?Ö) ("U;" ?Ü) ("C;" ?Ç) ("G;" ?Ğ) ("S;" ?Ş) ("A^" ?Â)
    ("E^" ?Ê) ("U^" ?Û) ("I^" ?Î) ("O^" ?Ô)

    ("a^" ?â) ("e^" ?ê) ("i^" ?î) ("u^" ?û) ("o^" ?ô)

    ;; Acute and breve
    ("a\\" ?à) ("e\\" ?è) ("i\\" ?ì) ("o\\" ?ò) ("u\\" ?ù)

    ("A\\" ?À) ("E\\" ?È) ("I\\" ?Ì) ("O\\" ?Ò) ("U\\" ?Ù)

    ("a/" ?á) ("e/" ?é) ("i/" ?í) ("o/" ?ó) ("u/" ?ú)

    ("A/" ?Á) ("E/" ?É) ("I/" ?Í) ("O/" ?Ó) ("U/" ?Ú)

    ;; Macron
    ("a_" ?ā) ("e_" ?ē) ("i_" ?ī) ("o_" ?ō) ("u_" ?ū)

    ("A_" ?Ā) ("E_" ?Ē) ("I_" ?Ī) ("O_" ?Ō) ("U_" ?Ū)

    ;; Tilde
    ("a~" ?ã) ("e~" ?ẽ) ("i~" ?ĩ) ("o~" ?õ) ("u~" ?ũ) ("n~" ?ñ)

    ("A~" ?Ã) ("E~" ?Ẽ) ("I~" ?Ĩ) ("O~" ?Õ) ("U~" ?Ũ) ("N~" ?Ñ)

    ;; Ordinal
    ("a&" ?ª) ("o&" ?º)

    ;; Various
    ("I:" ?Ï) ("i:" ?ï) ("a:" ?ä) ("A:" ?Ä) ("e:" ?ë) ("E:" ?Ë) ("a0" ?å)
    ("A0" ?Å) ("o$" ?ø) ("O$" ?Ø) ("sZ" ?ß) ("o£" ?œ) ("O£" ?Œ) ("a£" ?æ)
    ("A£" ?Æ)))


(defvar gk-unilat--undo-mappings
  (mapcar
   (lambda (m)
     (list (concat (car m) "#") (car (string-to-list (car m)))))
   gk-unilat--mappings))


(defvar gk-unilat--escape-mappings
  (mapcar
   (lambda (letter)
     (list (format "%c#" letter) letter))
   (seq-uniq
    (mapcar
     (lambda (m) (aref (car m) 0))
     gk-unilat--mappings))))


(eval
 `(quail-define-rules
   ,@gk-unilat--mappings

   ,@gk-unilat--undo-mappings

   ,@gk-unilat--escape-mappings))



;;; Footer:
(provide 'gk-unilat)
;;; gk-unilat.el ends here
