;;; gk-unilat.el -- Unified input method for variants of the Latin alphabet.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018, 2019, 2020, 2021, 2022 Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: input
;; Version: 2.0
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


(defvar gk-unilat-escape-char ?x
  "Character to escape an active composition.

When this character is entered, the current visible letter is
inserted and the sequence is no longer available for Quail
modifications.")


(defvar gk-unilat-undo-char ?u
  "Character to undo a composition.

When a composition is active and this character is entered, the
most recent modification is undone.")




;;; Unified latin input method:
(quail-define-package
 "unilat-gk" "Universal Latin (GK)" "☉" nil
 "A universal Latin input method.

The Unilat-GK input method is meant to help with entering text in
most, and ideally all, Latin-based scripts.

The 26 Latin letters insert themselves. For those that can
receive diacritics, Unilat-GK presents mnemonic key combinations
to insert them.

Some example conversions:
    C;ekoslovakyali;lasti;ramadi;klari;mi;zdan mi;si;ni;z?
        => Çekoslovakyalılaştıramadıklarımızdan mısınız?
    Na ve/spera de na~o partir nunca
        => Na véspera de não partir nunca
    Tu bi Kurdi^ diaxivi^?
        => Tu bi kurdî diaxivî?
    L'histoire se re/pe\te.
        => L'histoire se répète.
    I no$d skal du la£re dine venner a0 kenne.
        => I nød skal du lære dine venner å kenne.

There are cases where this can get complicated, for example when
typing a slash between two words or a colon after a vowel, in
which case just typing ahead when Unilat-GK is active will
produce a letter with a diacritic instead of the intended
sequence.

To remedy this, there are two mechanisms: escaping, and undoing.

The escape key, determined by ‘gk-unilat-escape-char’, terminates
the active letter’s composition, inserting what you see on the
screen into the text without any further changes or undoing. For
example, assuming default settings are active, observe:

    here/there => heréthere    vs.    herex/there => here/there
    see:       => seë          vs.    seex:       => see:

The undo key, determined by ‘gk-unilat-undo-char’, instead,
undoes the last modification to the current letter and inserts
the letter without that modification into the buffer. Observe:

    here/there => heréthere    vs.    here/u/there => here/there
    aks;am     => akşam        vs.    aks;uam      => aksam

The languages Unilat-GK supports are hard to pin down, as many
world languages use some variation of the Latin alphabet and I
only have familiarity with the orthography of a subset of them.

Thus rather than listing languages it supports, I resort to
enumerating those languages which I intentionally target. See
‘gk-unilat-languages’ for a list of these.

‘gk-unilat-languages’, with possible modifications, can be used
to assign Unilat-GK as the default input method for supported
languages with an Elisp snippet as follows:

    ;; Use ‘unilat-gk’ whenever possible.
    \(dolist (lang gk-unilat-languages)
      (let* ((env (assoc lang language-info-alist))
             (im (assoc 'input-method env)))
        ;; Some language environments may  not have an input-method
        ;; field, namely English.
        (when im
          (setcdr im \"unilat-gk\")))) ")

(defvar gk-unilat--mappings
  '(
    ;; Turkish I and umlauts, cedillas circumflecis
    ("i;" ?ı) ("i;;" ?i) ("o;" ?ö) ("u;" ?ü) ("c;" ?ç) ("g;" ?ğ) ("s;" ?ş)

    ("I;" ?İ) ("I;;" ?I) ("O;" ?Ö) ("U;" ?Ü) ("C;" ?Ç) ("G;" ?Ğ) ("S;" ?Ş)

    ("A^" ?Â) ("E^" ?Ê) ("U^" ?Û) ("I^" ?Î) ("O^" ?Ô)

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
    ("A£" ?Æ)

    ;; Inclusive language - Italian
    ("x;" ?ə) ("X;" ?Ə) ("q;" ?з) ("Q;" ?З)))


(defvar gk-unilat--undo-mappings
  (mapcar
   (lambda (m)
     (list (concat (car m) (make-string 1 gk-unilat-undo-char))
           (car (string-to-list (car m)))))
   gk-unilat--mappings))


(defvar gk-unilat--escape-mappings
  (append
   (list (list (format "i;%c" gk-unilat-escape-char) ?ı)
         (list (format "I;%c" gk-unilat-escape-char) ?İ))
   (mapcar
    (lambda (letter)
      (list (format "%c%c" letter gk-unilat-escape-char) letter))
    (seq-uniq
     (mapcar
      (lambda (m) (aref (car m) 0))
      gk-unilat--mappings)))))


(eval
 `(quail-define-rules
   ,@gk-unilat--mappings

   ,@gk-unilat--undo-mappings

   ,@gk-unilat--escape-mappings))



;;; Footer:
(provide 'gk-unilat)
;;; gk-unilat.el ends here
