;;; gk-greek.el --- Input method for modern Greek.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: input, greek
;; Version: 1.5
;; URL: http://gkayaalp.com/emacs.html#gk-greek-el

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

;; Input  method that  converts modern  Greek latinisation  into Greek
;; alphabet.

;; 'x' is for the 'χ', 'ks' is for  the 'ξ', 'v' is for 'β'.  ';' adds
;; the  accent  mark.  Phonologically  similar  letters  are used  for
;; diphongs and  common consonant  clusters, ie  'b' writes  'μπ', 'j'
;; writes  'αι',  'B'  writes  'Μπ', and  'B;'  writes  'ΜΠ'.   Space,
;; punctuation  and '#'  after  small sigma  writes word-final  sigma,
;; i.e. 'ς'.

;;; Code:
(require 'quail)
(quail-define-package
 "greek-translit-gk" "Greek transliteration (gk)" "ιγκ" nil
 "A transliteration scheme for Greek characters.")

(quail-define-rules
 ("a"	?α)		("A"   ?Α)
 ("a;"	?ά)		("A;"  ?Ά)
 ("a#"	?α)		("A;"  ?Α)
 ("af"	["αυ"])		("Af"  ["Αυ"])	("AF"  ["ΑΥ"])
 ("af;"	["αύ"])		("Af;" ["Αύ"])	("AF;" ["ΑΎ"])
 ("af#"	["αυ"])		("Af#" ["Αυ"])	("AF#" ["ΑΥ"])
 ("av"	["αυ"])		("Av"  ["Αυ"])	("AV"  ["ΑΥ"])
 ("av;"	["αύ"])		("Av;" ["Αύ"])	("AV;" ["ΑΎ"])
 ("av#"	["αυ"])		("Av#" ["Αυ"])	("AV#" ["ΑΥ"])
 ("a#"	?α)		("A;"  ?Α)
 ("v"	?β)		("V"   ?Β)
 ("g"	?γ)		("G"   ?Γ)
 ("d"	?δ)		("D"   ?Δ)
 ("e"	?ε)		("E"   ?Ε)
 ("e;"	?έ)		("E;"  ?Έ)
 ("e#"	?ε)		("E#"  ?Ε)
 ("ef"	["ευ"])		("Ef"  ["Ευ"])	("EF"  ["ΕΥ"])
 ("ef;"	["εύ"])		("Ef;" ["Εύ"])	("EF;" ["ΕΎ"])
 ("ef#"	["ευ"])		("Ef#" ["Ευ"])	("EF#" ["ΕΥ"])
 ("ev"	["ευ"])		("Ev"  ["Ευ"])	("EV"  ["ΕΥ"])
 ("ev;"	["εύ"])		("Ev;" ["Εύ"])	("EV;" ["ΕΎ"])
 ("ev#"	["ευ"])		("Ev#" ["Ευ"])	("EV#" ["ΕΥ"])
 ("z"	?ζ)		("Z"   ?Ζ)
 ("h"	?η)		("H"   ?Η)
 ("h;"	?ή)		("H;"  ?Ή)
 ("h#"	?η)		("H#"  ?Η)
 ("th"	?θ)		("Th"  ?Θ)	("TH"  ?Θ)
 ("i"	?ι)		("I"   ?Ι)
 ("i;"	?ί)		("I;"  ?Ί)
 ("i#"	?ι)		("I#"  ?Ι)
 ("k"	?κ)		("K"   ?Κ)
 ("k#"	?κ)		("K#"  ?Κ)
 ("l"	?λ)		("L"   ?Λ)
 ("m"	?μ)		("M"   ?Μ)
 ("n"	?ν)		("N"   ?Ν)
 ("ks"	?ξ)		("Ks"  ?Ξ)	("KS"  ?Ξ)
 ("o"	?ο)		("O"   ?Ο)
 ("o;"	?ό)		("O;"  ?Ό)
 ("o#"	?ο)		("O#"  ?Ο)
 ("p"	?π)		("P"   ?Π)
 ("r"	?ρ)		("r"   ?ρ)
 ("s"	?σ)		("S"   ?Σ)
 ("s "	["ς "]) 	
 ("s."	["ς."]) 	
 ("s,"	["ς,"]) 	
 ("s;"	["ς;"]) 	
 ("s:"	["ς:"]) 	
 ("s!"	["ς!"]) 	
 ("s#"	["ς"])  	
 ("t"	?τ)		("T"	?Τ)
 ("y"	?υ)		("Y"	?Υ)
 ("y;"	?ύ)		("Y;"	?Ύ)
 ("y#"	?υ)		("Y#"	?Υ)
 ("u"   ["ου"])		("U"   ["ΟΥ"])
 ("u;"  ["ού"])		("U;"  ["ΟΎ"])
 ("u#"  ["ου"])		("U#"  ["ΟΥ"])
 ("f"	?φ)		("F"	?Φ)
 ("x"	?χ)		("X"	?Χ)
 ("ps"	?ψ)		("Ps"	?Ψ)	("PS"  ?Ψ)
 ("w"	?ω)		("W"	?Ω)
 ("w;"	?ώ)		("W;"	?Ώ)
 ("w#"	?ω)		("W#"	?Ω)
 ;; Use unused keys for some diphthongs.
 ("b"	["μπ"])	("B"	["Μπ"])	("B;"	["ΜΠ"])
 ("j"	["αι"])	("J"	["Αι"]) ("J;"	["ΑΙ"])
 ("q"	["γγ"])	("Q"	["Γγ"]) ("Q;"	["ΓΓ"])
 ("c"	["γκ"])	("C"	["Γκ"]) ("C;"	["ΓΚ"]))

;;; Footer:
(provide 'gk-greek)
;;; gk-greek.el ends here
