(defvar iipa-synonyms
  '((stop        . plosive)
    (flap        . tap)
    (interdental . dental)
    (high        . close)
    (low         . open)
    (near-high   . near-close)
    (near-low    . near-open)
    (near-front  . front)
    (near-back   . back)
    (high-mid    . close-mid)
    (low-mid     . open-mid)))

;; https://westonruter.github.io/ipa-chart/keyboard/
(defvar iipa-symbols
  ;; TODO(2020-05-08): ɧ, suprasegmentals, affricate diacritics,
  ;; ejectives, diacritics, tones/accents
  '((stop
     ("p" voiceless bilabial)
     ("b" voiced    bilabial)
     ("t" voiceless alveolar)
     ("ʈ" voiceless retroflex)
     ("ɖ" voiced    retroflex)
     ("c" voiceless palatal)
     ("ɟ" voiced    palatal)
     ("k" voiceless velar)
     ("g" voiced    velar)
     ("q" voiceless uvular)
     ("ɢ" voiced    uvular)
     ("ʔ" voiceless glottal)
     ("ʡ" epiglottal))

    (nasal
     ("m" voiced    bilabial)
     ("ɱ" voiced    labiodental)
     ("n" voiced    alveolar)
     ("ɳ" voiced    retroflex)
     ("ŋ" voiced    velar)
     ("ɴ" voiced    uvular))

    (trill
     ("ʙ" voiced    bilabial)
     ("r" voiced    alveolar)
     ("ʀ" voiced    uvular))

    (flap
     ("ⱱ" voiced    labiodental)
     ("ɾ" voiced    alveolar)
     ("ɽ" voiced    retroflex)
     ("ɺ" alveolar-lateral))

    (fricative
     ("ɸ" voiceless bilabial)
     ("β" voiced    bilabial)
     ("f" voiceless labiodental)
     ("v" voiced    labiodental)
     ("θ" voiceless interdental)
     ("ð" voiced    interdental)
     ("s" voiceless alveolar)
     ("z" voiced    alveolar)
     ("ʃ" voiceless postalveolar)
     ("ʒ" voiced    postalveolar)
     ("ʂ" voiceless retroflex)
     ("ʐ" voiced    retroflex)
     ("ç" voiceless palatal)
     ("ʝ" voiced    palatal)
     ("x" voiceless velar)
     ("ɣ" voiced    velar)
     ("χ" voiceless uvular)
     ("ʁ" voiced    uvular)
     ("ħ" voiceless pharyngeal)
     ("ʕ" voiced    pharyngeal)
     ("h" voiceless glottal)
     ("ɦ" voiced    glottal)
     ("ʍ" voiceless labial-velar)
     ("ʜ" voiceless epiglottal)
     ("ʢ" voiced    epiglottal)
     ("ɕ" voiceless alveolo-palatal)
     ("ʑ" voiced    alveolo-palatal))

    (lateral-fricative
     ("ɬ" voiceless alveolar)
     ("ɮ" voiced    alveolar))

    (approximant
     ("ʋ" voiceless labiodental)
     ("ɹ" voiceless alveolar)
     ("ɻ" voiceless retroflex)
     ("j" voiceless palatal)
     ("ɰ" voiceless velar)
     ("w" voiced    labial-velar)
     ("ɥ" voiced    labial-palatal))

    (lateral-approximant
     ("l" voiceless alveolar)
     ("ɭ" voiceless retroflex)
     ("ʎ" voiceless palatal)
     ("ʟ" voiceless velar))

    (click
     ("ʘ" bilabial)
     ("ǀ" dental)
     ("ǃ" alveolar)
     ("ǃ" postalveolar)                 ;duplication intentional
     ("ǂ" palatoalveolar)
     ("ǁ" alveolar-lateral))

    (implosive
     ("ɓ" voiced bilabial)
     ("ɗ" voiced dental)
     ("ɗ" voiced alveolar)              ;duplication intentional
     ("ʄ" voiced palatal)
     ("ɠ" voiced velar)
     ("ʛ" voiced uvular)

     (vowel
      ("i" high      front   unrounded)
      ("y" high      front   rounded)
      ("ɨ" high      central unrounded)
      ("ʉ" high      central rounded)
      ("ɯ" high      back    unrounded)
      ("u" high      back    unrounded)

      ("ɪ" near-high front   unrounded)
      ("ʏ" near-high front   rounded)
      ("ʊ" near-high back    rounded)

      ("e" high-mid  front   unrounded)
      ("ø" high-mid  front   rounded)
      ("ɘ" high-mid  central unrounded)
      ("ɵ" high-mid  central rounded)
      ("ɤ" high-mid  back    unrounded)
      ("o" high-mid  back    rounded)

      ("ø̞" mid       front   rounded)
      ("ə" mid       central schwa)
      ("o̞" mid       back    rounded)

      ("ɛ" low-mid   front   unrounded)
      ("œ" low-mid   front   rounded)
      ("ɜ" low-mid   central unrounded)
      ("ɞ" low-mid   central rounded)
      ("ʌ" low-mid   back    unrounded)
      ("ɔ" low-mid   back    rounded)

      ("æ" near-low  front   unrounded)
      ("ɐ" near-low  central)

      ("a" low       front   unrounded)
      ("ɶ" low       front   rounded)
      ("ä" low       central unrounded)
      ("ɑ" low       back    unrounded)
      ("ɒ" low       back    rounded))

     (affricate
      ("t͡s" voiceless alveolar)
      ("d͡z" voiced    alveolar)
      ("t͡ʃ" voiceless palatoalveolar)
      ("t͡ɕ" voiceless alveolopalatal)
      ("d͡ʑ" voiced    alveolopalatal)
      ("ʈ͡ʂ" voiceless retroflex)
      ("ɖ͡ʐ" voiced    retroflex)
      ("d͡ʒ" voiced    postalveolar)))))

(defun iipa-type ())
