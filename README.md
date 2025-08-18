# Spanish Snowball stemmer in Emacs Lisp

This library provides a Spanish stemmer in Emacs Lisp based on the [Snowball algorithm](https://snowballstem.org/algorithms/spanish/stemmer.html), extended with:

 - [Stopword](https://snowballstem.org/algorithms/spanish/stop.txt) detection: common function words (e.g. "y", "o", "como") are returned unchanged.
 - Lemmatisation of _some_ irregular verb forms: forms of "tener", "haber", "ser", and "estar" are mapped to their infinitives.
 - A test suite (`ert`) for regression testing.

Example:
```elisp
   (snowball-spanish-stem "tienes")   ;; => "tener"
   (snowball-spanish-stem "torneos")  ;; => "torne"
   (snowball-spanish-stem "como")     ;; => "como"
   (snowball-spanish-stem-tokens '("camiones" "naciones" "lógico"))
   ;; => '("camion" "nacion" "logic")
```

To run the tests:

  `M-x ert RET snowball-spanish-test RET`

Acknowledgment: I heavily used the free version of ChatGPT (which can be GPT-5 and older models) to port the Snowball algorithm in Emacs Lisp. The License of this code follows [that of Snowball's](https://snowballstem.org/license.html).
