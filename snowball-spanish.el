;;; snowball-spanish.el --- Spanish Snowball stemmer -*- lexical-binding: t; -*-
;; Author: Yu Huo <yhuo@tuta.io>
;; URL: https://github.com/niwaka-ame/snowball-spanish.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, nlp, spanish, stemming

;;; Commentary:

;; This library provides a Spanish stemmer based on the Snowball
;; algorithm, extended with:
;;
;;  - Stopword detection: common function words (e.g. "y", "o", "como")
;;    are returned unchanged.
;;  - Lemmatisation of some irregular verb forms: forms of "tener", "haber",
;;    "ser", and "estar" are mapped to their infinitives.
;;  - A test suite (`ert`) for regression testing.
;;
;; Example:
;;
;;   (snowball-spanish-stem "tienes")   ;; => "tener"
;;   (snowball-spanish-stem "torneos")  ;; => "torne"
;;   (snowball-spanish-stem "como")     ;; => "como"
;;   (snowball-spanish-stem-tokens '("camiones" "naciones" "lógico"))
;;   ;; => '("camion" "nacion" "logic")
;;
;; To run the tests:
;;
;;   M-x ert RET snowball-spanish-test RET
;;
;; Acknowledgment: I heavily used the free version of ChatGPT (which can be
;; GPT-5 and older models) to port the Snowball algorithm in Emacs Lisp. The
;; License of this code follows that of Snowball's.

;;; Code:
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

;; --- Utilities --------------------------------------------------------------

(defconst snowball-spanish--vowels
  "aeiouáéíóúü"
  "Spanish vowels for the Snowball algorithm.")

(defconst snowball-spanish--stopwords
  (let ((tbl (make-hash-table :test #'equal)))
    (mapc (lambda (w) (puthash w t tbl))
          '("de" "la" "que" "el" "en" "y" "a" "los" "del" "se"
            "las" "por" "un" "para" "con" "no" "una" "su" "al"
            "lo" "como" "más" "pero" "sus" "le" "ya" "o"
            "este" "sí" "porque" "esta" "entre" "cuando" "muy"
            "sin" "sobre" "también" "me" "hasta" "hay" "donde"
            "quien" "desde" "todo" "nos" "durante" "todos"
            "uno" "les" "ni" "contra" "otros" "ese" "eso"
            "ante" "ellos" "e" "esto" "mí" "antes" "algunos"
            "qué" "unos" "yo" "otro" "otras" "otra" "él"
            "tanto" "esa" "estos" "mucho" "quienes" "nada"
            "muchos" "cual" "poco" "ella" "estas" "algunas"
            "algo" "nosotros" "mi" "mis" "tú" "te" "ti" "tu"
            "tus" "ellas" "nosotras" "vosotros" "vosotras" "os"
            "mío" "mía" "míos" "mías" "tuyo" "tuya" "tuyos"
            "tuyas" "suyo" "suya" "suyos" "suyas" "nuestro"
            "nuestra" "nuestros" "nuestras" "vuestro" "vuestra"
            "vuestros" "vuestras" "esos" "esas"))
    tbl)
  "Hash table of Spanish stopwords that should remain unchanged by stemming.")

;; Helpers to create lemma-map blocks
(defun snowball-spanish--split-words (s)
  (split-string s "[ \t\r\n]+" t))

(defconst snowball-spanish--estar-forms
  (snowball-spanish--split-words
   "estoy estás está estamos estáis están esté estés estemos estéis estén
    estaré estarás estará estaremos estaréis estarán estaría estarías estaríamos
    estaríais estarían estaba estabas estábamos estabais estaban estuve estuviste
    estuvo estuvimos estuvisteis estuvieron estuviera estuvieras estuviéramos
    estuvierais estuvieran estuviese estuvieses estuviésemos estuvieseis
    estuviesen estando estado estada estados estadas estad"))

(defconst snowball-spanish--haber-forms
  (snowball-spanish--split-words
   "he has ha hemos habéis han haya hayas hayamos hayáis hayan
    habré habrás habrá habremos habréis habrán habría habrías habríamos
    habríais habrían había habías habíamos habíais habían hube hubiste
    hubo hubimos hubisteis hubieron hubiera hubieras hubiéramos hubierais
    hubieran hubiese hubieses hubiésemos hubieseis hubiesen habiendo
    habido habida habidos habidas"))

(defconst snowball-spanish--ser-forms
  (snowball-spanish--split-words
   "soy eres es somos sois son sea seas seamos seáis sean
    seré serás será seremos seréis serán sería serías seríamos
    seríais serían era eras éramos erais eran fui fuiste fue
    fuimos fuisteis fueron fuera fueras fuéramos fuerais fueran
    fuese fueses fuésemos fueseis fuesen siendo sido sed"))

(defconst snowball-spanish--tener-forms
  (snowball-spanish--split-words
   "tengo tienes tiene tenemos tenéis tienen tenga tengas tengamos tengáis
    tengan tendré tendrás tendrá tendremos tendréis tendrán tendría
    tendrías tendríamos tendríais tendrían tenía tenías teníamos teníais
    tenían tuve tuviste tuvo tuvimos tuvisteis tuvieron tuviera tuvieras
    tuviéramos tuvierais tuvieran tuviese tuvieses tuviésemos tuvieseis
    tuviesen teniendo tenido tenida tenidos tenidas tened"))

(defconst snowball-spanish--lemma-map
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (w snowball-spanish--estar-forms) (puthash (downcase w) "estar" ht))
    (dolist (w snowball-spanish--haber-forms) (puthash (downcase w) "haber" ht))
    (dolist (w snowball-spanish--ser-forms)   (puthash (downcase w) "ser"   ht))
    (dolist (w snowball-spanish--tener-forms) (puthash (downcase w) "tener" ht))
    ht)
  "Map of conjugated forms to infinitive for estar/haber/ser/tener.")

(defun snowball-spanish--vowel-p (ch)
  "Return non-nil if CH (a character) is a Spanish vowel per Snowball."
  (when ch (cl-find (downcase ch) snowball-spanish--vowels :test #'char-equal)))

(defun snowball-spanish--string-suffix-p (suffix s)
  "Return start index of SUFFIX in S if S ends with SUFFIX; otherwise nil.
Like `string-suffix-p' but returns the start index for convenience."
  (let* ((len (length s))
         (slen (length suffix)))
    (when (and (>= len slen)
               (string= suffix (substring s (- len slen) len)))
      (- len slen))))

(defun snowball-spanish--region-index (word from-pred until-pred &optional start)
  "Internal helper to compute R1/R2-style regions.
Scan WORD from START (default 0) to find first index where FROM-PRED holds,
then continue until UNTIL-PRED holds; return index _after_ that point or (length WORD) if none."
  (let* ((n (length word))
         (i (or start 0)))
    (while (and (< i n) (not (funcall from-pred (aref word i))))
      (setq i (1+ i)))
    (while (and (< i n) (funcall from-pred (aref word i)))
      (setq i (1+ i)))
    (if (< i n) i n)))

(defun snowball-spanish--compute-R1-R2 (w)
  "Return (values r1 r2) region start indices for WORD per 'usual' definition."
  ;; R1: region after first non-vowel following a vowel
  (let* ((n (length w))
         (i 0))
    (while (and (< i n) (not (snowball-spanish--vowel-p (aref w i)))) (setq i (1+ i)))
    (while (and (< i n) (snowball-spanish--vowel-p (aref w i))) (setq i (1+ i)))
    (let ((r1 (if (< i n) i n)))
      ;; R2 is R1 applied again starting from r1
      (while (and (< i n) (not (snowball-spanish--vowel-p (aref w i)))) (setq i (1+ i)))
      (while (and (< i n) (snowball-spanish--vowel-p (aref w i))) (setq i (1+ i)))
      (let ((r2 (if (< i n) i n)))
        (cons r1 r2)))))

(defun snowball-spanish--compute-RV (w)
  "Compute RV start index for WORD per Spanish Snowball definition."
  (let* ((n (length w)))
    (cond
     ((<= n 2) n)
     (t
      (let ((c0 (aref w 0)) (c1 (aref w 1)))
        (cond
         ;; If the second letter is a consonant, RV = region after the next vowel
         ((and (not (snowball-spanish--vowel-p c1)))
          (let ((i 2))
            (while (and (< i n) (not (snowball-spanish--vowel-p (aref w i))))
              (setq i (1+ i)))
            (if (< i n) (1+ i) n)))
         ;; If the first two letters are vowels, RV = region after the next consonant
         ((and (snowball-spanish--vowel-p c0) (snowball-spanish--vowel-p c1))
          (let ((i 2))
            (while (and (< i n) (snowball-spanish--vowel-p (aref w i)))
              (setq i (1+ i)))
            (if (< i n) (1+ i) n)))
         ;; Otherwise (consonant-vowel), RV starts after the third letter
         (t 3)))))))

(defun snowball-spanish--in-region-p (idx region-start)
  "Return non-nil if index IDX lies in the region starting at REGION-START."
  (and idx (>= idx region-start)))

(defun snowball-spanish--remove-acute-accents (w)
  "Map áéíóú to aeiou in W. (ü and ñ are not altered by Snowball postlude.)"
  (replace-regexp-in-string
   "[áéíóú]" (lambda (m)
               (cl-case (aref m 0)
                 (?á "a") (?é "e") (?í "i") (?ó "o") (?ú "u")))
   w))

;; Small helpers for transformation
(defun snowball-spanish--maybe-drop-preceding-u-after-g (w start-of-suffix rv-start &optional must-u-in-rv)
  "If W before START-OF-SUFFIX ends with 'gu', drop the 'u'.
If MUST-U-IN-RV non-nil, only drop when the 'u' index is >= RV-START."
  (let ((u-idx (- start-of-suffix 1))
        (g-idx (- start-of-suffix 2)))
    (if (and (>= g-idx 0)
             (char-equal (aref w g-idx) ?g)
             (char-equal (aref w u-idx) ?u)
             (or (not must-u-in-rv) (>= u-idx rv-start)))
        (concat (substring w 0 u-idx) (substring w (1+ u-idx)))
      w)))

;; --- Step 0: attached pronoun ------------------------------------------------

(defconst snowball-spanish--pronouns
  '("selas" "selos" "sela" "selo"  ; longest first
    "las" "les" "los"
    "me" "se" "la" "le" "lo" "nos")
  "Pronoun endings considered in Step 0 (longest match first).")

(defconst snowball-spanish--step0-a-bases
  '(("iéndo" . "iendo")
    ("ándo"  . "ando")
    ("ár"    . "ar")
    ("ér"    . "er")
    ("ír"    . "ir"))
  "Accented bases for Step 0(a) and their unaccented replacements.")

(defconst snowball-spanish--step0-b-bases
  '("ando" "iendo" "ar" "er" "ir")
  "Unaccented bases for Step 0(b).")

(defun snowball-spanish--step0-attached-pronoun (w rv)
  "Implement Step 0 for WORD W given RV start RV.
Returns possibly modified word."
  (let ((res w))
    (catch 'done
      (dolist (p snowball-spanish--pronouns)
        (let ((pstart (snowball-spanish--string-suffix-p p res)))
          (when (and pstart (>= pstart rv))
            (let* ((head (substring res 0 pstart)))
              ;; Case (a): accented bases -> replace base (remove accent)
              (let ((hit-a (cl-find-if
                            (lambda (pair)
                              (let* ((base (car pair))
                                     (bstart (snowball-spanish--string-suffix-p base head)))
                                bstart))
                            snowball-spanish--step0-a-bases)))
                (when hit-a
                  (let* ((base (car hit-a))
                         (repl (cdr hit-a))
                         (bstart (snowball-spanish--string-suffix-p base head)))
                    (setq res (concat (substring head 0 bstart) repl))
                    (throw 'done res))))
              ;; Case (b): plain bases
              (when (cl-some (lambda (base) (snowball-spanish--string-suffix-p base head))
                             snowball-spanish--step0-b-bases)
                (setq res head)
                (throw 'done res))
              ;; Case (c): 'yendo' with preceding 'u' (u may be outside RV)
              (let ((ypos (snowball-spanish--string-suffix-p "yendo" head)))
                (when ypos
                  (let ((u-idx (1- ypos)))
                    (when (and (>= u-idx 0) (char-equal (aref head u-idx) ?u))
                      ;; delete the 'u', then drop pronoun
                      (setq head (concat (substring head 0 u-idx) (substring head (1+ u-idx)))))
                    (setq res head)
                    (throw 'done res)))))))))
    res))

;; --- Step 1: standard suffix removal ----------------------------------------

;; Suffix groups from the Snowball algorithm (longest-first where needed).
(defconst snowball-spanish--s1-del-r2
  '("amientos" "imiento" "imientos" "amiento"
    "anzas" "anza"
    "icos" "icas" "ico" "ica"
    "ismos" "ismo"
    "ables" "able"
    "ibles" "ible"
    "istas" "ista"
    "osos" "osas" "oso" "osa"))

(defconst snowball-spanish--s1-block2     ; delete in R2; if preceded by "ic", delete that too in R2
  '("adoras" "adores" "adora" "ador"
    "aciones" "ación" "ante" "antes" "ancia" "ancias"
    "acion"))                  ; misspelling accepted by Snowball

(defconst snowball-spanish--s1-logia->log '("logías" "logía"))
(defconst snowball-spanish--s1-ucion->u   '("ucciones" "ucción" "ucion"))
(defconst snowball-spanish--s1-encia->ente '("encias" "encia"))

(defconst snowball-spanish--s1-amente '("amente"))
(defconst snowball-spanish--s1-mente  '("mente"))
(defconst snowball-spanish--s1-idad   '("idades" "idad"))
(defconst snowball-spanish--s1-iva    '("ivas" "ivos" "iva" "ivo"))

(defun snowball-spanish--delete-if-in (w suffixes r-start)
  "Delete the longest SUFFIX in SUFFIXES from W if its start is >= R-START.
Return (cons changedp newword)."
  (let ((hit nil) (pos nil))
    (dolist (s suffixes)
      (let ((p (snowball-spanish--string-suffix-p s w)))
        (when (and p (>= p (or r-start 0)))
          (when (or (null hit) (> (length s) (length hit)))
            (setq hit s pos p)))))
    (if hit
        (cons t (concat (substring w 0 pos) (substring w (+ pos (length hit)))))
      (cons nil w))))

(defun snowball-spanish--step1 (w r1 r2 rv)
  "Spanish Step 1 of the Snowball algorithm."
  (let ((word w))
    (catch 'done
      ;; --- delete certain endings if in R2
      (dolist (s snowball-spanish--s1-del-r2)
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p r2))
            (setq word (substring word 0 p))
            (throw 'done word))))

      ;; --- block 2 endings like icadora/icador/icante
      (dolist (s snowball-spanish--s1-block2)
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p r2))
            (setq word (substring word 0 p))
            ;; if ends with "ic" in R2, delete
            (let ((icpos (snowball-spanish--string-suffix-p "ic" word)))
              (when (and icpos (>= icpos r2))
                (setq word (substring word 0 icpos))))
            (throw 'done word))))

      ;; --- logía → log
      (dolist (s snowball-spanish--s1-logia->log)
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p r2))
            (setq word (concat (substring word 0 p) "log"))
            (throw 'done word))))

      ;; --- ución → u
      (dolist (s snowball-spanish--s1-ucion->u)
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p r2))
            (setq word (concat (substring word 0 p) "u"))
            (throw 'done word))))

      ;; --- encia → ente
      (dolist (s snowball-spanish--s1-encia->ente)
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p r2))
            (setq word (concat (substring word 0 p) "ente"))
            (throw 'done word))))

      ;; --- -amente
      (let ((p (snowball-spanish--string-suffix-p "amente" word)))
        (when (and p (>= p r1))
          (setq word (substring word 0 p))
          ;; handle iv →, at → deletions
          (let ((ivp (snowball-spanish--string-suffix-p "iv" word)))
            (when (and ivp (>= ivp r2))
              (setq word (substring word 0 ivp))))
          (let ((atp (snowball-spanish--string-suffix-p "at" word)))
            (when (and atp (>= atp r2))
              (setq word (substring word 0 atp))))
          (throw 'done word)))

      ;; --- -mente
      (let ((p (snowball-spanish--string-suffix-p "mente" word)))
        (when (and p (>= p r2))
          (setq word (substring word 0 p))
          (throw 'done word)))

      ;; --- idad / idades
      (let ((s (or (and (snowball-spanish--string-suffix-p "idades" word) "idades")
                   (and (snowball-spanish--string-suffix-p "idad" word) "idad"))))
        (when s
          (let ((p (snowball-spanish--string-suffix-p s word)))
            (when (>= p r2)
              (setq word (substring word 0 p))
              (throw 'done word)))))

      ;; --- iva/ivo/ivas/ivos
      (dolist (s snowball-spanish--s1-iva)
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p r2))
            (setq word (substring word 0 p))
            ;; if ends with "at" in R2, delete it
            (let ((atp (snowball-spanish--string-suffix-p "at" word)))
              (when (and atp (>= atp r2))
                (setq word (substring word 0 atp))))
            (throw 'done word)))))

    word))



;; --- Step 2a & 2b: verb suffixes --------------------------------------------

(defconst snowball-spanish--step2a-y-sufs
  '("yeron" "yendo" "yamos" "yais"
    "yan" "yen" "yas" "yes" "yó" "yo" "ye" "ya"))

(defconst snowball-spanish--step2b-short
  '("en" "es" "éis" "emos"))

(defconst snowball-spanish--step2b-long
  '("arían" "arías" "arán" "arás" "aríais" "aría" "aréis" "aríamos" "aremos" "ará" "aré"
    "erían" "erías" "erán" "erás" "eríais" "ería" "eréis" "eríamos" "eremos" "erá" "eré"
    "irían" "irías" "irán" "irás" "iríais" "iría" "iréis" "iríamos" "iremos" "irá" "iré"
    "ábamos" "áramos" "ásemos" "iéramos" "iésemos" ; keep longer forms early
    "aba" "ada" "ida" "ía" "ara" "iera" "ad" "ed" "id" "ase" "iese"
    "aste" "iste" "an" "aban" "ían" "aran" "ieran" "asen" "iesen" "aron" "ieron"
    "ado" "ido" "ando" "iendo" "ió" "ar" "er" "ir" "as" "abas" "adas" "idas" "ías"
    "aras" "ieras" "ases" "ieses" "ís" "áis" "abais" "íais" "arais" "ierais"
    "aseis" "ieseis" "asteis" "isteis" "ados" "idos" "amos" "íamos" "imos"))
(defun snowball-spanish--step2 (w rv)
  "Spanish Step 2 of the Snowball algorithm."
  (let ((word w))
    ;; --- Step 2a: delete suffix yendo if in RV and preceded by u after gu
    (catch 'done
      (dolist (s snowball-spanish--step2a-y-sufs)
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p rv))
            ;; special gu/u handling
            (let ((u-idx (1- p)))
              (when (and (>= u-idx 0)
                         (char-equal (aref word u-idx) ?u))
                (setq word (substring word 0 u-idx))))
            (throw 'done word)))))

    ;; --- Step 2b: short suffixes
    (catch 'done
      (dolist (s snowball-spanish--step2b-short)
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p rv))
            (setq word (substring word 0 p))
            (setq word (snowball-spanish--maybe-drop-preceding-u-after-g word p rv nil))
            (throw 'done word)))))

    ;; --- Step 2b: long suffixes
    (dolist (s snowball-spanish--step2b-long)
      (let ((p (snowball-spanish--string-suffix-p s word)))
        (when (and p (>= p rv))
          (setq word (concat (substring word 0 p)
                             (substring word (+ p (length s))))))))

    word))


;; --- Step 3: residual suffix -------------------------------------------------
;;
(defun snowball-spanish--step3 (w rv)
  "Spanish Step 3 (residual suffixes) + special 'e/é' and 'gu' handling."
  (catch 'return
    (let ((word w))
      ;; delete suffix os / a / o / á / í / ó if in RV
      (dolist (s '("os" "a" "o" "á" "í" "ó"))
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p rv))
            (setq word (concat (substring word 0 p)
                               (substring word (+ p (length s)))))
            (throw 'return word))))
      ;; delete suffix e / é if in RV; if preceded by 'gu' with u in RV, drop 'u'
      (dolist (s '("e" "é"))
        (let ((p (snowball-spanish--string-suffix-p s word)))
          (when (and p (>= p rv))
            (setq word (concat (substring word 0 p)
                               (substring word (1+ p))))
            (setq word (snowball-spanish--maybe-drop-preceding-u-after-g word p rv t))
            (throw 'return word))))
      word)))

;; --- Main entry --------------------------------------------------------------

(defun snowball-spanish-stem (word)
  "Return the Snowball (Spanish) stem of WORD as a new string.
WORD should be a single token in NFC, lower/upper case allowed, may include accents.
Handles stopwords and conjugated forms of ser/estar/haber/tener specially."
  (unless (and word (stringp word))
    (user-error "snowball-spanish-stem expects a string"))
  (let* ((w (downcase word))
         (lemma (and (boundp 'snowball-spanish--lemma-map)
                     (hash-table-p snowball-spanish--lemma-map)
                     (gethash w snowball-spanish--lemma-map)))
         (result
          (cond
           ;; 1) lemma override → infinitive
           (lemma
            lemma)
           ;; 2) true stopwords → unchanged
           ((and (boundp 'snowball-spanish--stopwords)
                 (hash-table-p snowball-spanish--stopwords)
                 (gethash w snowball-spanish--stopwords))
            w)
           ;; 3) very short → unchanged
           ((<= (length w) 2)
            w)
           ;; 4) normal Snowball pipeline
           (t
            (let* ((r1r2 (snowball-spanish--compute-R1-R2 w))
                   (r1   (car r1r2))
                   (r2   (cdr r1r2))
                   (rv   (snowball-spanish--compute-RV w)))
              (setq w (snowball-spanish--step0-attached-pronoun w rv))
              (let ((w1 (snowball-spanish--step1 w r1 r2 rv)))
                (setq w (if (string= w1 w) (snowball-spanish--step2 w rv) w1)))
              (setq w (snowball-spanish--step3 w rv))
              (snowball-spanish--remove-acute-accents w))))))
    result))

(defun snowball-spanish-stem-tokens (tokens)
  "Map `snowball-spanish-stem' over a list of string TOKENS."
  (mapcar #'snowball-spanish-stem tokens))

(provide 'snowball-spanish)

;;; snowball-spanish.el ends here
