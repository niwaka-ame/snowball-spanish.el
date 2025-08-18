(require 'ert)

(defconst snowball-spanish--test-words
  '("che" "checa" "checar" "checo" "checoslovaquia" "chedraoui"
    "chefs" "cheliabinsk" "chelo" "chemical" "chemicalweek" "chemise"
    "chepo" "cheque" "chequeo" "cheques" "cheraw" "chesca" "chester"
    "chetumal" "chetumaleños" "chevrolet" "cheyene" "cheyenne" "chi"
    "chía" "chiapaneca" "chiapas" "chiba" "chic" "chica" "chicago"
    "chicana" "chicano" "chicas" "chicharrones" "chichen" "chichimecas"
    "chicles" "chico" "torá" "tórax" "torcer" "toreado" "toreados"
    "toreándolo" "torear" "toreara" "torearlo" "toreó" "torero"
    "toreros" "torio" "tormenta" "tormentas" "tornado" "tornados"
    "tornar" "tornen" "torneo" "torneos" "tornillo" "tornillos"
    "torniquete" "torno" "toro" "toronto" "toros" "torpedearon"
    "torpeza" "torrado" "torralba" "torre" "torrencial" "torrenciales"
    "torrente" "torreon" "torreón" "torres" "torrescano"))

(defconst snowball-spanish--test-stems
  '("che" "chec" "chec" "chec" "checoslovaqui" "chedraoui"
    "chefs" "cheliabinsk" "chel" "chemical" "chemicalweek" "chemis"
    "chep" "chequ" "cheque" "chequ" "cheraw" "chesc" "chest"
    "chetumal" "chetumaleñ" "chevrolet" "cheyen" "cheyenn" "chi"
    "chi" "chiapanec" "chiap" "chib" "chic" "chic" "chicag"
    "chican" "chican" "chic" "chicharron" "chich" "chichimec"
    "chicl" "chic" "tor" "torax" "torc" "tor" "tor" "tor" "tor" "tor" "tor"
    "tore" "torer" "torer" "tori" "torment" "torment" "torn" "torn"
    "torn" "torn" "torne" "torne" "tornill" "tornill" "torniquet"
    "torn" "tor" "toront" "tor" "torped" "torpez" "torr" "torralb"
    "torr" "torrencial" "torrencial" "torrent" "torreon" "torreon"
    "torr" "torrescan"))

(ert-deftest snowball-spanish-test-stemming ()
  "Check that `snowball-spanish-stem` matches the reference stems."
  (cl-loop for w in snowball-spanish--test-words
           for s in snowball-spanish--test-stems
           for got = (snowball-spanish-stem w)
           unless (string= got s)
           do (ert-fail (format "Word %S stemmed to %S, expected %S"
                                w got s))))
