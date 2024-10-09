(ns parser
  (:require [grammars :as wae]))

(defn parser-AE
  "Se parsea una expresión de tipo AE para relacionarla con su tipo correspondiente.

  Params: 
   - [exp]: La expresión de tipo AE a relacionar.

   Uses:
   - number?: Verifica si la expresión es un número.
   - list?:  Verifica si la expresión es una lista de expresiónes.
   - first:  Para obtener el primer elemento de la expresión.
   - second: Para obtener el segundo elemento de la expresión.
   - nth:    Para obtener el n-ésimo elemento de la expresión.

   - Returns: 
     Expresión AE relacionada con su tipo correspondiente.
   "

  [exp]
  (cond
    (number? exp) (wae/numG exp)
    (list? exp) (case (first exp)
                  + (wae/addG (parser-AE (second exp)) (parser-AE (nth exp 2)))
                  - (wae/subG (parser-AE (second exp)) (parser-AE (nth exp 2)))
                  ;; Si el operador no es reconocido se lanza la excepción.
                  (throw (Exception. (str "Operador desconocido: " (first exp)))))
    :else (throw (Exception. (str "Expresión Incorrecta: " exp)))))

(defn parser-WAE
  "Se parsea una expresión de tipo WAE para relacionarla con su tipo correspondiente.
  
    Params: 
     - [exp]: La expresión de tipo WAE a relacionar.
  
     
     Uses:
     - number?: Verifica si la expresión es un número.
     - symbol?: Verifica si la expresión es un símbolo. 
     - list?:  Verifica si la expresión es una lista de expresiónes.
     - first:  Para obtener el primer elemento de la expresión.
     - second: Para obtener el segundo elemento de la expresión.
     - nth:    Para obtener el n-ésimo elemento de la expresión.
  
     - Returns: 
       Expresión WAE relacionada con su tipo correspondiente.
     "
  [exp]
  (cond
    (number? exp) (wae/numG exp)
    (symbol? exp) (wae/idG exp)
    (list? exp) (case (first exp)
                  + (wae/addG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
                  - (wae/subG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))
                  with (let [binding (second exp)
                             id (first binding)
                             value (second binding)
                             body (nth exp 2)]
                         (wae/withG (wae/bindings id (parser-WAE value))
                                    (parser-WAE body)))

                  ;; Si el operador no es reconocido se lanza la excepción.
                  (throw (Exception. (str "Operador desconocido: " (first exp)))))

    ;; Si no es número, símbolo o lista, lanza una excepción
    :else (throw (Exception. (str "Expresión Incorrecta: " exp)))))