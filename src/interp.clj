(ns interp
  (:require [grammars :as wae]))

(defn interp-AE
  "Interpreta una expresión de tipo AE y la evalua a un resultado numérico.
  
   Params:
   - [exp]: La expresión de tipo AE que se evaluará.
   
   Uses:
   - instance?: Para revisar si la expresión ingresada es alguna instancia de 
    de un tipo AE
   
   Returns:
   - Valor número de la expresión ingresada.
   "
  [exp]
  (cond
    ;; Si es un número, simplemente retorna su valor
    (instance? wae/NumG exp) (:n exp)
    ;; Si es una suma, se evaluan ambos lados y se suma.
    (instance? wae/AddG exp) (+ (interp-AE (:izq exp)) (interp-AE (:der exp)))
    ;; Si es una resta, se evaluan ambos lados y se resta.
    (instance? wae/SubG exp) (- (interp-AE (:izq exp)) (interp-AE (:der exp)))

    :else (throw (Exception. (str "Expresión AE Inválida: " exp)))))

(defn interp-WAE
  "Interpreta una expresión de tipo WAE y la evalua a un resultado numérico.
   
   Uses:
   - instance?: Para revisar si la expresión ingresada es alguna instancia de 
    de un tipo WAE.
   - get: Para obtener, en donde haga falta, el entorno en el que se interpreta
    una expresión. 
   
   Params:
    - exp: La expresión de tipo WAE que se evaluará.
    - env: El entorno en que se esta evaluando la expresión WAE.
   
   Returns:
   - Valor número de la expresión ingresada."
  [exp env]
  (cond
    ;; Si es un número, simplemente retorna su valor
    (instance? wae/NumG exp) (:n exp)

    ;; Si es un identificador, busca su valor en el entorno
    (instance? wae/IdG exp) (get env (:id exp))

    ;; Si es una suma, evalúa ambos lados y suma
    (instance? wae/AddG exp) (+ (interp-WAE (:izq exp) env) (interp-WAE (:der exp) env))

    ;; Si es una resta, evalúa ambos lados y resta
    (instance? wae/SubG exp) (- (interp-WAE (:izq exp) env) (interp-WAE (:der exp) env))

    ;; Si es una expresión 'with', evalúa el valor y actualiza el entorno
    (instance? wae/WithG exp)
    (let [id (:id exp)
          value (interp-WAE (:value exp) env)  ;; Evalúa el valor
          body (:body exp)]
      (interp-WAE body (assoc env id value))) ;; Actualiza el entorno con el nuevo valor

    :else (throw (Exception. (str "Expresión WAE Inválida: " exp)))))