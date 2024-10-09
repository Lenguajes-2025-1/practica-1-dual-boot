(ns grammars)

(defprotocol WAE)

(defrecord Bindings [id value])
(defrecord IdG [i] WAE)
(defrecord NumG [n] WAE)
(defrecord AddG [izq der] WAE)
(defrecord SubG [izq der] WAE)
(defrecord WithG [assign body] WAE)

(defn bindings [id value]
  (if (and (symbol? id) (satisfies? WAE value))
    (->Bindings id value)
    (throw (IllegalArgumentException. "Error"))))

(defn idG [i]
  (if (symbol? i)
    (->IdG i)
    (throw (IllegalArgumentException. "Id debe ser un simbolo"))))

(defn numG [n]
  (if (number? n)
    (->NumG n)
    (throw (IllegalArgumentException. "Num debe ser un numero"))))

(defn addG [i d]
  (if (and (satisfies? WAE i)
           (satisfies? WAE d))
    (->AddG i d)
    (throw (IllegalArgumentException. "Los parametros deben ser de tipo WAE"))))

(defn subG [i d]
  (if (and (satisfies? WAE i)
           (satisfies? WAE d))
    (->SubG i d)
    (throw (IllegalArgumentException. "Los parametros deben ser de tipo WAE"))))

(defn withG [bindings body]
  (if (and
       (instance? Bindings bindings)
       (satisfies? WAE body))
    (->WithG bindings body)
    (throw (IllegalArgumentException. "Los parametros deben ser de tipo Bindings WAE"))))

(defn print_WAE [exp]
  (cond
    (instance? IdG exp)
    (str "(id " (:i exp) ")")

    (instance? NumG exp)
    (str "(num " (:n exp) ")")

    (instance? AddG exp)
    (str "(add " (print_WAE (:izq exp)) " " (print_WAE (:der exp)) ")")

    (instance? SubG exp)
    (str "(sub " (print_WAE (:izq exp)) " " (print_WAE (:der exp)) ")")

    (instance? WithG exp)
    (let [binding (:assign exp)]
      (str "(with (" (:id binding) " " (print_WAE (:value binding)) ") " (print_WAE (:body exp)) ")"))))
    

