(use 'clojure.test)

; Creates a java interface called ExprC
(defprotocol ExcrC
  (foo [this]))

; Classes that implement ExprC
(defrecord NumC [^Integer n]
  ExcrC)
(defrecord TrueC []
  ExcrC)
(defrecord FalseC []
  ExcrC)

; Creates a java interface called Value
(defprotocol Value
  (foo [this]))

; Classes that implement Value
(defrecord NumV [^int n]
  Value)
(defrecord BoolV [^boolean b]
  Value)

(deftest testValue
  (is (= (new NumV 4) (new NumV 4)))
  (is (not (= (new NumV 4) (new NumV 6)))))

(def n (new NumC 4))
(println (instance? NumC n))

(defn serialize ^String [^::ExcrC ex]
  "serialize an ExcrC"
  (cond
    (instance? NumC ex) (str (:n ex))
    (instance? TrueC ex) "true"
    (instance? FalseC ex) "false"
    :else (throw (new Exception "serialize requires an ExcrC"))))

(deftest testSerialize
  (is (= "true" (serialize (new TrueC))))
  (is (= "false" (serialize (new FalseC))))
  (is (= "4" (serialize (new NumC 4))))
  (is (= "-12" (serialize (new NumC -12))))
  (is (thrown-with-msg? Exception #"serialize requires an ExcrC"
      (serialize "asdf"))))


(run-tests)
