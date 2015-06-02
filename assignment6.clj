(use 'clojure.test)

(defprotocol ExcrC
  (foo [this]))

(defrecord NumC [^Integer n]
  ExcrC)
(defrecord TrueC []
  ExcrC)
(defrecord FalseC []
  ExcrC)

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
