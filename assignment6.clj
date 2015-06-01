(use 'clojure.test)

(defprotocol ExcrC)
(defrecord NumC [^int n]
  ExcrC)
(defrecord TrueC []
  ExcrC)
(defrecord FalseC []
  ExcrC)

(def n (new NumC 4))
(println (instance? NumC n))

(with-test
  (defn serialize ^String [^::ExcrC ex]
    "serialize an ExcrC"
    "hello")
  (is (= "true" (serialize (new TrueC))))
  (is (= "false" (serialize (new FalseC)))))

(run-all-tests)
