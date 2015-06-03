(use 'clojure.test)

; Creates a java interface called ExprC
(defprotocol ExcrC
  (foo [this]))

; Classes that implement ExprC
(defrecord NumC [^int n]
  ExcrC)
(defrecord TrueC []
  ExcrC)
(defrecord FalseC []
  ExcrC)
(defrecord BinopC [^String op ^::ExcrC a ^::ExcrC b]
  ExcrC)
(defrecord IfC [^::ExcrC check
                ^::ExcrC then
                ^::ExcrC otherwise]
  ExcrC)

; Creates a java interface called Value
(defprotocol Value
  (foo [this]))

; Classes that implement Value
(defrecord NumV [^int n]
  Value)
(defrecord BoolV [^boolean b]
  Value)

;;;;;;;;;;;;; INTERP & HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn interp ^::Value [^::ExcrC ex]
  (cond
    (instance? NumC ex) (new NumV (:n ex))
    (instance? TrueC ex) (new BoolV true)
    (instance? FalseC ex) (new BoolV false)
    (instance? IfC ex)
      (let [res (interp (:check ex))]
        (cond
          (instance? BoolV res) (interp (:then ex))
          :else (throw (new Exception "must test boolean expression"))))
    (instance? BinopC ex)
      (case (:op ex)
        "+" (new NumV (+ (:n (:a ex)) (:n (:b ex))))
        "-" (new NumV (- (:n (:a ex)) (:n (:b ex))))
        "/" (new NumV (/ (:n (:a ex)) (:n (:b ex))))
        "*" (new NumV (* (:n (:a ex)) (:n (:b ex)))))))

(defn serialize ^String [^::ExcrC ex]
  "serialize an ExcrC"
  (cond
    (instance? NumC ex) (str (:n ex))
    (instance? TrueC ex) "true"
    (instance? FalseC ex) "false"
    :else (throw (new Exception "serialize requires an ExcrC"))))

;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(def n (new NumC 4))
;(println (instance? NumC n))

(deftest testValue
  (is (= (new NumV 4) (new NumV 4)))
  (is (not (= (new NumV 4) (new NumV 6)))))

(deftest testSerialize
  (is (= "true" (serialize (new TrueC))))
  (is (= "false" (serialize (new FalseC))))
  (is (= "4" (serialize (new NumC 4))))
  (is (= "-12" (serialize (new NumC -12))))
  (is (thrown-with-msg? Exception #"serialize requires an ExcrC"
      (serialize "asdf"))))

(deftest testInterp
  (is (= (new NumV 4) (interp (new NumC 4))))
  (is (= (new BoolV true) (interp (new TrueC))))
  (is (= (new BoolV false) (interp (new FalseC))))
  (is (= (new NumV 4) (interp (new IfC (new TrueC) (new NumC 4) (new NumC 5)))))
  (is (= (new NumV 7) (interp (new BinopC "+" (new NumV 3) (new NumV 4)))))
  (is (= (new NumV 12) (interp (new BinopC "*" (new NumV 3) (new NumV 4)))))
  (is (= (new NumV 3/4) (interp (new BinopC "/" (new NumV 3) (new NumV 4))))))

(run-tests)
