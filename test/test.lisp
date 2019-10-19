(in-package :polisher.test)

(defparameter *left-paren* polisher::*left-paren*)
(defparameter *right-paren* polisher::*right-paren*)
(defparameter *separator* polisher::*separator*)


(defun map-op-object (list)
  (mapcar #'(lambda (item)
              (let ((mapped (polisher::symbol-to-operator item)))
                (if mapped
                    mapped
                    item)))
          list))

(add-operator (make-instance 'operator :symbol '^ :function 'expt :priority 3 :left-associative nil))
(add-operator (make-instance 'operator :symbol '? :function 'mod :priority 10000))

(test tokenize-test
  (is (tree-equal (polisher::tokenize "1+1") (map-op-object '(1 + 1)) :test #'equal))
  (is (tree-equal (polisher::tokenize "1.0+1") (map-op-object '(1.0 + 1)) :test #'equal))
  (is (tree-equal (polisher::tokenize "1+.2") (map-op-object '(1 + .2)) :test #'equal))
  (is (not (tree-equal (polisher::tokenize "1.0d0+1") (map-op-object '(1 + 1)) :test #'equal)))
  (is (tree-equal (polisher::tokenize "2+3*4") (map-op-object '(2 + 3 * 4)) :test #'equal))
  (is (tree-equal (polisher::tokenize "2.0d0+3.e0*4") (map-op-object '(2.0d0 + 3.e0 * 4)) :test #'equal))
  (is (tree-equal (polisher::tokenize "  2+ 3*  4") (map-op-object '(2 + 3 * 4)) :test #'equal))
  (is (tree-equal (polisher::tokenize "  2+ 3*  4    ") (map-op-object '(2 + 3 * 4)) :test #'equal))
  (is (tree-equal (polisher::tokenize "(2+3)*4") (map-op-object `(,*left-paren* 2 + 3 ,*right-paren* * 4)) :test #'equal))
  (is (tree-equal (polisher::tokenize "-1") '(-1) :test #'equal))
  (is (tree-equal (polisher::tokenize "sin(x)")
              (map-op-object `(sin ,*left-paren* x ,*right-paren*)) :test #'equal))
  (is (tree-equal (polisher::tokenize "  sin  ( x  )     ")
              (map-op-object `(sin ,*left-paren* x ,*right-paren*)) :test #'equal))
  (is (tree-equal (polisher::tokenize "-sin(x)")
              (map-op-object `(,*left-paren* -1 ,*right-paren* * sin ,*left-paren* x ,*right-paren*))
              :test #'equal))
  (is (tree-equal (polisher::tokenize "sin(-abc,z)")
              (map-op-object `(sin ,*left-paren* ,*left-paren* -1 ,*right-paren* * abc
                                   ,*separator* z ,*right-paren*))
              :test #'equal))
  (is (tree-equal (polisher::tokenize "1+\"*gamma*\"*\"+euler-constant+\"")
                  (map-op-object `(1 + *gamma* * +euler-constant+))
                  :test #'equal))
  (is (tree-equal (polisher::tokenize "1+\"long-name-symbol\"")
                  (map-op-object `(1 + long-name-symbol))
                  :test #'equal))
  (is (tree-equal (polisher::tokenize "1+sin(\"long-name-symbol\")")
                  (map-op-object `(1 + sin ,*left-paren* long-name-symbol ,*right-paren*))
                  :test #'equal))
  (is (tree-equal (polisher::tokenize "\"*long-name-symbol*\"+2.5")
                  (map-op-object `(*long-name-symbol* + 2.5))
                  :test #'equal))
  )

(test transform-test
  (is (tree-equal (polisher::infix-to-sexp "1+1") '(+ 1 1) :test #'equal))
  (is (tree-equal (polisher::infix-to-sexp "(1+(1))") '(+ 1 1) :test #'equal))
  (is (tree-equal (polisher::infix-to-sexp "1*2*3") '(* (* 1 2) 3) :test #'equal))
  (is (tree-equal (polisher::infix-to-sexp "1*(2*3)") '(* 1 (* 2 3)) :test #'equal))
  (is (tree-equal (polisher::infix-to-sexp "1*2**3**4*5+6") '(+ (* (* 1 (expt 2 (expt 3 4))) 5) 6) :test #'equal))
  (is (tree-equal (polisher::infix-to-sexp "1*2^3^4*5+6") '(+ (* (* 1 (expt 2 (expt 3 4))) 5) 6) :test #'equal))
  (is (tree-equal (polisher::infix-to-sexp "1+2*3?4*5") '(+ 1 (* (* 2 (mod 3 4)) 5)) :test #'equal))
  (is (tree-equal (polisher::infix-to-sexp "1e5+#C(2 4)") '(+ 1e5 #C(2 4)) :test #'equal))
  (is (tree-equal (polisher::infix-to-sexp "sin(#b1011 +5)") '(sin (+ #b1011 5)) :test #'equal)) ; space required
  )

(test invalid-formula-test
  (signals simple-error (polisher::infix-to-sexp ""))
  (signals simple-error (polisher::infix-to-sexp "1+"))
  (signals simple-error (polisher::infix-to-sexp "1++2"))
  (signals simple-error (polisher::infix-to-sexp "sin("))
  (signals simple-error (polisher::infix-to-sexp "(1)(2)"))
  (signals simple-error (polisher::infix-to-sexp "()"))
  (signals simple-error (polisher::infix-to-sexp "x y z"))
  (signals simple-error (polisher::infix-to-sexp "sin    +"))
  (signals simple-error (polisher::infix-to-sexp "1ee2+3"))
  (signals simple-error (polisher::infix-to-sexp "1+\"*gamma*\"*\"+euler-constant+")))


(defun run-test ()
  (let ((*package* (find-package :polisher.test)))
    (run)))
