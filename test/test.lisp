(in-package :polisher.test)


(defun map-op-object (list)
  (mapcar #'(lambda (item)
              (let ((mapped (symbol-to-operator item)))
                (if mapped
                    mapped
                    item)))
          list))

(add-operator (make-instance 'operator :symbol '^ :function 'expt :priority 3 :left-associative nil))

(test tokenize-test
  (is (tree-equal (tokenize "1+1") (map-op-object '(1 + 1)) :test #'equal))
  (is (tree-equal (tokenize "1.0+1") (map-op-object '(1.0 + 1)) :test #'equal))
  (is (tree-equal (tokenize "1+.2") (map-op-object '(1 + .2)) :test #'equal))
  (is (not (tree-equal (tokenize "1.0d0+1") (map-op-object '(1 + 1)) :test #'equal)))
  (is (tree-equal (tokenize "2+3*4") (map-op-object '(2 + 3 * 4)) :test #'equal))
  (is (tree-equal (tokenize "2.0d0+3.e0*4") (map-op-object '(2.0d0 + 3.e0 * 4)) :test #'equal))
  (is (tree-equal (tokenize "  2+ 3*  4") (map-op-object '(2 + 3 * 4)) :test #'equal))
  (is (tree-equal (tokenize "  2+ 3*  4    ") (map-op-object '(2 + 3 * 4)) :test #'equal))
  (is (tree-equal (tokenize "(2+3)*4") (map-op-object `(,*left-paren* 2 + 3 ,*right-paren* * 4)) :test #'equal))
  (is (tree-equal (tokenize "-1") '(-1) :test #'equal))
  (is (tree-equal (tokenize "sin(x)")
              (map-op-object `(sin ,*left-paren* x ,*right-paren*)) :test #'equal))
  (is (tree-equal (tokenize "  sin  ( x  )     ")
              (map-op-object `(sin ,*left-paren* x ,*right-paren*)) :test #'equal))
  (is (tree-equal (tokenize "-sin(x)")
              (map-op-object `(,*left-paren* -1 ,*right-paren* * sin ,*left-paren* x ,*right-paren*))
              :test #'equal))
  (is (tree-equal (tokenize "sin(-abc,z)")
              (map-op-object `(sin ,*left-paren* ,*left-paren* -1 ,*right-paren* * abc
                                   ,*separator* z ,*right-paren*))
              :test #'equal))
  )

(test transform-test
  (is (tree-equal (infix-to-sexp "1+1") '(+ 1 1) :test #'equal))
  (is (tree-equal (infix-to-sexp "(1+(1))") '(+ 1 1) :test #'equal))
  (is (tree-equal (infix-to-sexp "1*2*3") '(* (* 1 2) 3) :test #'equal))
  (is (tree-equal (infix-to-sexp "1*(2*3)") '(* 1 (* 2 3)) :test #'equal))
  (is (tree-equal (infix-to-sexp "1*2**3**4*5+6") '(+ (* (* 1 (expt 2 (expt 3 4))) 5) 6) :test #'equal))
  (is (tree-equal (infix-to-sexp "1*2^3^4*5+6") '(+ (* (* 1 (expt 2 (expt 3 4))) 5) 6) :test #'equal))
  (is (tree-equal (infix-to-sexp "1e5+#C(2 4)") '(+ 1e5 #C(2 4)) :test #'equal))
  (is (tree-equal (infix-to-sexp "sin(#b1011 +5)") '(sin (+ #b1011 5)) :test #'equal)) ; space required
  )

(test invalid-formula-test
  (signals simple-error (infix-to-sexp "") :test #'equal)
  (signals simple-error (infix-to-sexp "1+") :test #'equal)
  (signals simple-error (infix-to-sexp "1++2") :test #'equal)
  (signals simple-error (infix-to-sexp "sin(") :test #'equal)
  (signals simple-error (infix-to-sexp "(1)(2)") :test #'equal)
  (signals simple-error (infix-to-sexp "()") :test #'equal)
  (signals simple-error (infix-to-sexp "x y z") :test #'equal)
  (signals simple-error (infix-to-sexp "sin    +") :test #'equal)
  (signals simple-error (infix-to-sexp "1ee2+3")) :test #'equal)
