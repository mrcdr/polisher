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

(defun two-reterner ()
  2)

(defun tree-exact-equal (a b)
  (tree-equal a b :test #'equal))

(add-operator (make-instance 'operator :symbol '^ :function 'expt :priority 3 :left-associative nil))
(add-operator (make-instance 'operator :symbol '? :function 'mod :priority 10000))

(define-test "tokenize"
  (is tree-exact-equal (polisher::tokenize "1+1") (map-op-object '(1 + 1)))
  (is tree-exact-equal (polisher::tokenize "1.0+1") (map-op-object '(1.0 + 1)))
  (is tree-exact-equal (polisher::tokenize "1+.2") (map-op-object '(1 + .2)))
  (isnt tree-exact-equal (polisher::tokenize "1.0d0+1") (map-op-object '(1 + 1)))
  (is tree-exact-equal (polisher::tokenize "2+3*4") (map-op-object '(2 + 3 * 4)))
  (is tree-exact-equal (polisher::tokenize "2.0d0+3.e0*4") (map-op-object '(2.0d0 + 3.e0 * 4)))
  (is tree-exact-equal (polisher::tokenize "  2+ 3*  4") (map-op-object '(2 + 3 * 4)))
  (is tree-exact-equal (polisher::tokenize "  2+ 3*  4    ") (map-op-object '(2 + 3 * 4)))
  (is tree-exact-equal (polisher::tokenize "(2+3)*4") (map-op-object `(,*left-paren* 2 + 3 ,*right-paren* * 4)))
  (is tree-exact-equal (polisher::tokenize "-1") (map-op-object `(,*left-paren* -1 ,*right-paren* * 1 )))
  (is tree-exact-equal (polisher::tokenize "sin(x)") (map-op-object `(sin ,*left-paren* x ,*right-paren*)))
  (is tree-exact-equal (polisher::tokenize "  sin  ( x  )     ")
              (map-op-object `(sin ,*left-paren* x ,*right-paren*)))
  (is tree-exact-equal (polisher::tokenize "-sin(x)")
              (map-op-object `(,*left-paren* -1 ,*right-paren* * sin ,*left-paren* x ,*right-paren*)))
  (is tree-exact-equal (polisher::tokenize "sin(-abc,z)")
              (map-op-object `(sin ,*left-paren* ,*left-paren* -1 ,*right-paren* * abc
                                   ,*separator* z ,*right-paren*)))
  (is tree-exact-equal (polisher::tokenize "1+\"*gamma*\"*\"+euler-constant+\"")
                  (map-op-object `(1 + *gamma* * +euler-constant+)))
  (is tree-exact-equal (polisher::tokenize "1+\"long-name-symbol\"")
                  (map-op-object `(1 + long-name-symbol)))
  (is tree-exact-equal (polisher::tokenize "1+sin(\"long-name-symbol\")")
                  (map-op-object `(1 + sin ,*left-paren* long-name-symbol ,*right-paren*)))
  (is tree-exact-equal (polisher::tokenize "\"*long-name-symbol*\"+2.5")
                  (map-op-object `(*long-name-symbol* + 2.5)))
  (is tree-exact-equal (polisher::tokenize "-     x + y")
                  (map-op-object `(,*left-paren* -1 ,*right-paren* * x + y)))
  (is tree-exact-equal (polisher::tokenize "sin(-     \"*x*\")")
                  (map-op-object `(sin ,*left-paren* ,*left-paren* -1 ,*right-paren* * *x* ,*right-paren*)))
  (is tree-exact-equal (polisher::tokenize "sin   (-

  \"*x*\")")
                  (map-op-object `(sin ,*left-paren* ,*left-paren* -1 ,*right-paren* * *x* ,*right-paren*)))
  (is tree-exact-equal (polisher::tokenize "-     3.14")
                  (map-op-object `(,*left-paren* -1 ,*right-paren* * 3.14)))
  )

(define-test "transform"
  (is tree-exact-equal (polisher::infix-to-sexp "1+1") '(+ 1 1))
  (is tree-exact-equal (polisher::infix-to-sexp "(1+(1))") '(+ 1 1))
  (is tree-exact-equal (polisher::infix-to-sexp "1*2*3") '(* (* 1 2) 3))
  (is tree-exact-equal (polisher::infix-to-sexp "1*(2*3)") '(* 1 (* 2 3)))
  (is tree-exact-equal (polisher::infix-to-sexp "1*2**3**4*5+6") '(+ (* (* 1 (expt 2 (expt 3 4))) 5) 6))
  (is tree-exact-equal (polisher::infix-to-sexp "1*2^3^4*5+6") '(+ (* (* 1 (expt 2 (expt 3 4))) 5) 6))
  (is tree-exact-equal (polisher::infix-to-sexp "1+2*3?4*5") '(+ 1 (* (* 2 (mod 3 4)) 5)))
  (is tree-exact-equal (polisher::infix-to-sexp "1e5+#C(2 4)") '(+ 1e5 #C(2 4)))
  (is tree-exact-equal (polisher::infix-to-sexp "1e+1+1") '(+ 1e+1 1))
  (is tree-exact-equal (polisher::infix-to-sexp "\"1e\"+1+1") '(+ (+ 1e 1) 1))
  (is tree-exact-equal (polisher::infix-to-sexp "sin(#b1011 +5)") '(sin (+ #b1011 5))) ; space required
  (is tree-exact-equal (polisher::infix-to-sexp "\"two-returner\"()") '(two-returner))
  (is tree-exact-equal (polisher::infix-to-sexp "\"two-returner\"") 'two-returner)
  )

(define-test "Typical formula"
  (is = (let ((a 1)
              (b 1)
              (c -2))
           (polisher::polish "(   -b+sqrt(b**2-4*a*c))/(2*a)"))
         1))

(define-test "Invalid formula"
  (fail (polisher::infix-to-sexp ""))
  (fail (polisher::infix-to-sexp "1+"))
  (fail (polisher::infix-to-sexp "1++2"))
  (fail (polisher::infix-to-sexp "sin("))
  (fail (polisher::infix-to-sexp "(1)(2)"))
  (fail (polisher::infix-to-sexp "()"))
  (fail (polisher::infix-to-sexp "x y z"))
  (fail (polisher::infix-to-sexp "sin    +"))
  (fail (polisher::infix-to-sexp "1ee2+3"))
  (fail (polisher::infix-to-sexp "1+\"*gamma*\"*\"+euler-constant+")))

(defun run-test ()
  (test (package-tests :polisher.test)))

(defun run-test-interactive ()
  (test (package-tests :polisher.test) :report 'interactive))
