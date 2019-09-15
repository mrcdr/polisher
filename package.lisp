(defpackage polisher
  (:use :cl)
  (:export :infix-to-sexp
           :add-operator
           :tokenize
           :transform-into-sexp
           :symbol-to-operator
           :operator
           :*left-paren*
           :*right-paren*
           :*separator*))
