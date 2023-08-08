(defpackage polisher
  (:use :cl)
  (:export :polish
           :operator
           :add-operator :list-operators :*operator-list*
           :activate-infix-syntax))
