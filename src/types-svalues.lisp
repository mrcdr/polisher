(in-package :polisher)


(defclass operator ()
  ((symbol :initarg :symbol)
   (function :initarg :function)
   (priority :initarg :priority)
   (left-associative :initarg :left-associative :initform t)))


(defmethod initialize-instance :after
  ((this operator) &rest args)
  (declare (ignore args))
  (unless (slot-boundp this 'function)
    (setf (slot-value this 'function)
          (slot-value this 'symbol))))


(defmethod print-object ((this operator) stream)
  (format stream "~a" (slot-value this 'symbol)))


(defparameter *operator-list*
  (list
   (make-instance 'operator :symbol '+ :priority 1)
   (make-instance 'operator :symbol '- :priority 1)
   (make-instance 'operator :symbol '* :priority 2)
   (make-instance 'operator :symbol '/ :priority 2)
   (make-instance 'operator :symbol '^ :function 'expt :priority 3 :left-associative nil)
   ))


(defparameter *left-paren* '{)
(defparameter *right-paren* '})
(defparameter *separator* '_)

(defparameter *max-priority* 1000)


(defun symbol-to-operator (symbol)
  (find-if #'(lambda (item)
                  (eq symbol
                      (slot-value item 'symbol)))
           *operator-list*))
