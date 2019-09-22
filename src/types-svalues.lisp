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
  (format stream "op~a" (slot-value this 'symbol)))


(defparameter *operator-list* nil)


(defparameter *left-paren* '{)
(defparameter *right-paren* '})
(defparameter *separator* '_)

(defparameter *max-priority* 1000)
; This value will be automatically increased
; when a higher prior operator is registered.


(defun add-operator (op)
  (unless (typep op 'operator)
    (error "Argument must be operator"))
  (remove-if #'(lambda (x) (eq (slot-value op 'symbol) (slot-value x 'symbol)))
             *operator-list*)
  (push op *operator-list*)
  (when (> (slot-value op 'priority) *max-priority*)
    (setf *max-priority* (slot-value op 'priority)))
  (setf *operator-list*
        (sort *operator-list* #'(lambda (x y)
                                  (>= (length (string (slot-value x 'symbol)))
                                      (length (string (slot-value y 'symbol))))))))

(add-operator (make-instance 'operator :symbol '+ :priority 1))
(add-operator (make-instance 'operator :symbol '- :priority 1))
(add-operator (make-instance 'operator :symbol '* :priority 2))
(add-operator (make-instance 'operator :symbol '/ :priority 2))
(add-operator (make-instance 'operator :symbol '** :function 'expt :priority 3 :left-associative nil))
; Here used a little trick; the symbol ** is interned in CL package as a previously evaluated result.

; (add-operator (make-instance 'operator :symbol '^ :function 'expt :priority 3 :left-associative nil))
; To enable this, symbol ^ must be exported by package definition.


(defun symbol-to-operator (symbol)
  (find-if #'(lambda (item)
               (equal symbol
                      (slot-value item 'symbol)))
           *operator-list*))
