(in-package :polisher)


(defclass operator ()
  ((symbol :initarg :symbol
           :type 'symbol
           :documentation "Symbol used in infix-style.")
   (function :initarg :function
             :documentation "Function to be called, which must receive exactly two arguments.")
   (priority :initarg :priority
             :type 'number
             :documentation "Operator priority. Operators will be evaluated from ones having larger priority.")
   (left-associative :initarg :left-associative
                     :initform t
                     :type 'boole
                     :documentation "If t, this operator will be left associative (e.g. addition operator +).
If nil, this operator will be right associative (e.g. power operator **).")
   (args :initarg :args
         :initform 2
         :type 'number
         :documentation "This determines how many input values are expected by the operator.
The first arg goes before the operator and the rest after, as in languages like Haskell."))
  (:documentation "Operator class, whose instance will usually be registered by polisher:add-operator function."))


(defmethod initialize-instance :after
  ((this operator) &rest args)
  (declare (ignore args))
  (unless (slot-boundp this 'function)
    (setf (slot-value this 'function)
          (slot-value this 'symbol))))


(defmethod print-object ((this operator) stream)
  (format stream "op~a" (slot-value this 'symbol)))


(defgeneric readable-string (object))
(defmethod readable-string ((object operator))
  (format nil "symbol: ~5a, function: ~10a, priority: ~3a, left-associative: ~a, arg-count: ~a"
          (slot-value object 'symbol)
          (slot-value object 'function)
          (slot-value object 'priority)
          (slot-value object 'left-associative)
          (slot-value object 'args)))


(defparameter *operator-list* nil)


(defparameter *left-paren* '{)
(defparameter *right-paren* '})
(defparameter *separator* '_)

(defparameter *max-priority* 1000)
; This value will be automatically increased
; when a higher prior operator is registered.


(defun add-operator (op)
  "Add infix-style operator, which should be a polisher:operator instance."
  (unless (typep op 'operator)
    (error "Argument must be operator"))
  (unless (every #'(lambda (x)
                   (or (/= (slot-value op 'priority)
                           (slot-value x 'priority))
                       (eq (slot-value op 'left-associative)
                           (slot-value x 'left-associative))))
               *operator-list*)
    (error "Left- and right-associative operators both having the same priority can't be registered simultaneously"))
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
;; Here used a little trick; the symbol ** is interned in CL package as a previously evaluated result.

;; (add-operator (make-instance 'operator :symbol '^ :function 'expt :priority 3 :left-associative nil))
;; To enable this, symbol ^ must be exported by package definition.


(defun symbol-to-operator (symbol)
  (find-if #'(lambda (item)
               (equal symbol
                      (slot-value item 'symbol)))
           *operator-list*))
