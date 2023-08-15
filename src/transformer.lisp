(in-package :polisher)


(defun transform-into-sexp (formula)
  (let ((formula-vec (coerce formula 'vector)))
    (parse-formula formula-vec 0 (length formula-vec))))


(defun parse-formula (formula begin end)
  (if (should-be-peeled formula begin end)
      (if (>= begin (- end 2))
          (error "Invalid paren")
          (parse-formula formula (+ begin 1) (- end 1)))
      (let ((div-index (find-split-point formula begin end)))
        (if (< div-index 0) ;; formula contains no operators
            (parse-value-or-function formula begin end)
            (case (slot-value (aref formula div-index) 'args)
              (1 (if (= div-index begin)
                     (list (implementation-of (aref formula div-index))
                           (parse-formula formula (+ div-index 1) end))
                     (error "Invalid unary operator")))
              (2 (list (implementation-of (aref formula div-index))
                       (parse-formula formula begin div-index)
                       (parse-formula formula (+ div-index 1) end))))))))


(defun should-be-peeled (formula begin end)
  (and (< begin end)
       (eq (aref formula begin) *left-paren*)
       (loop with last-index = (- end 1)
             for i from begin below end
             for token = (aref formula i)
             with paren-depth = 0
             do (cond
                  ((eq token *left-paren*) (incf paren-depth))
                  ((eq token *right-paren*)
                   (progn
                     (decf paren-depth)
                     (when (= paren-depth 0)
                       (return (= i last-index))))))
             finally (return nil))))


(defun find-split-point (formula begin end)
  (loop for i from begin below end
        for token = (aref formula i)
        with max-priority = (+ *max-priority* 1)
        with index = -1
        with paren-depth = 0
        do (cond
             ((eq token *left-paren*) (incf paren-depth))
             ((eq token *right-paren*)
              (progn
                (decf paren-depth)
                (when (< paren-depth 0)
                  (error "Unmatched paren"))))
             ((and (= paren-depth 0)
                   (typep token 'operator)
                   (or (and (is-left-associative token)
                            (<= (priority-of token) max-priority))
                       (and (not (is-left-associative token))
                            (< (priority-of token) max-priority))))
              (setf max-priority (priority-of token))
              (setf index i)))
        finally (if (> paren-depth 0)
                    (error "Unmatched paren")
                    (return index))))


(defun parse-value-or-function (formula begin end)
  (cond
    ((>= begin end) (error "Invalid formula"))
    ((= (- end begin) 1) (aref formula begin)) ; symbol or value
    ((and (symbolp (aref formula begin))
          (eq (aref formula (+ begin 1)) *left-paren*)) ; function
     (let ((children nil))
       (loop
         for i from (+ begin 2) below end
         for token = (aref formula i)
         with last-index = (- end 1)
         with paren-depth = 1
         with buffer-begin = (+ begin 2)
         do (cond
              ((eq token *separator*)
               (if (= buffer-begin i)
                   (error "Invalid argument")
                   (progn
                     (push (parse-formula formula buffer-begin i) children)
                     (setf buffer-begin (+ i 1)))))

              ((eq token *left-paren*) (incf paren-depth))

              ((eq token *right-paren*)
               (progn
                 (decf paren-depth)
                 (when (= paren-depth 0)
                   (when (/= i last-index)
                     (error "Invalid sytax"))
                   (when (< buffer-begin i)
                     (push (parse-formula formula buffer-begin i) children))
                   (return)))))
         finally (error "Unreachable point"))
       (cons (aref formula begin) (nreverse children))))
    (t (error "Invalid formula"))))
