(in-package :polisher)


(defun transform-into-sexp (formula)
  (if (should-be-peeled formula)
      (let ((peeled (subseq formula 1 (- (length formula) 1))))
        (if (null peeled)
            (error "Invalid paren")
            (transform-into-sexp peeled)))
      (let ((div-index (find-split-point formula)))
        (if (< div-index 0) ;; formula contains no operators
            (parse-value-or-function formula)
            (list (slot-value (nth div-index formula) 'function)
                  (transform-into-sexp (subseq formula 0 div-index))
                  (transform-into-sexp (nthcdr (+ 1 div-index) formula)))))))


(defun should-be-peeled (formula)
  (and (eq (car formula) *left-paren*)
       (loop with last-index = (- (length formula) 1)
             for token in formula
             for i from 0
             with paren-depth = 0
             do (cond
                  ((eq token *left-paren*) (incf paren-depth))
                  ((eq token *right-paren*)
                   (progn
                     (decf paren-depth)
                     (when (= paren-depth 0)
                       (return (= i last-index))))))
             finally (return nil))))


(defun find-split-point (formula)
  (loop for i from 0
        for token in formula
        with max-priority = *max-priority*
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
                   (or (and (slot-value token 'left-associative)
                            (<= (slot-value token 'priority) max-priority))
                       (and (not (slot-value token 'left-associative))
                            (< (slot-value token 'priority) max-priority))))
                (setf max-priority (slot-value token 'priority))
                (setf index i)))
        finally (if (> paren-depth 0)
                    (error "Unmatched paren")
                    (return index))))


(defun parse-value-or-function (formula)
  (cond
    ((= (length formula) 1) (car formula)) ;; symbol or value
    ((and (symbolp (car formula))
          (eq (cadr formula) *left-paren*)) ;; function
     (let ((children nil))
       (loop
         for token in (cddr formula)         
         for i from 0
         with last-index = (- (length (cddr formula)) 1)
         with paren-depth = 1
         with buffer = nil
         do (cond
              ((eq token *separator*) (if (null buffer)
                                        (error "Invalid argument")
                                        (progn
                                          (push (transform-into-sexp (reverse buffer)) children)
                                          (setf buffer nil))))
              ((eq token *left-paren*) (progn
                                         (incf paren-depth)
                                         (push token buffer)))
              ((eq token *right-paren*) (progn
                                          (decf paren-depth)
                                          (if (= paren-depth 0)
                                              (progn
                                                (when (/= i last-index)
                                                  (error "Invalid sytax"))
                                                (when buffer
                                                  (push (transform-into-sexp (reverse buffer)) children))
                                                (return))
                                              (push token buffer))))
              (t (push token buffer)))
         finally (error "Unreachable point"))
       (cons (car formula) (reverse children))))
    (t (error "Invalid formula"))))


(defun tester (list)
  (transform-into-sexp (mapcar #'(lambda (item)
                                 (let ((conved (symbol-to-operator item)))
                                   (if (null conved)
                                       item
                                       conved)))
                             list)))
