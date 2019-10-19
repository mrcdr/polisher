(in-package :polisher)


;; From Paul Graham's "On Lisp"
(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))


(defun match-length (regex target-string &optional group-index)
  (multiple-value-bind (start end s-group e-group)
      (cl-ppcre:scan regex target-string)
    (when start
      (if group-index
          (- (aref e-group group-index) (aref s-group group-index))
          (- end start)))))


(defun create-operator-regex ()
  (format nil "~{~a~^|~}" (mapcar
                           #'(lambda (x)
                               (cl-ppcre:quote-meta-chars
                                (symbol-name (slot-value x 'symbol))))
                           *operator-list*)))

(defun read-safely (str)
  (multiple-value-bind (read position) (read-from-string str)
    (when (/= (length str) position)
      (error (format nil "Illegal read: trying to read \"~a\", got ~a" str (string read))))
    read))


(defun tokenize (formula-str)
  (let* ((signed-value-regex "^[+-]?[0-9]+(?:\\.[0-9]*)?(?:[dDeEfFlLsS][+-]?[0-9]+)?")
         (unsigned-value-regex "^[0-9]+(?:\\.[0-9]*)?(?:[dDeEfFlLsS][+-]?[0-9]+)?")
         (signed-double-quote-regex "^[+-]\".*?\"")
         (unsigned-double-quote-regex "^\".*?\"")
         (operator-regex-tmp (create-operator-regex))
         (operator-regex (format nil "^(~a)" operator-regex-tmp))
         (separator-regex (format nil "~a|\\(|\\)|," operator-regex-tmp))
         (signed-symbol-regex (format nil "^[+-](.+?)(?:~a|\\s|\\z)" separator-regex)) ;; \z is required to
         (unsigned-symbol-regex (format nil "^(.+?)(?:~a|\\s|\\z)" separator-regex)))  ;; terminate non-greedy match
    (let ((tokenized nil))
      (loop with rest-str = formula-str
            with sign-allowed = t
            if (zerop (length rest-str)) do (loop-finish)
            do (acond
                ((match-length "^\\s+" rest-str)
                 (setf rest-str (subseq rest-str it))) ; remove spaces

                ((match-length "^#" rest-str) ;; parse reader macro
                 (declare (ignore it))
                 (multiple-value-bind (read-form position)
                     (read-from-string rest-str)
                   (push read-form tokenized)
                   (setf rest-str (subseq rest-str position))
                   (setf sign-allowed nil)))
                
                ((and sign-allowed (match-length signed-value-regex rest-str))
                 (progn
                   (push (read-safely (subseq rest-str 0 it)) tokenized)
                   (setf rest-str (subseq rest-str it))
                   (setf sign-allowed nil)))
                
                ((match-length unsigned-value-regex rest-str)
                 (progn
                   (push (read-safely (subseq rest-str 0 it)) tokenized)
                   (setf rest-str (subseq rest-str it))
                   (setf sign-allowed nil)))

                ((and sign-allowed (match-length signed-double-quote-regex rest-str))
                 (progn
                   (when (char= (char rest-str 0) #\-)
                     ;; When double-quoted symbol with negative sign, insert "(-1)*"
                     (push *left-paren* tokenized)
                     (push -1 tokenized)
                     (push *right-paren* tokenized)
                     (push (symbol-to-operator '*) tokenized))
                   (push (read-safely (subseq rest-str 2 (- it 1))) tokenized) ; remove sign and double quotes
                   (setf rest-str (subseq rest-str it))
                   (setf sign-allowed nil)))

                ((and (match-length unsigned-double-quote-regex rest-str))
                 (progn
                   (push (read-safely (subseq rest-str 1 (- it 1))) tokenized) ; remove double quotes
                   (setf rest-str (subseq rest-str it))
                   (setf sign-allowed nil)))

                ((cl-ppcre:scan "^\"" rest-str)
                 ;; Reaching here through the above conditions means unmatched double quote existing
                 (declare (ignore it))
                 (error "Unmatched double quote"))

                ((and (not sign-allowed) (match-length operator-regex rest-str 0))
                 (progn
                   (push (symbol-to-operator (read-from-string (subseq rest-str 0 it))) tokenized)
                   (setf rest-str (subseq rest-str it))
                   (setf sign-allowed nil)))
                  
                ((match-length "^\\(" rest-str)
                 (progn
                   (push *left-paren* tokenized)
                   (setf rest-str (subseq rest-str it))
                   (setf sign-allowed t)))

                ((match-length "^\\)" rest-str)
                 (progn
                   (push *right-paren* tokenized)
                   (setf rest-str (subseq rest-str it))
                   (setf sign-allowed nil)))
                
                ((match-length "^," rest-str)
                 (progn
                   (push *separator* tokenized)
                   (setf rest-str (subseq rest-str it))
                   (setf sign-allowed t)))
                
                ((and sign-allowed (match-length signed-symbol-regex rest-str 0))
                 (progn
                   (when (char= (char rest-str 0) #\-)
                     ;; When symbol with negative sign, insert "(-1)*"
                     (push *left-paren* tokenized)
                     (push -1 tokenized)
                     (push *right-paren* tokenized)
                     (push (symbol-to-operator '*) tokenized))
                   (push (read-safely (subseq rest-str 1 (+ 1 it))) tokenized) ; skip sign
                   (setf rest-str (subseq rest-str (+ 1 it)))
                   (setf sign-allowed nil)))
                
                ((match-length unsigned-symbol-regex rest-str 0)
                 (progn
                   (push (read-safely (subseq rest-str 0 it)) tokenized)
                   (setf rest-str (subseq rest-str it))
                   (setf sign-allowed nil)))
                
                (t
                 (declare (ignore it))
                 (error "Invalid sytax"))))
      (reverse tokenized))))
