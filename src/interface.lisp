(in-package :polisher)


;; Interface function
(defun infix-to-sexp (formula-str)
  (transform-into-sexp (tokenize formula-str)))


;; Macro
(defmacro polish (formula-str)
  (infix-to-sexp formula-str))


;; Reader macro
(defun read-formula (stream end-char)
  (coerce (loop for c = (read-char stream)
                if (char= c end-char) do (loop-finish)
                collect c)
          'string))


(defun activate-infix-syntax (&optional (activate t) (dispatch-char #\i))
  (if activate
      (progn
        (when (get-dispatch-macro-character #\# dispatch-char)
          (warn (format nil "Reader macro #~a has been overwritten" dispatch-char)))
        (set-dispatch-macro-character #\# dispatch-char
                                      #'(lambda (stream disp-char sub-char)
                                          (declare (ignore disp-char sub-char))
                                          (let ((first-char (read-char stream)))
                                            (when (char/= first-char #\{)
                                              (error "Infix syntax must be like #i{...}"))
                                            (infix-to-sexp (read-formula stream #\}))))))
      (set-dispatch-macro-character #\# dispatch-char nil)))
