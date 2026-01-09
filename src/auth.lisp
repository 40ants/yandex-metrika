(uiop:define-package #:yandex-metrika/auth
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:yandex-metrika/vars
                #:*counter*
                #:*token*)
  (:import-from #:secret-values
                #:ensure-value-revealed)
  (:import-from #:log4cl-extras/secrets
                #:with-secrets))
(in-package #:yandex-metrika/auth)


(-> check-credentials ()
    (values &optional))

(defun check-credentials ()
  "Check that required credentials are set."
  (unless *counter*
    (error "Please set yandex-metrika/vars:*counter* variable"))
  (unless *token*
    (error "Please set yandex-metrika/vars:*token* variable"))
  (values))


(defun call-with-auth-headers (thunk)
  (check-credentials)
  
  (let* ((value (concatenate 'string "OAuth "
                             (ensure-value-revealed *token*)))
         (headers (list (cons "Authorization" value))))
    ;; It is critical to let log4cl-extras know that this value
    ;; should not be logged in a backtrace.
    (with-secrets (value)
      (funcall thunk headers))))


(defmacro with-auth-headers ((headers-var) &body body)
  "Creates authorization headers for API requests."
  `(flet ((with-auth-headers-thunk (,headers-var)
            ,@body))
     (declare (dynamic-extent #'with-auth-headers-thunk))
     (call-with-auth-headers #'with-auth-headers-thunk)))
