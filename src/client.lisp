(uiop:define-package #:yandex-metrika/client
  (:use #:cl)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:jonathan
                #:to-json)
  (:import-from #:yandex-metrika/vars
                #:*counter*)
  (:export #:hit
           #:*counter*))
(in-package #:yandex-metrika/client)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-interpol:enable-interpol-syntax))


(defun hit (url &key params user-id (timeout 1))
  (unless *counter*
    (error "Please, set cl-yandex-metrika:*counter* variable"))
  (let* ((rn (random 1000000))
         (endpoint #?"https://mc.yandex.ru/watch/${*counter*}/")
         (browser-info #?"en:utf-8:rn:${rn}:ar:1")
         (content `(("page-url" . ,url)
                    ("ut" . "noindex")
                    ("browser-info" . ,browser-info))))

    (when user-id
      ;; This should be a nested dictionary like
      ;; "{\"__ym\":{\"user_id\":\"100500\"}}"
      (setf (getf (getf params
                        :|__ym|)
                  :|user_id|)
            user-id))
    
    (when params
      (setf (assoc-value content "site-info")
            (to-json params)))

    (log:debug "Sending hit to" endpoint "with" content)
    (dex:post endpoint :content content
                       :read-timeout timeout
                       :connect-timeout timeout)
    (values)))
