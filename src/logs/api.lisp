(uiop:define-package #:yandex-metrika/logs/api
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:yandex-metrika/vars
                #:*counter*
                #:*token*)
  (:import-from #:local-time
                #:timestamp
                #:format-timestring)
  (:export #:*api-base-url*
           #:api-get
           #:api-post
           #:format-date
           #:logs-api-error
           #:logs-api-error-code
           #:logs-api-error-message))
(in-package #:yandex-metrika/logs/api)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-interpol:enable-interpol-syntax))


(defvar *api-base-url* "https://api-metrica.yandex.net/management/v1/counter"
  "Base URL for Yandex Metrika Management API.")


(define-condition logs-api-error (error)
  ((code :initarg :code :reader logs-api-error-code)
   (message :initarg :message :reader logs-api-error-message))
  (:report (lambda (condition stream)
             (format stream "Logs API error ~A: ~A"
                     (logs-api-error-code condition)
                     (logs-api-error-message condition)))))


(-> check-credentials ()
    (values &optional))

(defun check-credentials ()
  "Check that required credentials are set."
  (unless *counter*
    (error "Please set yandex-metrika/vars:*counter* variable"))
  (unless *token*
    (error "Please set yandex-metrika/vars:*token* variable"))
  (values))


(-> make-auth-headers () list)
(defun make-auth-headers ()
  "Create authorization headers for API requests."
  `(("Authorization" . ,#?"OAuth ${*token*}")))


(-> build-url (string) string)
(defun build-url (path)
  "Build full API URL for the given path."
  #?"${*api-base-url*}/${*counter*}${path}")


(-> build-query-string (list) string)
(defun build-query-string (params)
  "Build a URL query string from an alist of parameters."
  (format nil "~{~A=~A~^&~}"
          (loop for (key . value) in params
                collect key
                collect (quri:url-encode (princ-to-string value)))))


(-> parse-response (string) hash-table)
(defun parse-response (response)
  "Parse JSON response body."
  (yason:parse response
               :object-as :hash-table
               :json-arrays-as-vectors t
               :json-booleans-as-symbols t
               :json-nulls-as-keyword nil
               :object-key-fn #'identity))


(-> handle-error-response (string integer) nil)
(defun handle-error-response (response status-code)
  "Handle error response from API."
  (let* ((parsed (ignore-errors (parse-response response)))
         (code (when parsed (gethash "code" parsed)))
         (message (when parsed (gethash "message" parsed))))
    (error 'logs-api-error
           :code (or code status-code)
           :message (or message response))))


(-> api-get (string &key (:params list))
    (values hash-table &optional))

(defun api-get (path &key params)
  "Make GET request to Logs API.
   PATH is the API endpoint path (e.g., \"/logrequests\").
   PARAMS is an alist of query parameters."
  (check-credentials)

  (let* ((base-url (build-url path))
         (url (if params
                  #?"${base-url}?${(build-query-string params)}"
                  base-url)))
    (multiple-value-bind (body status-code)
        (dex:get url :headers (make-auth-headers))
      (if (and (>= status-code 200) (< status-code 300))
          (parse-response body)
          (handle-error-response body status-code)))))


(-> api-post (string &key (:params list) (:content t))
    (values hash-table &optional))

(defun api-post (path &key params content)
  "Make POST request to Logs API.
   PATH is the API endpoint path.
   PARAMS is an alist of query parameters.
   CONTENT is the request body (will be JSON-encoded if provided as hash-table)."
  (check-credentials)

  (let* ((base-url (build-url path))
         (url (if params
                  #?"${base-url}?${(build-query-string params)}"
                  base-url)))
    (multiple-value-bind (body status-code)
        (dex:post url
                  :headers (make-auth-headers)
                  :content content)
      (if (and (>= status-code 200) (< status-code 300))
          (parse-response body)
          (handle-error-response body status-code)))))


(-> format-date ((or string timestamp)) string)
(defun format-date (date)
  "Format a date for the API (YYYY-MM-DD format).
   DATE can be a local-time:timestamp or a string."
  (etypecase date
    (string date)
    (timestamp (format-timestring nil date
                                  :format '(:year "-" (:month 2) "-" (:day 2))))))
