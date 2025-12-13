(uiop:define-package #:yandex-metrika/logs/requests
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:local-time
                #:timestamp)
  (:import-from #:yandex-metrika/logs/api
                #:api-get
                #:api-post
                #:format-date)
  (:export #:evaluate-request
           #:create-request
           #:get-request
           #:list-requests
           #:clean-request
           #:request-status
           #:request-parts))
(in-package #:yandex-metrika/logs/requests)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-interpol:enable-interpol-syntax))


(-> evaluate-request (string (or string timestamp) (or string timestamp) (or string list)) hash-table)
(defun evaluate-request (source date1 date2 fields)
  "Evaluate possibility of creating a log request.
   SOURCE is either \"visits\" or \"hits\".
   DATE1 and DATE2 define the date range (can be timestamps or strings).
   FIELDS is a list of field names to retrieve.
   Returns a hash-table with 'log_request_evaluation' data including
   'possible' (boolean) and 'max_possible_day_quantity'."
  (let ((fields-str (if (listp fields)
                        (format nil "~{~A~^,~}" fields)
                        fields)))
    (api-get "/logrequests/evaluate"
             :params `(("date1" . ,(format-date date1))
                       ("date2" . ,(format-date date2))
                       ("source" . ,source)
                       ("fields" . ,fields-str)))))


(-> create-request (string (or string timestamp) (or string timestamp) (or string list)) hash-table)
(defun create-request (source date1 date2 fields)
  "Create a new log request.
   SOURCE is either \"visits\" or \"hits\".
   DATE1 and DATE2 define the date range (can be timestamps or strings).
   FIELDS is a list of field names to retrieve.
   Returns a hash-table with 'log_request' data including 'request_id'."
  (let ((fields-str (if (listp fields)
                        (format nil "~{~A~^,~}" fields)
                        fields)))
    (api-post "/logrequests"
              :params `(("date1" . ,(format-date date1))
                        ("date2" . ,(format-date date2))
                        ("source" . ,source)
                        ("fields" . ,fields-str)))))


(-> get-request (integer) hash-table)
(defun get-request (request-id)
  "Get information about a specific log request.
   REQUEST-ID is the ID of the log request.
   Returns a hash-table with 'log_request' data including 'status' and 'parts'."
  (api-get #?"/logrequests/${request-id}"))


(-> list-requests () hash-table)
(defun list-requests ()
  "List all log requests for the current counter.
   Returns a hash-table with 'requests' array."
  (api-get "/logrequests"))


(-> clean-request (integer) hash-table)
(defun clean-request (request-id)
  "Delete (clean) a log request to free up quota.
   REQUEST-ID is the ID of the log request to delete.
   Returns a hash-table with 'log_request' data."
  (api-post #?"/logrequests/${request-id}/clean"))


(-> request-status (integer) string)
(defun request-status (request-id)
  "Get the status of a log request.
   Returns one of: \"created\", \"processed\", \"canceled\", \"processing_failed\",
   \"cleaned_by_user\", \"cleaned_automatically_as_too_old\"."
  (let* ((response (get-request request-id))
         (log-request (gethash "log_request" response)))
    (gethash "status" log-request)))


(-> request-parts (integer) (or null list))
(defun request-parts (request-id)
  "Get the list of parts for a processed log request.
   Returns a list of part numbers available for download, or NIL if not ready."
  (let* ((response (get-request request-id))
         (log-request (gethash "log_request" response))
         (parts (gethash "parts" log-request)))
    (when parts
      (loop for part across parts
            collect (gethash "part_number" part)))))
