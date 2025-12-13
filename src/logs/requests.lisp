(uiop:define-package #:yandex-metrika/logs/requests
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:local-time
                #:timestamp
                #:today
                #:timestamp<
                #:timestamp<=
                #:parse-timestring
                #:adjust-timestamp)
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
           #:request-parts
           ;; Date utilities
           #:n-days-ago
           #:yesterday))
(in-package #:yandex-metrika/logs/requests)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-interpol:enable-interpol-syntax))


(deftype source-type ()
  "Valid source types for log requests."
  '(member :visits :hits))


(-> source-to-string (source-type) string)

(defun source-to-string (source)
  "Convert source keyword to API string."
  (ecase source
    (:visits "visits")
    (:hits "hits")))


(-> to-timestamp ((or string timestamp)) timestamp)

(defun to-timestamp (date)
  "Convert a date to a timestamp for comparison."
  (etypecase date
    (timestamp date)
    (string (parse-timestring date))))


(-> n-days-ago ((integer 0)) timestamp)

(defun n-days-ago (n)
  "Return a timestamp for N days ago from today.
   N must be a non-negative integer."
  (adjust-timestamp (today) (:offset :day (- n))))


(-> yesterday () timestamp)

(defun yesterday ()
  "Return a timestamp for yesterday.
   Useful as TO-DATE since today's data is incomplete."
  (n-days-ago 1))


(-> validate-dates ((or string timestamp) (or string timestamp))
    (values timestamp timestamp &optional))

(defun validate-dates (from-date to-date)
  "Validate date range for log requests.
   Checks that:
   - FROM-DATE <= TO-DATE
   - TO-DATE is not today (current day data is incomplete)
   Returns parsed timestamps as (VALUES from-timestamp to-timestamp)."
  (let ((from-ts (to-timestamp from-date))
        (to-ts (to-timestamp to-date))
        (today-ts (today)))
    (unless (timestamp<= from-ts to-ts)
      (error "FROM-DATE (~A) must be less than or equal to TO-DATE (~A)"
             (format-date from-date) (format-date to-date)))
    (unless (timestamp< to-ts today-ts)
      (error "TO-DATE (~A) cannot be today or in the future. ~
              Current day data is incomplete."
             (format-date to-date)))
    (values from-ts to-ts)))


(-> evaluate-request (source-type
                      (or string timestamp)
                      (or string timestamp)
                      (or string list))
    hash-table)

(defun evaluate-request (source from-date to-date fields)
  "Evaluate possibility of creating a log request.
   SOURCE is either :visits or :hits.
   FROM-DATE and TO-DATE define the date range (can be timestamps or strings).
   FIELDS is a list of field names to retrieve.
   Returns a hash-table with 'log_request_evaluation' data including
   'possible' (boolean) and 'max_possible_day_quantity'."
  (multiple-value-bind (from-ts to-ts) (validate-dates from-date to-date)
    (let ((fields-str (if (listp fields)
                          (format nil "~{~A~^,~}" fields)
                          fields)))
      (api-get "/logrequests/evaluate"
               :params `(("date1" . ,(format-date from-ts))
                         ("date2" . ,(format-date to-ts))
                         ("source" . ,(source-to-string source))
                         ("fields" . ,fields-str))))))


(-> create-request (source-type
                    (or string timestamp)
                    (or string timestamp)
                    (or string list))
    hash-table)

(defun create-request (source from-date to-date fields)
  "Create a new log request.
   SOURCE is either :visits or :hits.
   FROM-DATE and TO-DATE define the date range (can be timestamps or strings).
   FIELDS is a list of field names to retrieve.
   Returns a hash-table with 'log_request' data including 'request_id'."
  (multiple-value-bind (from-ts to-ts) (validate-dates from-date to-date)
    (let ((fields-str (if (listp fields)
                          (format nil "~{~A~^,~}" fields)
                          fields)))
      (api-post "/logrequests"
                :params `(("date1" . ,(format-date from-ts))
                          ("date2" . ,(format-date to-ts))
                          ("source" . ,(source-to-string source))
                          ("fields" . ,fields-str))))))


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
