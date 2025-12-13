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
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:str
                #:replace-all)
  (:export #:evaluate-request
           #:create-request
           #:get-request
           #:list-requests
           #:clean-request
           #:request-status
           #:request-parts
           ;; Date utilities
           #:n-days-ago
           #:yesterday
           ;; Log request class
           #:log-request
           #:log-request-id
           #:log-request-counter-id
           #:log-request-source
           #:log-request-from-date
           #:log-request-to-date
           #:log-request-fields
           #:log-request-status
           #:log-request-size
           #:log-request-attribution
           #:log-request-parts))
(in-package #:yandex-metrika/logs/requests)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-interpol:enable-interpol-syntax))


(defparameter +valid-sources+ '(:visits :hits)
  "List of valid source types.")

(deftype source-type ()
  "Valid source types for log requests."
  `(member ,@+valid-sources+
           :unknown))


(defparameter +valid-attributions+
  '(:first
    :last
    :lastsign
    :last-yandex-direct-click
    :cross-device-last-significant
    :cross-device-first
    :cross-device-last-yandex-direct-click
    :cross-device-last
    :automatic)
  "List of valid attribution models.")


(deftype attribution-type ()
  "Valid attribution models for log requests.
   See: https://yandex.ru/dev/metrika/ru/logs/openapi/createLogRequest"
  `(member ,@+valid-attributions+
           :unknown))


(defparameter +valid-statuses+
  '(:created
    :canceled
    :processed
    :cleaned-by-user
    :cleaned-automatically-as-too-old
    :processing-failed
    :awaiting-retry)
  "List of valid status values.")


(deftype status-type ()
  "Valid status values for log requests."
  `(member ,@+valid-statuses+
           :unknown))


(-> source-to-string (source-type) string)

(defun source-to-string (source)
  "Convert source keyword to API string."
  (string-downcase (symbol-name source)))


(-> string-to-source (string) source-type)

(defun string-to-source (string)
  "Convert API string to source keyword."
  (let ((keyword (make-keyword (string-upcase
                                (replace-all "_" "-" string)))))
    (if (member keyword +valid-sources+)
        keyword
        (progn
          (log:warn "Unknown value for source: ~A" string)
          :unknown))))


(-> attribution-to-string (attribution-type) string)

(defun attribution-to-string (attribution)
  "Convert attribution keyword to API string."
  (replace-all "-" "_" (symbol-name attribution)))


(-> string-to-attribution (string) attribution-type)

(defun string-to-attribution (string)
  "Convert API string to attribution keyword."
  (let ((keyword (make-keyword (string-upcase
                                (replace-all "_" "-" string)))))
    (if (member keyword +valid-attributions+)
        keyword
        (progn
          (log:warn "Unknown value for attribution: ~A" string)
          :unknown))))


(-> string-to-status (string) status-type)

(defun string-to-status (string)
  "Convert API string to status keyword."
  (let ((keyword (make-keyword (string-upcase
                                (replace-all "_" "-" string)))))
    (if (member keyword +valid-statuses+)
        keyword
        (progn
          (log:warn "Unknown value for status: ~A" string)
          :unknown))))


(defclass log-request ()
  ((request-id :initarg :request-id
               :reader log-request-id
               :type integer
               :documentation "Unique identifier of the log request.")
   (counter-id :initarg :counter-id
               :reader log-request-counter-id
               :type integer
               :documentation "Counter ID this request belongs to.")
   (source :initarg :source
           :reader log-request-source
           :type source-type
           :documentation "Data source: :visits or :hits.")
   (from-date :initarg :from-date
              :reader log-request-from-date
              :type timestamp
              :documentation "Start date of the request period.")
   (to-date :initarg :to-date
            :reader log-request-to-date
            :type timestamp
            :documentation "End date of the request period.")
   (fields :initarg :fields
           :reader log-request-fields
           :type list
           :documentation "List of requested fields.")
   (status :initarg :status
           :reader log-request-status
           :type status-type
           :documentation "Request status: :created, :processed, :canceled, etc.")
   (size :initarg :size
         :reader log-request-size
         :type integer
         :documentation "Size of the prepared data in bytes.")
   (attribution :initarg :attribution
                :reader log-request-attribution
                :type attribution-type
                :documentation "Attribution model used.")
   (parts :initarg :parts
          :reader log-request-parts
          :initform nil
          :type list
          :documentation "List of available parts for download."))
  (:documentation "Represents a Yandex Metrika Logs API request."))


(defmethod print-object ((obj log-request) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A ~A..~A ~A"
            (log-request-id obj)
            (log-request-source obj)
            (format-date (log-request-from-date obj))
            (format-date (log-request-to-date obj))
            (log-request-status obj))))


(-> parse-log-request (hash-table) log-request)

(defun parse-log-request (data)
  "Parse a hash-table into a LOG-REQUEST object."
  (let ((fields-raw (gethash "fields" data))
        (parts-raw (gethash "parts" data)))
    (make-instance 'log-request
                   :request-id (gethash "request_id" data)
                   :counter-id (gethash "counter_id" data)
                   :source (string-to-source (gethash "source" data))
                   :from-date (parse-timestring (gethash "date1" data))
                   :to-date (parse-timestring (gethash "date2" data))
                   :fields (if (stringp fields-raw)
                               (uiop:split-string fields-raw :separator '(#\,))
                               (coerce fields-raw 'list))
                   :status (string-to-status (gethash "status" data))
                   :size (gethash "size" data)
                   :attribution (string-to-attribution (gethash "attribution" data))
                   :parts (when parts-raw
                            (loop for part across parts-raw
                                  collect (gethash "part_number" part))))))


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
    (values boolean integer integer integer integer))

(defun evaluate-request (source from-date to-date fields)
  "Evaluate possibility of creating a log request.
   SOURCE is either :visits or :hits.
   FROM-DATE and TO-DATE define the date range (can be timestamps or strings).
   FIELDS is a list of field names to retrieve.
   Returns (VALUES possible
                   max-possible-day-quantity
                   expected-size
                   log-request-sum-max-size
                   log-request-sum-size)."
  (multiple-value-bind (from-ts to-ts) (validate-dates from-date to-date)
    (let ((fields-str (if (listp fields)
                        (format nil "~{~A~^,~}" fields)
                        fields)))
      (let* ((response (api-get "/logrequests/evaluate"
                                :params `(("date1" . ,(format-date from-ts))
                                          ("date2" . ,(format-date to-ts))
                                          ("source" . ,(source-to-string source))
                                          ("fields" . ,fields-str))))
             (eval-data (gethash "log_request_evaluation" response)))
        (values (gethash "possible" eval-data)
                (gethash "max_possible_day_quantity" eval-data)
                (gethash "expected_size" eval-data)
                (gethash "log_request_sum_max_size" eval-data)
                (gethash "log_request_sum_size" eval-data))))))


(-> create-request (source-type
                    (or string timestamp)
                    (or string timestamp)
                    (or string list)
                    &key (:attribution (or null attribution-type)))
    log-request)

(defun create-request (source from-date to-date fields &key attribution)
  "Create a new log request.
   SOURCE is either :visits or :hits.
   FROM-DATE and TO-DATE define the date range (can be timestamps or strings).
   FIELDS is a list of field names to retrieve.
   ATTRIBUTION is the attribution model (optional), one of:
     :first :last :lastsign :last-yandex-direct-click
     :cross-device-last-significant :cross-device-first
     :cross-device-last-yandex-direct-click :cross-device-last :automatic
   Returns a LOG-REQUEST object."
  (multiple-value-bind (from-ts to-ts)
      (validate-dates from-date to-date)
    (let* ((fields-str (if (listp fields)
                           (format nil "~{~A~^,~}" fields)
                           fields))
           (params `(("date1" . ,(format-date from-ts))
                     ("date2" . ,(format-date to-ts))
                     ("source" . ,(source-to-string source))
                     ("fields" . ,fields-str))))
      (when attribution
        (push `("attribution" . ,(attribution-to-string attribution)) params))
      (let* ((response (api-post "/logrequests" :params params))
             (log-request-data (gethash "log_request" response)))
        (parse-log-request log-request-data)))))


(-> get-request (integer) hash-table)

(defun get-request (request-id)
  "Get information about a specific log request.
   REQUEST-ID is the ID of the log request.
   Returns a hash-table with 'log_request' data including 'status' and 'parts'."
  (api-get #?"/logrequests/${request-id}"))


(-> list-requests () list)

(defun list-requests ()
  "List all log requests for the current counter.
   Returns a list of LOG-REQUEST objects."
  (let* ((response (api-get "/logrequests"))
         (requests (gethash "requests" response)))
    (map 'list #'parse-log-request requests)))


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
