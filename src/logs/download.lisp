(uiop:define-package #:yandex-metrika/logs/download
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:yandex-metrika/vars
                #:*counter*
                #:*token*)
  (:import-from #:yandex-metrika/logs/api
                #:*api-base-url*
                #:logs-api-error)
  (:import-from #:yandex-metrika/logs/requests
                #:log-request
                #:log-request-id
                #:log-request-status
                #:log-request-parts
                #:get-request)
  (:export #:download-part
           #:download-all-parts
           #:wait-for-request
           #:parse-tsv-line
           #:parse-tsv))
(in-package #:yandex-metrika/logs/download)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-interpol:enable-interpol-syntax))


(-> make-auth-headers () list)
(defun make-auth-headers ()
  "Create authorization headers for API requests."
  `(("Authorization" . ,#?"OAuth ${*token*}")))


(-> build-download-url (integer integer) string)
(defun build-download-url (request-id part-number)
  "Build URL for downloading a specific part."
  #?"${*api-base-url*}/${*counter*}/logrequest/${request-id}/part/${part-number}/download")


(-> download-part (log-request integer) string)
(defun download-part (request part-number)
  "Download a specific part of a log request.
   REQUEST is a LOG-REQUEST object.
   PART-NUMBER is the part number to download.
   Returns the raw TSV data as a string."
  (let ((url (build-download-url (log-request-id request) part-number)))
    (multiple-value-bind (body status-code)
        (dex:get url :headers (make-auth-headers))
      (if (and (>= status-code 200) (< status-code 300))
          body
          (error 'logs-api-error
                 :code status-code
                 :message body)))))


(-> download-all-parts (log-request) list)
(defun download-all-parts (request)
  "Download all parts of a processed log request.
   REQUEST is a LOG-REQUEST object.
   Returns a list of TSV data strings, one per part."
  (let ((parts (log-request-parts request)))
    (unless parts
      (error "Request ~A has no parts available for download"
             (log-request-id request)))
    (loop for part-number in parts
          collect (download-part request part-number))))


(-> wait-for-request (log-request &key (:interval integer) (:timeout integer)) boolean)
(defun wait-for-request (request &key (interval 10) (timeout 3600))
  "Wait for a log request to be processed.
   REQUEST is a LOG-REQUEST object.
   INTERVAL is the polling interval in seconds (default 10).
   TIMEOUT is the maximum wait time in seconds (default 3600 = 1 hour).
   Returns T if the request is processed, NIL if timeout or failed."
  (let ((start-time (get-universal-time))
        (request-id (log-request-id request)))
    (loop
      (let ((status (log-request-status (get-request request-id))))
        (cond
          ((eq status :processed)
           (return t))
          ((member status '(:canceled :processing-failed
                            :cleaned-by-user :cleaned-automatically-as-too-old))
           (return nil))
          ((> (- (get-universal-time) start-time) timeout)
           (return nil))
          (t
           (sleep interval)))))))


(-> parse-tsv-line (string) list)
(defun parse-tsv-line (line)
  "Parse a single TSV line into a list of values."
  (let ((values nil)
        (current (make-string-output-stream))
        (len (length line)))
    (loop for i from 0 below len
          for char = (char line i)
          do (if (char= char #\Tab)
                 (progn
                   (push (get-output-stream-string current) values)
                   (setf current (make-string-output-stream)))
                 (write-char char current)))
    (push (get-output-stream-string current) values)
    (nreverse values)))


(-> parse-tsv (string &key (:header boolean)) (values list (or null list)))
(defun parse-tsv (data &key (header t))
  "Parse TSV data into a list of rows.
   DATA is the raw TSV string.
   If HEADER is T (default), the first row is returned separately as headers.
   Returns (VALUES rows headers) where rows is a list of lists,
   and headers is the first row if HEADER is T, or NIL otherwise."
  (let* ((lines (remove-if (lambda (line) (string= line ""))
                           (uiop:split-string data :separator '(#\Newline))))
         (parsed-lines (mapcar #'parse-tsv-line lines)))
    (if (and header parsed-lines)
        (values (rest parsed-lines) (first parsed-lines))
        (values parsed-lines nil))))
