(uiop:define-package #:yandex-metrika/logs/download
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:import-from #:lisp-stat
                #:string-to-keyword)
  (:import-from #:yandex-metrika/vars
                #:*counter*
                #:*token*)
  (:import-from #:yandex-metrika/logs/api
                #:*api-base-url*
                #:logs-api-error)
  (:import-from #:yandex-metrika/logs/requests
                #:log-request-source
                #:log-request
                #:log-request-id
                #:log-request-status
                #:log-request-parts
                #:get-request)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:yandex-metrika/auth
                #:with-auth-headers)
  (:export #:download-part
           #:download-all-parts))
(in-package #:yandex-metrika/logs/download)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-interpol:enable-interpol-syntax))


(-> build-download-url (integer integer) string)
(defun build-download-url (request-id part-number)
  "Build URL for downloading a specific part."
  #?"${*api-base-url*}/${*counter*}/logrequest/${request-id}/part/${part-number}/download")


(defun make-keyword-factory (prefix-to-remove)
  (flet ((make-field-name (name)
           (let* ((without-prefix (if (str:starts-with-p prefix-to-remove name)
                                    (subseq name (length prefix-to-remove))
                                    name))
                  (as-param (str:param-case without-prefix)))
             (make-keyword
              (string-upcase as-param)))))
    #'make-field-name))


(-> download-part (log-request integer)
    lisp-stat:data-frame)

(defun download-part (request part-number)
  "Download a specific part of a log request.
   REQUEST is a LOG-REQUEST object.
   PART-NUMBER is the part number to download.
   Returns the raw TSV data as a string."
  (let ((url (build-download-url (log-request-id request) part-number)))
    (with-auth-headers (headers)
        (multiple-value-bind (response status-code)
            (dex:get url :headers headers)
          (cond
            ((and (>= status-code 200)
                  (< status-code 300))
             (let ((fare-csv:*separator* #\Tab))
               (lisp-stat:read-csv response
                                   :column-keys-or-function (case (log-request-source request)
                                                              (:hits (make-keyword-factory "ym:pv:"))
                                                              (:visits (make-keyword-factory "ym:s:"))
                                                              (t #'string-to-keyword))
                                   :map-alist '(("" . nil)
                                                ("NA" . nil)))))
            (t
             (error 'logs-api-error
                    :code status-code
                    :message response)))))))


(defun join-dataframes (df1 df2)
  (lisp-stat:matrix-df
   (lisp-stat:keys df1)
   (lisp-stat:stack-rows df1 df2)))


(-> download-all-parts (log-request)
    lisp-stat:data-frame)

(defun download-all-parts (request)
  "Download all parts of a processed log request.
   REQUEST is a LOG-REQUEST object.
   Returns a list of TSV data strings, one per part."
  (let ((parts (log-request-parts request)))
    (unless parts
      (error "Request ~A has no parts available for download"
             (log-request-id request)))
    (loop with result = nil
          for part-number in parts
          for new-df = (download-part request part-number)
          do (setf result
                   (if result
                     (join-dataframes result new-df)
                     new-df))
          finally (return result))))
