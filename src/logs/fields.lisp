(uiop:define-package #:yandex-metrika/logs/fields
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:export ;; Common visits (sessions) fields
           #:+visits-basic-fields+
           #:+visits-traffic-fields+
           #:+visits-device-fields+
           #:+visits-geo-fields+
           #:+visits-goal-fields+
           ;; Common hits (events) fields
           #:+hits-basic-fields+
           #:+hits-page-fields+
           #:+hits-device-fields+
           ;; Field helpers
           #:make-fields-string
           #:+hits-utm-fields+
           #:+hits-goal-fields+))
(in-package #:yandex-metrika/logs/fields)


;;; Common visits (sessions) fields - prefix ym:s:

(defparameter +visits-basic-fields+
  '("ym:s:counterID"
    "ym:s:dateTime"
    "ym:s:date"
    "ym:s:visitID"
    "ym:s:clientID"
    "ym:s:watchIDs"
    "ym:s:isNewUser"
    "ym:s:startURL"
    "ym:s:pageViews"
    "ym:s:visitDuration")
  "Basic visits fields for session identification and metrics.")


(defparameter +visits-traffic-fields+
  '("ym:s:trafficSource"
    "ym:s:lastTrafficSource"
    "ym:s:referer"
    "ym:s:searchEngineRoot"
    "ym:s:searchEngine"
    "ym:s:searchPhrase"
    "ym:s:UTMCampaign"
    "ym:s:UTMContent"
    "ym:s:UTMMedium"
    "ym:s:UTMSource"
    "ym:s:UTMTerm")
  "Traffic source related visits fields.")


(defparameter +visits-device-fields+
  '("ym:s:deviceCategory"
    "ym:s:mobilePhone"
    "ym:s:mobilePhoneModel"
    "ym:s:operatingSystem"
    "ym:s:operatingSystemRoot"
    "ym:s:browser"
    "ym:s:browserLanguage"
    "ym:s:screenWidth"
    "ym:s:screenHeight"
    "ym:s:screenColors")
  "Device and browser related visits fields.")


(defparameter +visits-geo-fields+
  '("ym:s:regionCountry"
    "ym:s:regionCity"
    "ym:s:regionCountryID"
    "ym:s:regionCityID"
    "ym:s:ipAddress")
  "Geographic related visits fields.")


(defparameter +visits-goal-fields+
  '("ym:s:goalsID"
    "ym:s:goalsSerialNumber"
    "ym:s:goalsDateTime"
    "ym:s:goalsPrice"
    "ym:s:goalsOrder")
  "Goal/conversion related visits fields.")


;;; Common hits (events) fields - prefix ym:pv:
;;; From https://yandex.ru/dev/metrika/ru/logs/fields/hits

(defparameter +hits-basic-fields+
  '("ym:pv:counterID"
    "ym:pv:dateTime"
    "ym:pv:date"
    "ym:pv:watchID"
    "ym:pv:clientID"
    "ym:pv:counterUserIDHash")
  "Basic hits fields for event identification.")


(defparameter +hits-page-fields+
  '("ym:pv:URL"
    "ym:pv:title"
    "ym:pv:referer")
  "Page and URL related hits fields.")

(defparameter +hits-utm-fields+
  '("ym:pv:UTMCampaign"
    "ym:pv:UTMContent"
    "ym:pv:UTMMedium"
    "ym:pv:UTMSource"
    "ym:pv:UTMTerm")
  "UTM related fields.")


(defparameter +hits-goal-fields+
  '("ym:pv:goalsID")
  "Goal/conversion related hits fields.")


(defparameter +hits-device-fields+
  '("ym:pv:deviceCategory"
    "ym:pv:operatingSystem"
    "ym:pv:operatingSystemRoot"
    "ym:pv:browser"
    "ym:pv:browserMajorVersion"
    "ym:pv:browserMinorVersion"
    "ym:pv:screenWidth"
    "ym:pv:screenHeight")
  "Device and browser related hits fields.")


;;; Field helpers

(-> make-fields-string ((or string list)) string)
(defun make-fields-string (fields)
  "Convert a list of field names to a comma-separated string.
   FIELDS can be a list of strings or a single string.
   Returns the fields as a comma-separated string suitable for API calls."
  (etypecase fields
    (string fields)
    (list (format nil "~{~A~^,~}" fields))))
