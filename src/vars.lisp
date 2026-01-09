(uiop:define-package #:yandex-metrika/vars
  (:use #:cl)
  (:export #:*counter*
           #:*token*))
(in-package #:yandex-metrika/vars)


(defvar *counter* nil
  "Yandex Metrika's counter id.")


(defvar *token* nil
  "Yandex Metrika's oauth token.

   Use this link to get dev token https://oauth.yandex.ru/authorize?response_type=token&client_id=b7e202e6bbd548eb9f0e25dd94eaf6a4 or create your own oauth client application.")
