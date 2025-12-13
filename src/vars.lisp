(uiop:define-package #:yandex-metrika/vars
  (:use #:cl)
  (:export #:*counter*))
(in-package #:yandex-metrika/vars)


(defvar *counter* nil
  "Yandex Metrika's counter id.")
