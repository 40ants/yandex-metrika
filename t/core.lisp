(uiop:define-package #:yandex-metrika-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:yandex-metrika-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))
