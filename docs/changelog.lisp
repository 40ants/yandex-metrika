(uiop:define-package #:yandex-metrika-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package #:yandex-metrika-docs/changelog)


(defchangelog (:ignore-words ("SLY"
                              "ASDF"
                              "REPL"
                              "HTTP"))
  (0.2.0 2026-01-09
         "* Now library keeps auth token secret in case if some error happens and traceback logged.")
  (0.1.0 2023-02-05
         "* Initial version."))
