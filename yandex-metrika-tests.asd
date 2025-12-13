(defsystem "yandex-metrika-tests"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/yandex-metrika"
  :class :package-inferred-system
  :description "Provides tests for yandex-metrika."
  :source-control (:git "https://github.com/40ants/yandex-metrika")
  :bug-tracker "https://github.com/40ants/yandex-metrika/issues"
  :pathname "t"
  :depends-on ("yandex-metrika-tests/core")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
