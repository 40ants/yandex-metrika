(defsystem "yandex-metrika-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/yandex-metrika"
  :class :package-inferred-system
  :description "Provides CI settings for yandex-metrika."
  :source-control (:git "https://github.com/40ants/yandex-metrika")
  :bug-tracker "https://github.com/40ants/yandex-metrika/issues"
  :pathname "src"
  :depends-on ("40ants-ci"
               "yandex-metrika-ci/ci"))
