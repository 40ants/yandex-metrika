(defsystem "yandex-metrika-docs"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/yandex-metrika"
  :class :package-inferred-system
  :description "Provides documentation for yandex-metrika."
  :source-control (:git "https://github.com/40ants/yandex-metrika")
  :bug-tracker "https://github.com/40ants/yandex-metrika/issues"
  :pathname "docs"
  :depends-on ("yandex-metrika"
               "yandex-metrika-docs/index"))
