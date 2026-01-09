#-asdf3.1 (error "yandex-metrika requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "yandex-metrika"
  :description "A client
library for metrika.yandex.ru"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :homepage "https://40ants.com/yandex-metrika"
  :source-control (:git "https://github.com/40ants/yandex-metrika")
  :bug-tracker "https://github.com/40ants/yandex-metrika/issues"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("log4cl-extras"
               "yandex-metrika/client"
               "yandex-metrika/logs/api"
               "yandex-metrika/logs/requests"
               "yandex-metrika/logs/download"
               "yandex-metrika/logs/fields")
  :in-order-to ((test-op (test-op "yandex-metrika-tests"))))


(asdf:register-system-packages "dexador" '("DEX"))
