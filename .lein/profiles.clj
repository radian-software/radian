{:user {:plugins [;; Linters
                  [jonase/eastwood "0.2.3"]
                  [lein-ancient "0.6.10"]
                  [lein-bikeshed "0.3.0"]
                  [lein-cljfmt "0.5.3"]
                  [lein-kibit "0.1.2"]
                  [venantius/yagni "0.1.4"]

                  ;; Documentation generators
                  [lein-codox "0.9.5"]]

        ;; Prevent the Clojure REPL from showing in the Mac app switcher.
        ;; See http://stackoverflow.com/questions/24619300/hide-clojure-repl-from-command-tab-application-switcher-via-lein-command-line
        :jvm-opts ["-Dapple.awt.UIElement=true"]}}
