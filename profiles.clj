{;;; Modular profiles

 :cider {:dependencies [;; cider-nrepl 0.14.0+ requires Clojure 1.7.0+
                        ;; and tools.nrepl 0.2.12+ to work. These are
                        ;; used by default, but projects might want to
                        ;; override them. If we're using CIDER though,
                        ;; we don't want to let them do that.
                        [org.clojure/clojure "1.8.0"]
                        [org.clojure/tools.nrepl "0.2.12"]]

         :plugins [;; REPL-side support for CIDER and other editor tools
                   [cider/cider-nrepl "0.15.0-SNAPSHOT"]]}

 :cljs {:dependencies [;; ClojureScript REPL in Emacs
                       [com.cemerick/piggieback "0.2.1"]
                       [figwheel-sidecar "0.5.4-7"]]}

 :hide-app {;; Prevent the Clojure REPL from showing in the Mac app switcher.
            ;; See http://stackoverflow.com/questions/24619300/hide-clojure-repl-from-command-tab-application-switcher-via-lein-command-line
            :jvm-opts ["-Dapple.awt.UIElement=true"]}

 :humane {:dependencies [;; Better-formatted clojure.test output
                         [pjstadig/humane-test-output "0.8.0"]]

          :injections [;; Activate better-formatted clojure.test output
                       (require 'pjstadig.humane-test-output)
                       (pjstadig.humane-test-output/activate!)]}

 :inject {:dependencies [;; Inject utility functions into convenient namespaces
                         ;; (by default, the '.' namespace)
                         [im.chit/vinyasa.inject "0.4.7"]]

          :injections [;; The following injections do not incur any additional startup
                       ;; cost -- the clojure.* namespaces are loaded by default when
                       ;; the REPL starts up, and vinyasa.inject is obviously already
                       ;; being loaded because that is the whole point of this profile.
                       ;; So, unlike the other injections, these ones are not split out
                       ;; into different profiles.
                       (require 'vinyasa.inject)
                       (vinyasa.inject/in
                         [clojure.core [refer-clojure rc]]
                         [clojure.java.shell sh]
                         [clojure.pprint pp pprint]
                         [clojure.repl doc pst source]
                         [vinyasa.inject [in inject]])]}

 ;; BEGIN profiles with optional injections
 ;; Put these *after* the :inject profile if you want the injections.

 :alembic {:dependencies [;; Pull dependencies from Clojars in the REPL,
                          ;; and invoke Leiningen tasks from the REPL
                          [alembic "0.3.2"]]

           :injections [;; Modify the alembic/distill function to work without needing a
                        ;; project.clj file. This is a hack! It works simply by
                        ;; telling alembic not to look for repositories in the project.clj.
                        ;; However, this is a global override, so if you need to use one
                        ;; of the repositories in the project.clj, you must either pass
                        ;; the :repositories key explicitly, or pass the keyword :project
                        ;; to suppress this hack and allow alembic to search project.clj
                        ;; for repositories.
                        (require 'alembic.still)
                        (alter-var-root
                          #'alembic.still/distill
                          (fn [distill]
                            (fn [dependencies & args]
                              (if (= (first args) :project)
                                (apply distill dependencies (rest args))
                                (apply distill dependencies :repositories
                                       [["central"
                                         {:snapshots false
                                          :url "https://repo1.maven.org/maven2/"}]
                                        ["clojars"
                                         {:url "https://clojars.org/repo/"}]]
                                       args)))))

                        (when-let [inject (resolve 'vinyasa.inject/inject)]
                          ;; We have to use extra brackets because of a bug
                          ;; in vinyasa. See https://github.com/zcaudate/vinyasa/pull/31
                          (inject '[[alembic.still distill lein load-project]]))]}

 :pull {:dependencies [;; Pull dependencies from Clojars in the REPL
                       [im.chit/vinyasa.maven "0.4.7"]]

        :injections [(when-let [inject (resolve 'vinyasa.inject/inject)]
                       ;; We have to use extra brackets because of a bug
                       ;; in vinyasa. See https://github.com/zcaudate/vinyasa/pull/31
                       (inject '[[vinyasa.maven pull]]))]}

 :reflection {:dependencies [;; Convenient reflection functions
                             [im.chit/vinyasa.reflection "0.4.7"]]

              :injections [(when-let [inject (resolve 'vinyasa.inject/inject)]
                             ;; We have to use extra brackets because of a bug
                             ;; in vinyasa. See https://github.com/zcaudate/vinyasa/pull/31
                             (inject '[clojure.core
                                       [vinyasa.reflection .& .> .? .* .% .%>]]))]}

 :refresh {:dependencies [;; Properly refresh a dirty namespace
                          [org.clojure/tools.namespace "0.2.11"]]

           :injections [(when-let [inject (resolve 'vinyasa.inject/inject)]
                          ;; We have to use extra brackets because of a bug
                          ;; in vinyasa. See https://github.com/zcaudate/vinyasa/pull/31
                          (inject '[[clojure.tools.namespace.repl refresh refresh-all]]))]}

 ;; END profiles with optional injections

 :lint {:plugins [;; Miscellaneous linting
                  [jonase/eastwood "0.2.3"]

                  ;; Check for outdated dependencies and plugins
                  [lein-ancient "0.6.10"]

                  ;; Basic linting
                  [lein-bikeshed "0.3.0"]

                  ;; Style linting
                  [lein-cljfmt "0.5.3"]

                  ;; Miscellaneous linting
                  [lein-kibit "0.1.2"]

                  ;; Check for unused functions
                  [venantius/yagni "0.1.4"]]}

 :pretty {:dependencies [;; Pretty stack traces
                         [io.aviso/pretty "0.1.29"]]

          :injections [;; Use pretty stack traces for pst. CIDER has even better stack trace
                       ;; functionality by default, so we won't make the REPL print a full
                       ;; stack trace on encountering an exception. (This would be done by
                       ;; overriding clojure.main/repl-caught.)
                       (require 'clojure.repl 'io.aviso.repl)
                       (alter-var-root
                         #'clojure.repl/pst
                         (constantly (fn [& [e & _]]
                                       (if (instance? Throwable e)
                                         (io.aviso.repl/pretty-pst e)
                                         (io.aviso.repl/pretty-pst)))))]}

 :refactor {:dependencies [;; refactor-nrepl requires Clojure 1.7.0+ to work
                           [org.clojure/clojure "1.8.0"]]

            :plugins [;; REPL-side support for clj-refactor
                      [refactor-nrepl "2.3.0-SNAPSHOT"]]}

 :spyscope {:dependencies [;; Quick-and-dirty debugging tools
                           [spyscope "0.1.5"]]

            :injections [;; Make the spyscope reader macros available.
                         (require 'spyscope.core)]}

 ;;; Temporary hacks

 :hacks {}

 ;;; Composite profiles

 ;; This profile includes useful profiles for a REPL in the terminal.
 :awesome [:cljs
           :humane
           :inject
           :alembic
           :pretty
           :refactor
           :refresh]

 ;; This profile is for use with Emacs.
 :emacs [:cider
         :cljs
         :inject
         :alembic
         :refactor]

 ;; When running tests.
 :test [:humane]

 ;; Always, except when building uberjar.
 :user [:hide-app :lint :hacks]}
