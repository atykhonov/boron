(require 'commander)

(setq commander-args (-reject 's-blank? (s-split " " (getenv "BORON_RUNNER_ARGS"))))

(commander
 (name "boron")
 (description "Opinionated Ert testing workflow")
 (config ".boron")

 (default boron-cli)

 ;; (option "--help, -h" ert-runner/usage)
 ;; (option "--pattern <pattern>, -p <pattern>" ert-runner/pattern)
 ;; (option "--tags <tags>, -t <tags>" ert-runner/tags)
 ;; (option "--load <*>, -l <*>" ert-runner/load)
 ;; (option "--debug" ert-runner/debug)
 ;; (option "--quiet" ert-runner/quiet)
 ;; (option "--verbose" ert-runner/verbose)
 ;; (option "--reporter <name>" ert-runner/set-reporter)
 ;; (option "-L <path>" ert-runner/load-path)

 ;; (option "--script" "Run Emacs as a script/batch job (default)" ignore)
 ;; (option "--no-win" "Run Emacs without GUI window" ignore)
 ;; (option "--win" "Run Emacs with full GUI window" ignore)

 (command "init [name]" boron-cli)
 (command "help" boron-cli))

(provide 'boron-cli)
