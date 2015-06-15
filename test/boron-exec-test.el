(require 'boron-exec)
(require 'boron-utils)


(ert-deftest test-exec/trivial-command ()
  "Test execution of very trivial command."
  (boron-exec "M-x forward-word"))

(ert-deftest test-exec/incorrect-command ()
  (should-error (boron-exec "M-x forward")))

(ert-deftest test-exec/incorrect-command ()
  (boron-exec "M-x describe-function RET \"test\" C-a"))

(ert-deftest test-exec/command-with-wrong-parameters ()
  (boron-exec "M-x replace-string RET \"Foo\""))

(ert-deftest test-exec/command-with-prefix-argument ()
  (boron-exec "C-u -1 M-x forward-word"))

(ert-deftest test-exec/incorrect-expression ()
  (should-error (boron-exec "M-: forward-word")))

(ert-deftest test-exec/command-with-c-x ()
  (boron-exec "C-x C-s"))

(ert-deftest test-exec/command-with-c-c ()
  (boron-exec "C-c C-c"))

(ert-deftest boron-exec/list ()
  (boron-exec (list "C-c C-c"
                    "M-x forward-word"
                    "M-: (forward-word)")))
