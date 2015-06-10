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

(ert-deftest test-exec/boron-setup-complex-command ()
  (let ((temp-buffer (get-buffer-create "*temp-buffer*")))
    (pop-to-buffer temp-buffer)
    (setq boron-after-test-hook nil)
    (setq boron-before-test-hook nil)
    (setq boron-at-least-one-test-was-executed nil)
    (should
     (equal
      (with-current-buffer temp-buffer
        (insert "test\n")
        (insert "test\n")
        (insert "test")
        (goto-char (point-min))
        (boron-exec "M-x boron-setup RET replace-string RET test RET best")
        (boron-exec "M-x boron-test RET test")
        (prog1
            (buffer-substring (point-min) (point-max))
          (kill-buffer temp-buffer)))
      "best
best
best"))))

(ert-deftest test-exec/boron-teardown-complex-command ()
  (let ((temp-buffer (get-buffer-create "*temp-buffer*")))
    (pop-to-buffer temp-buffer)
    (setq boron-after-test-hook nil)
    (setq boron-before-test-hook nil)
    (setq boron-at-least-one-test-was-executed nil)
    (should
     (equal
      (with-current-buffer temp-buffer
        (insert "test\n")
        (insert "test\n")
        (insert "test")
        (goto-char (point-min))
        (boron-exec "M-x boron-teardown RET replace-string RET test RET best")
        (boron-exec "M-x boron-test RET test")
        (boron-exec "M-x boron-test RET test")
        (prog1
            (buffer-substring (point-min) (point-max))
          (kill-buffer temp-buffer)))
      "best
best
best"))))
