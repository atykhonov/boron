(require 'boron-exec)
(require 'boron-utils)

;; boron-exec is just a little wrapper around execute-kbd-macro and
;; edmacro-parse-keys functions. These unit tests doesn't actually tests these
;; functions but rather helps me better understand how they do work and what I can
;; expect from them in different cases.

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

;; requirements to the boron-exec-protected:

;; 1. All commands should be executed in the test buffer

;;    q1: what if a user doesn't have a need to have test buffer?

;;    probable answer: we shouldn't create test buffer but we need allow a user to
;;    create and make active the test buffer. In that case a user needs to make extra
;;    steps. Yes, that is correct! Actually that should be done in the setUp way.

;;    So. The answer is: we don't create any test buffer. We executes all commands in
;;    the current buffer.

;;    q2: then... what if the current buffer is a buffer with .boron file? If a user
;;    creates a buffer and make it active, then current buffer will be user defined!
;;    Probably we need to kill it after the test (without a need of tearDown). But
;;    then we need to figure out which buffers are created by a user. Right? There
;;    are can be at least two way: 1. a user kills its buffers by means of tearDown
;;    way and 2. we really catches all buffers created in setUp and then just
;;    tearDown them.

;; (ert-deftest test-exec-protected/trivial-command ()
;;   (boron-exec-protected "M-x forward-word"))

;; (ert-deftest test-exec/boron-setup-simple-command ()
;;   (boron-exec "M-x boron-setup RET forward-word"))

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
