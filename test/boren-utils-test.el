(require 'boron-utils)
(require 'boron-core)

(defvar test-hook-result nil)

(defun test-testcase-hook (testcase)
  (interactive)
  (setq test-hook-result testcase))

(defun test-before-test-hook (test)
  (interactive)
  (setq test-hook-result test))

(defun test-after-test-hook (test)
  (interactive)
  (setq test-hook-result test))

(ert-deftest test-boron-testcase ()
  (add-hook 'boron-testcase-hook 'test-testcase-hook)
  (boron-testcase "testcase")
  (should (equal test-hook-result "testcase")))

(ert-deftest test-boron-test/before-hook ()
  (add-hook 'boron-before-test-hook 'test-before-test-hook)
  (boron-test "test")
  (should (equal test-hook-result "test")))

(ert-deftest test-boron-test/after-hook ()
  (add-hook 'boron-after-test-hook 'test-after-test-hook)
  (boron-test "test")
  (should (equal test-hook-result nil))
  (boron-test "test2")
  (should (equal test-hook-result "test2")))

;; There is a problem regarding the setup recent ideas. We need to collect all
;; commands between boron-feature-setup and the next predefined command. Predefined
;; command may be boron-scenario, boron-teardown and so on. But I really dislike that
;; way. May be we can find a little bit different way? For example may be we can use
;; something like the following:

;; M-x boron-feature-setup RET boron-pop-to-new-buffer RET "test-buffer"
;; M-x boron-feature-setup RET boron-insert RET "Lorem ipsum"

;; Could such a way be ok for my case? We can see some duplications but these
;; commands could be really executed so we don't need to hack skipping such records
;; and executing afterwards.

;; The same way we could use:

;; M-x boron-feature-teardown RET boron-kill-buffer RET "test-buffer"
;; M-x boron-feature-teardown RET eval-expression RET "(kill-buffer \"test-buffer\")"

;; Yes! Just reminded! I need to rename all these feature/scenarios! I want to have
;; testcase and test instead of these cucumber-like names.

;; Also may be it would be good idea to have something like the following:

;; M-x boron-setup RET boron-pop-to-new-buffer RET "test-buffer"

;; Such command will be creating "test-buffer" before a suite.

;; Such commands can be found on many different files and I'll be not able to parse
;; all these commands and add to the very beginning of execution!

;; So may be even I'll come to a conclusion to short-cut the name and leave:
;; M-x boron-setup
;; and
;; M-x boron-teardown
;;
;; At least it looks pretty interesting
;;

(ert-deftest test-boron-insert ()
  (let ((test-buffer "*test-buffer*")
        (test-text "foo bar baz"))
    (should (equal
             (with-current-buffer (get-buffer-create test-buffer)
               (boron-insert "foo bar baz")
               (prog1
                   (buffer-substring (point-min) (point-max))
                 (kill-buffer test-buffer)))
             test-text))))
