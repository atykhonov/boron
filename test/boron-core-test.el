(require 'cl)
(require 'boron-core)
(require 'boron-exec)


(ert-deftest test-core/boron-hooks-are-empty ()
  (with-test-hooks
      (should (equal boron-testcase-hook nil))
      (should (equal boron-test-hook nil))
    (with-test-buffer ()
                      (boron-exec "M-x boron-test RET foo"))
    (should (equal boron-testcase-hook nil))
    (should (equal boron-test-hook nil))))

(ert-deftest test-core/boron-testcase-hook ()
  (let ((test-hook-called nil))
    (with-test-hooks
        (add-hook 'boron-testcase-hook
                  (lambda (test)
                    (interactive)
                    (setq test-hook-called t)))
        (with-test-buffer ()
                          (boron-testcase "TestCase foo.")))
    (should (equal test-hook-called t))))

(ert-deftest test-core/boron-test-hook ()
  (let ((test-hook-called nil))
    (with-test-hooks
        (add-hook 'boron-test-hook
                  (lambda (test)
                    (interactive)
                    (setq test-hook-called t)))
        (with-test-buffer ()
                          (boron-exec "M-x boron-test RET foo")))
    (should (equal test-hook-called t))))

(ert-deftest boron-assert-buffer-equal/test-passed ()
  (let ((test-passed nil))
    (add-hook 'boron-test-passed
              (lambda ()
                (interactive)
                (setq test-passed t)))
    (with-test-buffer
     (erase-buffer)
     (insert "TestBestRest")
     (boron-exec "M-x boron-assert-buffer-equal RET test-buffer RET TestBestRest"))
    (should (equal test-passed t))))

(ert-deftest boron-assert-buffer-equal/test-failed ()
  (let ((test-failed nil))
    (add-hook 'boron-test-failed
              (lambda ()
                (interactive)
                (setq test-failed t)))
    (with-test-buffer
     (erase-buffer)
     (insert "FooBarBaz")
     (boron-exec "M-x boron-assert-buffer-equal RET test-buffer RET TestBestRest"))
    (should (equal test-failed t))))

(ert-deftest boron-assert-buffer-contains/test-passed ()
  (interactive)
  (let ((test-passed nil))
    (add-hook 'boron-test-passed
              (lambda ()
                (interactive)
                (setq test-passed t)))
    (with-test-buffer
     (boron-exec "M-x boron-assert-buffer-contains RET test-buffer RET Lorem SPC Ipsum")))
  )
