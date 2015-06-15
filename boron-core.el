(defvar boron-testcase-hook nil)

(defvar boron-test-hook nil)

(defvar boron-test-passed nil)

(defvar boron-test-failed nil)


(defun boron-testcase (testcase)
  (interactive "sTest Case: ")
  (run-hook-with-args 'boron-testcase-hook testcase))

(defun boron-test (test)
  (interactive "sTest: ")
  (run-hook-with-args 'boron-test-hook test))

(defun boron-assert-buffer-equal (buffer string)
  (interactive "bBuffer: \nsString: ")
  (if (equal string
             (with-current-buffer buffer
               (buffer-substring-no-properties (point-min) (point-max))))
      (run-hooks 'boron-test-passed)
    (run-hooks 'boron-test-failed)))

(defun boron-assert-buffer-contains (buffer string)
  (interactive "bBuffer: \nsString: ")
  (if (not (equal
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward string nil t))
            nil))
      (run-hooks 'boron-test-passed)
    (run-hooks 'boron-test-failed)))

(provide 'boron-core)
