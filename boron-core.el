(defvar boron-testcase-hook nil)

(defvar boron-before-test-hook nil)

(defvar boron-after-test-hook nil)

(defvar boron-at-least-one-test-was-executed nil)


(defun boron-setup (command)
  (interactive (list (read-command "Command: ")))
  (call-interactively command))

(defun boron-teardown (command)
  (interactive (list (read-command "Command: ")))
  (call-interactively command)
  (add-hook 'boron-after-test-hook
            `(lambda (test)
               (interactive)
               ,(nth 0 command-history))))

(defun boron-testcase (testcase)
  (interactive "sTest Case: ")
  (run-hook-with-args 'boron-testcase-hook testcase))

(defun boron-test (test)
  (interactive "sTest: ")
  (when boron-at-least-one-test-was-executed
    (run-hook-with-args 'boron-after-test-hook test))
  (run-hook-with-args 'boron-before-test-hook test)
  (setq boron-at-least-one-test-was-executed t))

(defun boron-assert-equal (assertion)
  (interactive "sEqual to: ")
  (let ((test-buffer "*test-buffer*"))
    (if (equal assertion
               (with-current-buffer test-buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
        (boron-reporter-test-passed)
      (boron-reporter-test-failed))))

(defun boron-assert-buffer-contains (buffer string)
  (interactive "bBuffer: \nsString: ")
  (if (not (equal
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward string nil t))
            nil))
      (boron-reporter-test-passed)
    (boron-reporter-test-failed)))

(provide 'boron-core)
