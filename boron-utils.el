(defvar boron-at-least-one-test-was-executed nil)

(defun boron-testcase (testcase)
  (interactive "sTest Case: ")
  (run-hook-with-args 'boron-testcase-hook testcase))

(defun boron-test (test)
  (interactive "sTest: ")
  (when boron-at-least-one-test-was-executed
    (run-hook-with-args 'boron-after-test-hook test))
  (run-hook-with-args 'boron-before-test-hook test)
  (setq boron-at-least-one-test-was-executed t))

(defvar boron-test-args (list))

(defvar boron-test-command nil)

(defun boron-read-args (arg)
  (interactive (list (read-string "Arg: ")))
  (when (equal arg "")
    (throw 'last-arg nil))
  (setq boron-test-args (append boron-test-args (list arg)))
  t)

(defun boron-setup (command)
  (interactive (list (read-command "Command: ")))
  (setq boron-test-command command)
  (setq boron-test-args (list))
  (while
      (catch 'last-arg
        (call-interactively 'boron-read-args)))
  (add-hook 'boron-before-test-hook
            (lambda (test)
              (interactive)
              (apply boron-test-command boron-test-args))))

(defun boron-teardown (command)
  (interactive (list (read-command "Command: ")))
  (setq boron-test-command command)
  (setq boron-test-args (list))
  (while
      (catch 'last-arg
        (call-interactively 'boron-read-args)))
  (add-hook 'boron-after-test-hook
            (lambda (test)
              (interactive)
              (message "LCommand: %s" boron-test-command)
              (message "LArgs: %s" boron-test-args)
              ;; (princ boron-test-args)
              (apply boron-test-command boron-test-args))))

(defun boron-insert (text)
  (interactive "sText: ")
  (insert text))

(provide 'boron-utils)
