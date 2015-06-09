;;; boron.el --- Integration testing

;; Copyright (C) 2015 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; Version: 0.0.1
;; Keywords: test
;; URL: http://github.com/atykhonov/boron.el
;; Package-Requires: ((s "1.6.1") (dash "1.8.0") (f "0.10.0") (commander "0.2.0") (ansi "0.1.0"))

;; This file is NOT part of GNU Emacs.

(require 'f)
(require 'ansi)
(require 'commander)
(require 'boron-parse)
(require 'boron-exec)
(require 'boron-reporter)

;; (defvar boron-path (f-dirname (f-this-file)))

(defvar boron-font-lock-keywords
  ;; Keywords
  `((,(rx symbol-start
          (group (or
                  (group "M-" (any print))
                  "M-"
                  "RET" "SPC" (group "C-" (any print)) "as" "elif" "global" "or" "with"
                  "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
                  "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
                  "try"
                  ;; Python 2:
                  "print" "exec"
                  ;; Python 3:
                  ;; False, None, and True are listed as keywords on the Python 3
                  ;; documentation, but since they also qualify as constants they are
                  ;; fontified like that in order to keep font-lock consistent between
                  ;; Python versions.
                  "nonlocal"
                  ;; Extra:
                  "self"))
          symbol-end)
     (1 font-lock-function-name-face))
    ;; Constants
    (,(rx symbol-start
          (or
           "Ellipsis" "False" "None" "NotImplemented" "True" "__debug__"
           ;; copyright, license, credits, quit and exit are added by the site
           ;; module and they are not intended to be used in programs
           "copyright" "credits" "exit" "license" "quit")
          symbol-end) . font-lock-constant-face)))

(defun boron-eval-current-buffer ()
  (interactive)
  (let ((report-buffer (get-buffer-create "*boron-report*"))
        (curr-buffer (buffer-name (current-buffer)))
        (lines (list)))
    (setq lines (boron-parse-buffer curr-buffer))
    (pop-to-buffer report-buffer)
    (while (> (length lines) 0)
      (boron-exec (pop lines))
      (sit-for 0.1))))

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

(defun boron-reporter-test-passed ()
  (with-current-buffer (get-buffer-create "*boron-report*")
    (insert (concat "*** Test passed!" "\n"))))

(defun boron-reporter-test-failed ()
  (with-current-buffer (get-buffer-create "*boron-report*")
    (insert (concat "*** Test failed!" "\n"))))

(defun boron-reporter-testcase (testcase)
  (with-current-buffer (get-buffer-create "*boron-report*")
    (insert (concat "* " testcase "\n"))))

(defun boron-reporter-test (test)
  (with-current-buffer (get-buffer-create "*boron-report*")
    (insert (concat "** " test "\n"))))

(defconst boron-syntax-propertize-function
  (syntax-propertize-rules
   ((rx "(.*)")
    (0 (ignore (python-syntax-stringify))))))

(defvar boron-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table.")

;;;###autoload
(define-derived-mode boron-mode fundamental-mode "Boron"
  "Major mode for editing boron files.

\\{boron-mode-map}"
  ;; (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (set (make-local-variable 'comment-start) "#")
  ;; (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  
  ;; (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  ;; (setq-local comment-add 1)		;default to `;;' in comment-region
  ;; (setq-local comment-column 40)
  ;; (setq-local comment-use-syntax t)
  
  (set (make-local-variable 'font-lock-defaults)
       '(boron-font-lock-keywords nil nil nil nil))

  (set (make-local-variable 'syntax-propertize-function)
       boron-syntax-propertize-function)

  (set-syntax-table boron-syntax-table)
  
  (set (make-local-variable 'paragraph-start) "\\s-*$"))

(defun boron-reporter-cli-testcase (testcase)
  (interactive)
  (let* ((header (format "Test Case: %s" testcase)))
    (princ (ansi-red header))))

(defun boron-reporter-cli-scenario (test)
  (interactive)
  (let* ((header (format "Test: %s" test)))
    (princ (ansi-cyan header))))

(defun boron-cli ()
  (interactive)
  (find-file "example.boron")
  (add-hook 'boron-testcase-hook 'boron-reporter-cli-feature)
  (add-hook 'boron-before-test-hook 'boron-reporter-cli-scenario)
  (boron-eval-current-buffer))


;; (setq commander-args (-reject 's-blank? (s-split " " (getenv "BORON_RUNNER_ARGS"))))

;; (commander
;;  (name "boron")
;;  (description "Opinionated Ert testing workflow")
;;  (config ".boron")

;;  (default boron-cli)

;;  ;; (option "--help, -h" ert-runner/usage)
;;  ;; (option "--pattern <pattern>, -p <pattern>" ert-runner/pattern)
;;  ;; (option "--tags <tags>, -t <tags>" ert-runner/tags)
;;  ;; (option "--load <*>, -l <*>" ert-runner/load)
;;  ;; (option "--debug" ert-runner/debug)
;;  ;; (option "--quiet" ert-runner/quiet)
;;  ;; (option "--verbose" ert-runner/verbose)
;;  ;; (option "--reporter <name>" ert-runner/set-reporter)
;;  ;; (option "-L <path>" ert-runner/load-path)

;;  ;; (option "--script" "Run Emacs as a script/batch job (default)" ignore)
;;  ;; (option "--no-win" "Run Emacs without GUI window" ignore)
;;  ;; (option "--win" "Run Emacs with full GUI window" ignore)

;;  (command "init [name]" boron-cli)
;;  (command "help" boron-cli)
;;  )

(provide 'boron)

;;; boron.el ends here
