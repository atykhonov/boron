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
(require 'boron-core)
(require 'boron-parse)
(require 'boron-exec)
(require 'boron-reporter)


(defvar boron-font-lock-keywords
  ;; Keywords
  `((,(rx symbol-start
          (group (or
                  "M-"
                  "RET" "SPC"
                  (group "C-" (any print))
                  (group "M-" (any print))))
          symbol-end)
     (1 font-lock-function-name-face))))

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


(provide 'boron)

;;; boron.el ends here
