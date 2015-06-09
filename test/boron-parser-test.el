(require 'boron-parse)


(ert-deftest test-parse-line/empty-line ()
  "In case of empty line returns nil."
  (should (equal
           (boron-parse-line "")
           nil)))

(ert-deftest test-parse-line/full-of-spaces ()
  "In case of line with spaces returns nil."
  (should (equal
           (boron-parse-line "      ")
           nil)))

(ert-deftest test-parse-line/line-begins-with-expression ()
  "A line begins with M-:."
  (should (equal (boron-parse-line "M-: \"(forward-word)\"")
                 "M-: (forward-word)")))

(ert-deftest test-parse-line/expression-with-space ()
  "A line begins with M-:."
  (should (equal (boron-parse-line "M-: \"(forward-word 4)\"")
                 "M-: (forward-word SPC 4)")))

(ert-deftest test-parse-line/expression-with-several-spaces ()
  "A line begins with M-:."
  (should (equal (boron-parse-line "M-: \"(progn (forward-word 1) (forward-word 1) (forward-word 1))\"")
                 "M-: (progn SPC (forward-word SPC 1) SPC (forward-word SPC 1) SPC (forward-word SPC 1))")))

(ert-deftest test-parse-line/line-begins-with-command ()
  "A line begins with M-x."
  (should (equal (boron-parse-line "M-x forward-word")
                 "M-x forward-word")))

(ert-deftest test-parse-line/command-with-simple-string ()
  "A line begins with M-x."
  (should (equal (boron-parse-line "M-x search-forward RET \"Foo\"")
                 "M-x search-forward RET Foo")))

(ert-deftest test-parse-line/command-with-string ()
  "A line begins with M-x."
  (should (equal (boron-parse-line "M-x search-forward RET \"Foo Bar Baz\"")
                 "M-x search-forward RET Foo SPC Bar SPC Baz")))

(ert-deftest test-parse-line/command-with-several-strings ()
  "A line begins with M-x."
  (should (equal (boron-parse-line "M-x replace-string RET \"Foo Bar Baz\" RET \"Test Best Rest\"")
                 "M-x replace-string RET Foo SPC Bar SPC Baz RET Test SPC Best SPC Rest")))

(ert-deftest test-parse-line/line-begins-with-comment ()
  "In case of line begins with comment symbol # then return nil."
  (should (equal (boron-parse-line "# comment")
                 nil)))

(ert-deftest test-parse-line/line-begins-with-prefix-argument ()
  (should (equal (boron-parse-line "C-u M-x forward-word")
                 "C-u M-x forward-word")))

(ert-deftest test-parse-line/line-begins-with-prefix-argument-and-expression ()
  (should (equal (boron-parse-line "C-u M-: \"(forward-word)\"")
                 "C-u M-: (forward-word)")))

(ert-deftest test-parse-buffer ()
  "Test parse a buffer."
  (let ((test-buffer (get-buffer-create " *test-buffer*")))
    (with-current-buffer test-buffer
      (insert "# comment\n")
      (insert "M-x forward-word\n")
      (insert "M-: \"(forward-word)\"\n"))
    (should (equal (boron-parse-buffer test-buffer)
                   (list "M-x forward-word"
                         "M-: (forward-word)")))))
