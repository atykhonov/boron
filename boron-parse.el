(defun boron-trim-string (string)
  "Remove whitespaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n\r]*" ""
                            (replace-regexp-in-string "[ \t\n\r]*\\'" "" string)))

(defun boron-parse-line (line)
  (let ((result nil)
        (line (boron-trim-string line)))
    (if (> (length line) 0)
        (cond ((equal (substring line 0 1) "#")
               (setq result nil))
              (t (setq result (boron-parse-command-line line))))
      (setq result nil))
    result))

(defun boron-parse-command-line (line)
  "Parse a line which begins with a command, i.e. with M-x."
  (if (string-match "\\(\".+?\"\\)" line)
      (progn
        (while (string-match "\\(\".+?\"\\)" line)
          (let ((begin (match-beginning 1))
                (end (match-end 1)))
            (setq line (concat
                        (substring line 0 begin)
                        (concat (apply (lambda (&rest args)
                                         (let ((result ""))
                                           (dolist (arg args) ; replace dolist
                                             (when (equal arg " ")
                                               (setq arg " SPC "))
                                             (setq result (concat result arg)))
                                           result))
                                       (split-string (substring line (+ begin 1) (- end 1))
                                                     "" t)))
                        (substring line end)))))
        line)
    line))

(defun boron-parse-buffer (buffer)
  (let ((lines (list))
        (parsed-line nil))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (not (eobp))
        (setq line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
        (setq parsed-line (boron-parse-line line))
        (when parsed-line
          (setq lines
                (append lines (list parsed-line))))
        (search-forward "\n" nil t)))
    lines))

(provide 'boron-parse)
