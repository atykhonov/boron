(defun boron-eval-buffer ()
  (interactive)
  (let ((report-buffer (get-buffer-create "*boron-report*"))
        (curr-buffer (buffer-name (current-buffer)))
        (lines (list)))
    (setq lines (boron-parse-buffer curr-buffer))
    (pop-to-buffer report-buffer)
    (while (> (length lines) 0)
      (boron-exec (pop lines))
      (sit-for 0.1))))


(provide 'boron-eval)
