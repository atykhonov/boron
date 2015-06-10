(require 'f)


(defun load-fixture (fixture)
  (let ((fixture-path (f-join boron-fixtures-path fixture)))
    (with-temp-buffer
      (find-file fixture-path)
      (buffer-substring-no-properties (point-min) (point-max)))))
