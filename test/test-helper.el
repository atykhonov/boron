(require 'f)


(defun fixture-file-path (fixture)
  (f-join boron-fixtures-path fixture))

(defun load-fixture (fixture)
  (with-temp-buffer
    (find-file (fixture-file-path fixture))
    (buffer-substring-no-properties (point-min) (point-max))))

(defmacro with-test-buffer (&rest body)
  (declare (doc-string 3) (indent 2))
  `(let ((test-buffer "*test-buffer*"))
     (with-current-buffer (get-buffer-create test-buffer)
       (pop-to-buffer test-buffer)
       (insert "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.")
       (goto-char (point-min))
       ,@body
       (prog1
           (buffer-substring-no-properties (point-min) (point-max))
         (kill-buffer test-buffer)))))

(defmacro with-test-hooks (&rest body)
  (declare (doc-string 3) (indent 2))
  `(progn
     (setq boron-after-test-hook nil)
     (setq boron-before-test-hook nil)
     (setq boron-at-least-one-test-was-executed nil)
     ,@body))
