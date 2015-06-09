(require 'edmacro)


(defun boron-exec (command)
  (execute-kbd-macro
   (edmacro-parse-keys command)))


(defun boron-exec-protected (command wwwin temp-buffer orig-buffer)
  (let ((win (selected-window)))
    (unwind-protect
        (with-current-buffer temp-buffer
          (set-window-buffer wwwin temp-buffer t)
          (boron-exec command))
      (set-window-buffer wwwin orig-buffer t))))


(provide 'boron-exec)
