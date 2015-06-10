(require 'boron-eval)


(ert-deftest boron-eval-current-buffer/simple-case ()
  (boron-eval-buffer)
  (should (equal t t)))
