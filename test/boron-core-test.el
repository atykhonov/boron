(require 'boron-core)


(ert-deftest test-core/boron-setup-unconditional-execution-when-is-declared ()
  (with-test-hooks
   (should
    (string-prefix-p
     "Loret Ipsut is sitply dutty text of the"
     (with-test-buffer ()
                       (boron-exec "M-x boron-setup RET replace-string RET m RET t"))))))

(ert-deftest test-core/boron-setup-doesnt-execute-in-the-hook-before-first-test ()
  (with-test-hooks
   (should
    (string-prefix-p
     "Lorem Ipsum is simply dummy mexm"
     (with-test-buffer ()
                       (boron-exec "M-x boron-setup RET replace-string RET m RET t")
                       (boron-exec "M-x beginning-of-buffer")
                       (boron-exec "M-x replace-string RET t RET m")
                       (boron-exec "M-x boron-test"))))))
;; TODO: change all these boron-exec to:
;;
;; (boron-exec (list "M-x command"
;;                   "M-x command"))
;;
;; write a test for the boron-exec with the mocks (learn it!)

(ert-deftest test-core/boron-setup-before-second-test ()
  (with-test-hooks
   (should
    (string-prefix-p
     "Loret Ipset is sitply detty text of the"
     (with-test-buffer ()
                       (boron-exec "M-x boron-setup RET replace-string RET m RET t")
                       (boron-exec "M-x boron-test RET test")
                       (boron-exec "M-x beginning-of-buffer")
                       (boron-exec "M-x replace-string RET ut RET em")
                       (boron-exec "M-x beginning-of-buffer")
                       (boron-exec "M-x boron-test RET test2"))))))

(ert-deftest test-core/boron-teardown-unconditional-execution-when-is-declared ()
  (with-test-hooks
   (should
    (string-prefix-p
     "Loret Ipsut is sitply dutty text of the"
     (with-test-buffer ()
                       (boron-exec "M-x boron-teardown RET replace-string RET m RET t"))))))
