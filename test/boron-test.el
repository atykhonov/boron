(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)
(require 'ansi)

(defvar boron-test/test-path
  (f-dirname load-file-name))

(defvar boron-test/root-path
  (f-parent boron-test/test-path))

(setq debug-on-entry t)
(setq debug-on-error t)
(setq boron-include-tags nil)
(setq boron-verbose t)
(setq boron-path boron-test/root-path)


(add-to-list 'load-path boron-test/root-path)
