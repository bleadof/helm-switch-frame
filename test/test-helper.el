(require 'f)

(defconst hsf-test/test-path
  (f-parent (f-this-file)))

(defconst hsf-test/root-path
  (f-parent hsf-test/test-path))

(add-to-list 'load-path hsf-test/root-path)

(require 'helm-switch-frame)
