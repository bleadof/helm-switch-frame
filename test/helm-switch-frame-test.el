(ert-deftest frame-names-test ()
  (new-frame)
  (helm-switch-frame)
  (should (= ("F1") ("F1"))))
