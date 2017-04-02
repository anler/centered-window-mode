(require 'centered-window-mode nil t)

(ert-deftest cwm-mode-activation/deactivation ()
  (with-temp-buffer
    (centered-window-mode-toggle)
    (should (eq t centered-window-mode))
    (centered-window-mode-toggle)
    (should (eq nil centered-window-mode))))
