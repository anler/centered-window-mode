;;; centered-window-mode.el --- Center the text when there's only one window
;;
;; Copyright (C) 2014 Anler Hp <http://anler.me>
;;
;; Author: Anler Hp <http://anler.me>
;; Version: 0.0.1
;; Keywords: faces, windows
;; URL: https://github.com/ikame/centered-window-mode
;; Compatibility: GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Enable centered-window-mode and your text is going to be centered when there's
;; only one window in the frame.
;;
;;; Changes Log:
;;
;;; Code:

(defvar fringe-background nil "The background color used for the fringe")
(defvar centered-window-width 110 "text size")

(defun cwm/setup ()
  (add-hook 'window-configuration-change-hook
            'cwm/window-configuration-change)
  (cwm/window-configuration-change))

(defun cwm/teardown ()
  (remove-hook 'window-configuration-change-hook
               'cwm/window-configuration-change)
  (cwm/reset))

(defadvice split-window-right (before cwm/reset-on-split activate)
  "Disable cwm-mode presentation (if active) before splitting window"
  (when (centered-window-mode)
    (cwm/reset)))

(defadvice split-window-right (after cwm/center-on-split activate)
  "Restore cwm-mode presentation (if active) after splitting window"
  (when (centered-window-mode)
    (cwm/center)))

(defadvice load-theme (after cwm/set-faces-on-load-theme activate)
  "Change the default fringe background whenever the theme changes"
  (message "load theme after here")
  (cwm/update-fringe-background))

(defun cwm/window-configuration-change ()
  (if (null centered-window-mode)
      (cwm/reset)
    (cwm/center)))

(defun cwm/calculate-fringe (&optional win)
  (let ((fringe_margin (* (frame-char-width)
                          (/ (- (window-total-width win) centered-window-width) 2))))
    (if (or (< fringe_margin 0)
            (< (window-total-width win) centered-window-width))
        0 fringe_margin)))

(defun cwm/center ()
  (mapcar (lambda(win)
            (set-window-fringes win (cwm/calculate-fringe win) (cwm/calculate-fringe win)))
            (window-list nil 0)))

(defun cwm/reset ()
  (mapcar (lambda(win)
            (set-window-fringes win 0 0))
          (window-list nil 0)))

(defun cwm/set-faces ()
  (custom-set-faces
   `(fringe ((t (:background ,fringe-background))))))

(defun cwm/update-fringe-background ()
  (setq fringe-background (cwm/get-fringe-background))
  (cwm/set-faces))

(defun cwm/get-fringe-background ()
  (face-attribute 'default :background))

(cwm/update-fringe-background)

;;;###autoload
(define-minor-mode centered-window-mode
  "Minor mode to cbm on the current buffer."
  :init-value nil
  :lighter " #"
  :global t
  (if (window-system)
      (if centered-window-mode
          (cwm/setup)
        (cwm/teardown))
    (message "Currently not supported in the terminal")))

(provide 'centered-window-mode)

;;; centered-window-mode.el ends here
