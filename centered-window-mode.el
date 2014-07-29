;;; centered-window-mode.el --- Center the text when there's only one window
;;
;; Copyright (C) 2014 Anler Hp <http://anler.me>
;;
;; Author: Anler Hp <http://anler.me>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))
;; Keywords: faces, windows
;; URL: https://github.com/ikame/centered-window-mode
;; Compatibility: GNU Emacs 24.x
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

(defvar fringe-background
  (face-attribute 'fringe :background))

(defun cwm/setup ()
  (add-hook 'window-configuration-change-hook
            'cwm/window-configuration-change)
  (cwm/window-configuration-change))

(defun cwm/teardown ()
  (remove-hook 'window-configuration-change-hook
               'cwm/window-configuration-change)
  (cwm/window-configuration-change))

(defadvice split-window-right (before cwm/reset-on-split activate)
  "Disable cbm-mode presentation (if active) before splitting window"
  (when fringe-mode
    (cwm/reset)))

(defun cwm/window-configuration-change ()
  (if (or (> (length (window-list)) 1)
          (null centered-window-mode))
      (cwm/reset)
    (cwm/center)))

(defun cwm/center ()
  (set-fringe-mode
   (/ (- (frame-pixel-width)
         (* 100 (frame-char-width)))
      2))
  (let ((fringe-background (face-attribute 'default :background)))
    (cwm/set-faces)))

(defun cwm/reset ()
  (set-fringe-mode nil)
  (cwm/set-faces))

(defun cwm/set-faces ()
  (custom-set-faces
   `(fringe ((t (:background ,fringe-background))))))

;;;###autoload
(define-minor-mode centered-window-mode
  "Minor mode to cbm on the current buffer."
  :init-value nil
  :lighter " ⌗"
  :global t
  (if centered-window-mode
      (cwm/setup)
    (cwm/teardown)))

(provide 'centered-window-mode)

;;; centered-window-mode.el ends here
