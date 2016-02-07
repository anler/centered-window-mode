;;; centered-window-mode.el --- Center the text when there's only one window
;;
;; Author: Anler Hp   <http://anler.me>
;; Version: 1.0.0
;; Contributors:
;;    Mickael Kerjean <http://github.com/mickael-kerjean>
;;    Pierre Lecocq   <http://github.com/pierre-lecocq>
;; Keywords: faces, windows
;; URL: https://github.com/anler/centered-window-mode
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
;; Customizable options are:
;;  cwm-fringe-background
;;  cwm-lighter
;;  cwm-centered-window-width
;;  cwm-ignore-buffer-predicates
;;
;;; Code:
(require 'face-remap)
(require 's)

(defgroup centered-window-mode nil
  "Center text in buffers."
  :group 'customize)

(defcustom cwm-fringe-background
  nil
  "The background color used for the fringe.
If not set is automatically deducted."
  :group 'centered-window-mode)

(defcustom cwm-lighter
  " #"
  "Mode's lighter used in the mode line."
  :group 'centered-window-mode)

(defcustom cwm-centered-window-width
  110
  "Minimum line length required to apply the margins."
  :group 'centered-window-mode)

(defcustom cwm-ignore-buffer-predicates
  (list #'cwm/special-buffer-p)
  "List of predicate functions.
Each is run with current buffer and if it returns 't the
mode won't activate in that buffer.")

(defcustom centered-window-mode-hooks
  nil
  "Hooks to run everytime the text is centered (be careful)."
  :group 'centered-window-mode)

(defun cwm/setup ()
  (add-hook 'window-configuration-change-hook
            'cwm/window-configuration-change)
  (cwm/window-configuration-change))

(defun cwm/teardown ()
  (remove-hook 'window-configuration-change-hook
               'cwm/window-configuration-change)
  (cwm/reset))

(defun cwm/special-buffer-p (buff)
  "Return 't if BUFF buffer name is special (starts with an *).

The *scratch* buffer although special, is treated as not special
by this function."
  (let ((buffname (buffer-name buff)))
    (and (s-starts-with-p "*" buffname)
         (not (string= "*scratch*" buffname)))))

(defun cwm/ignore-buffer-p (buff)
  "Check if BUFF should be ignored when activating the mode."
  (not (null (delq nil (mapcar (lambda (predicate)
                                 (funcall predicate buff))
                               cwm-ignore-buffer-predicates)))))

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
                          (/ (- (window-total-width win) cwm-centered-window-width) 2))))
    (if (or (< fringe_margin 0)
            (< (window-total-width win) cwm-centered-window-width))
        0 fringe_margin)))

(defun cwm/center ()
  (mapcar (lambda(win)
            (let ((winbuff (window-buffer win))
                  (winfringe (cwm/calculate-fringe win)))
              (unless (cwm/ignore-buffer-p winbuff)
                (set-window-fringes win winfringe winfringe)
                (run-hooks 'centered-window-mode-hooks))))
          (window-list nil 0)))

(defun cwm/reset ()
  (mapcar (lambda(win)
            (set-window-fringes win 0 0))
          (window-list nil 0)))

(defun cwm/set-faces ()
  (custom-set-faces
   `(fringe ((t (:background ,cwm-fringe-background))))))

(defun cwm/update-fringe-background ()
  (setq cwm-fringe-background (cwm/get-fringe-background))
  (cwm/set-faces))

(defun cwm/get-fringe-background ()
  (face-attribute 'default :background))

(cwm/update-fringe-background)

;;;###autoload
(define-minor-mode centered-window-mode
  "Minor mode to center text on the current buffer."
  :init-value nil
  :lighter cwm-lighter
  :global t
  (if (window-system)
      (if centered-window-mode
          (cwm/setup)
        (cwm/teardown))
    (message "Centered window mode is currently not supported in the terminal")))

(provide 'centered-window-mode)

;;; centered-window-mode.el ends here
