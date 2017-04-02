;;; centered-window-mode.el --- Center the text when there's only one window  -*- lexical-binding: t; -*-
;;
;; Author: Anler Hernández Peral <inbox+emacs@anler.me>
;; Version: 1.0.0
;; Contributors:
;;    Mickael Kerjean <http://github.com/mickael-kerjean>
;;    Pierre Lecocq   <http://github.com/pierre-lecocq>
;; Keywords: faces windows
;; URL: https://github.com/anler/centered-window-mode
;; Package-Requires: ((s "1.10.0"))
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
(require 'cl)

(defgroup centered-window-mode nil
  "Center text in buffers."
  :group 'customize)

(defcustom cwm-lighter
  " cwm"
  "Mode's lighter used in the mode line."
  :group 'centered-window-mode)

(defcustom cwm-centered-window-width
  110
  "Minimum line length required to apply the margins."
  :group 'centered-window-mode)

(defcustom cwm-use-vertical-padding
  nil
  "Whether or not use experimental vertical padding."
  :group 'centered-window-mode)

(defcustom cwm-frame-internal-border
  70
  "Frame internal border to use when vertical padding is used."
  :group 'centered-window-mode)

(defcustom cwm-ignore-buffer-predicates
  (list #'cwm-special-buffer-p)
  "List of predicate functions.
Each is run with current buffer and if it returns 't the
mode won't activate in that buffer.")

(defcustom centered-window-mode-hooks
  nil
  "Hooks to run everytime the text is centered (be careful)."
  :group 'centered-window-mode)

(defadvice load-theme (after cwm-set-faces-on-load-theme activate)
  "Change the default fringe background whenever the theme changes."
  (cwm-update-fringe-background))

(defun cwm-ignore-window-p (window)
  "Check if BUFF should be ignored when activating the mode."
  (not
   (null
    (delq nil
          (mapcar (lambda (predicate)
                    (funcall predicate (window-buffer window)))
                  cwm-ignore-buffer-predicates)))))

(defun cwm-special-buffer-p (buffer)
  "Return 't if BUFF buffer name is special (starts with an *).

The *scratch* buffer although special, is treated as not special
by this function."
  (let ((buffname (s-trim (buffer-name buffer))))
    (and buffname
         (s-starts-with-p "*" buffname)
         (not (string= "*scratch*" buffname)))))

(defun cwm-update-fringe-background ()
  (custom-set-faces
   `(fringe ((t (:background ,(face-attribute 'default :background)))))))

(defun cwm-turn-on ()
  (add-hook 'window-configuration-change-hook #'cwm-center-windows)
  (add-hook 'window-configuration-change-hook #'cwm-center-frame)
  (cwm-center-windows)
  (when cwm-use-vertical-padding
    (set-frame-parameter nil 'internal-border-width cwm-frame-internal-border)))

(defun cwm-turn-off ()
  (remove-hook 'window-configuration-change-hook #'cwm-center-windows)
  (cwm-center-windows)
  (set-frame-parameter nil 'internal-border-width 0))

(defun cwm-center-windows ()
  (let ((windows (window-list nil :exclude-minibuffer)))
    (mapc #'cwm-center-window-instructions
          (mapcar #'cwm-centering-instructions
                  (cl-remove-if #'cwm-ignore-window-p windows))
          )
    (run-hooks 'centered-window-mode-hooks)))

(defstruct cwm-centering-instructions
  window
  left-width
  right-width)

(defun cwm-center-window-instructions (instructions)
  (let* ((window (cwm-centering-instructions-window instructions))
         (frame (window-frame window)))
    (set-window-fringes window
                        (cwm-centering-instructions-left-width instructions)
                        (cwm-centering-instructions-right-width instructions))))

(defun cwm-centering-instructions (window)
  (let ((buffer (window-buffer window))
        (widths (cwm-calculate-appropriate-fringe-widths window)))
    (make-cwm-centering-instructions
     :window window
     :left-width (car widths)
     :right-width (cdr widths))))

(defun cwm-calculate-appropriate-fringe-widths (window)
  (let* ((mode-active-p (with-current-buffer (window-buffer window) (cwm-activated-p)))
         (pixel (frame-char-width (window-frame window)))
         (n  (if mode-active-p
                 (/ (max (- (window-total-width window) cwm-centered-window-width) 0)
                    2)
               0))
         (ratio (/ (* n 40) 100))
         (left-width (* pixel (if (> n 0) (+ n ratio) n)))
         (right-width (* pixel (if (> n 0) (- n ratio) n)))
         )
    `(,left-width . ,right-width)))

(defun cwm-activated-p ()
  centered-window-mode)

;;;###autoload
(defun centered-window-mode-toggle ()
  (if centered-window-mode
      (centered-window-mode -1)
    (centered-window-mode +1)))

;;;###autoload
(define-minor-mode centered-window-mode
  "Minor mode to center text on the current buffer"
  :init-value nil
  :lighter cwm-lighter
  (if centered-window-mode (cwm-turn-on) (cwm-turn-off))
  )

(provide 'centered-window-mode)
;;; centered-window-mode.el ends here
;; (frame-height)
;; (frame-parameter nil 'internal-border-width)
;; (frame-char-width)
;; (/ 67 9)
