;;; centered-window-mode.el --- Center the text when there's only one window  -*- lexical-binding: t; -*-
;;
;; Author: Anler Hernández Peral <inbox+emacs@anler.me>
;; Version: 1.3.0
;; Contributors:
;;    Mickael Kerjean <https://github.com/mickael-kerjean>
;;    Pierre Lecocq   <https://github.com/pierre-lecocq>
;;    Syohei YOSHIDA  <https://github.com/syohex>
;;    Lars Tveito     <https://github.com/larstvei>
;; Keywords: faces windows
;; URL: https://github.com/anler/centered-window-mode
;; Package-Requires: ((emacs "24.4"))
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
;;  cwm-lighter
;;  cwm-centered-window-width
;;  cwm-ignore-buffer-predicates
;;
;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'face-remap)
(require 'subr-x)

(defgroup centered-window-mode nil
  "Center text in windows."
  :group 'windows
  :prefix "cwm-")

(defcustom cwm-lighter
  " #"
  "Mode's lighter used in the mode line."
  :group 'centered-window-mode
  :type 'string)

(defcustom cwm-centered-window-width
  110
  "Minimum line length required to apply the margins."
  :group 'centered-window-mode
  :type 'integer)

(defcustom cwm-use-vertical-padding
  nil
  "Whether or not use experimental vertical padding."
  :group 'centered-window-mode
  :type 'boolean)

(defcustom cwm-frame-internal-border
  70
  "Frame internal border to use when vertical padding is used."
  :group 'centered-window-mode
  :type 'integer)

(defcustom cwm-left-fringe-ratio
  40
  "Ratio by which the left fringe is padded more than the right.
Should be a value between 0 and 100. A value of 0 means off."
  :group 'centered-window-mode
  :type '(integer
          :validate (lambda (widget)
                      (let ((ratio (widget-value widget)))
                        (unless (<= 0 ratio 100)
                          (widget-put widget :error (format "Invalid ratio (0-100): '%s'" ratio))
                          widget)))))

(defcustom cwm-ignore-buffer-predicates
  (list #'cwm-special-buffer-p)
  "List of predicate functions.
Each is run with current buffer and if it returns 't the
mode won't activate in that buffer."
  :group 'centered-window-mode
  :type '(list function))

(define-obsolete-variable-alias
  'centered-window-mode-hooks
  'cwm-hooks "1.3.0")
(defcustom cwm-hooks
  nil
  "Hooks to run everytime the text is centered (be careful)."
  :group 'centered-window-mode
  :type 'hook)

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
  (let ((buffname (string-trim (buffer-name buffer))))
    (and buffname
         (string-prefix-p "*" buffname)
         (not (string= "*scratch*" buffname)))))

(defun cwm-update-fringe-background ()
  (custom-set-faces
   `(fringe ((t (:background ,(face-attribute 'default :background)))))))

(defun cwm-turn-on ()
  (add-hook 'window-configuration-change-hook #'cwm-center-windows)
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
                  (cl-remove-if #'cwm-ignore-window-p windows)))
    (run-hooks 'centered-window-mode-hooks)))

(cl-defstruct cwm-centering-instructions
  window
  left-width
  right-width)

(defun cwm-center-window-instructions (instructions)
  (let* ((window (cwm-centering-instructions-window instructions)))
    (set-window-fringes window
                        (cwm-centering-instructions-left-width instructions)
                        (cwm-centering-instructions-right-width instructions))))

(defun cwm-centering-instructions (window)
  (let ((widths (cwm-calculate-appropriate-fringe-widths window)))
    (make-cwm-centering-instructions
     :window window
     :left-width (car widths)
     :right-width (cdr widths))))

(defun cwm-calculate-appropriate-fringe-widths (window)
  (let* ((mode-active-p (with-current-buffer (window-buffer window) centered-window-mode))
         (pixel (frame-char-width (window-frame window)))
         (n  (if mode-active-p
                 (/ (max (- (window-total-width window) cwm-centered-window-width) 0)
                    2)
               0))
         (ratio (/ (* n cwm-left-fringe-ratio) 100))
         (left-width (* pixel (if (> n 0) (+ n ratio) n)))
         (right-width (* pixel (if (> n 0) (- n ratio) n))))
    `(,left-width . ,right-width)))

;;;###autoload
(defun centered-window-mode-toggle ()
  (if centered-window-mode
      (centered-window-mode -1)
    (centered-window-mode +1)))

;;;###autoload
(define-minor-mode centered-window-mode
  "Minor mode to center text on the current buffer"
  :init-value nil
  :global t
  :lighter cwm-lighter
  (if centered-window-mode (cwm-turn-on) (cwm-turn-off)))

(provide 'centered-window-mode)
;;; centered-window-mode.el ends here
