;;; centered-window.el --- Center the text when there's only one window  -*- lexical-binding: t; -*-
;;
;; Author: Anler Hernández Peral <inbox+emacs@anler.me>
;; Version: 1.4.0
;; Contributors:
;;    Mickael Kerjean <https://github.com/mickael-kerjean>
;;    Pierre Lecocq   <https://github.com/pierre-lecocq>
;;    Syohei YOSHIDA  <https://github.com/syohex>
;;    Lars Tveito     <https://github.com/larstvei>
;;    Tianxiang Xiong <https://github.com/xiongtx>
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
;;  cwm-incremental-padding
;;  cwm-incremental-padding-%
;;  cwm-use-vertical-padding
;;  cwm-frame-internal-border
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'face-remap)
(require 'subr-x)
(require 'mac-win nil t)
(require 'mwheel nil t)

(defgroup centered-window nil
  "Center text in windows."
  :group 'windows
  :prefix "cwm-")

(defcustom cwm-lighter
  " #"
  "Mode's lighter used in the mode line."
  :group 'centered-window
  :type 'string)

(defcustom cwm-centered-window-width
  110
  "Minimum line length required to apply the margins."
  :group 'centered-window
  :initialize #'custom-initialize-default
  :set #'cwm--set-and-recenter-windows
  :type 'integer)

(defcustom cwm-incremental-padding
  nil
  "If t even when the window's width is less than `cwm-centered-window-width' a padding of `cwm-incremental-padding-%' will be applied to each side."
  :group 'centered-window
  :type 'boolean)

(defcustom cwm-incremental-padding-%
	0
  "Incremental padding percentage to use when `cwm-incremental-padding' is t."
  :group 'centered-window
  :type 'integer)

(defcustom cwm-use-vertical-padding
  nil
  "Whether or not use experimental vertical padding."
  :group 'centered-window
  :initialize #'custom-initialize-default
  :set #'cwm--set-and-recenter-windows
  :type 'boolean)

(defcustom cwm-frame-internal-border
  5
  "Frame internal border to use when vertical padding is used."
  :group 'centered-window
  :initialize #'custom-initialize-default
  :set #'cwm--set-and-recenter-windows
  :type 'integer)

(defcustom cwm-left-fringe-ratio
  0
  "Ratio by which the left fringe is padded more than the right.
Should be a value between 0 and 100. A value of 0 means off."
  :group 'centered-window
  :initialize #'custom-initialize-default
  :set #'cwm--set-and-recenter-windows
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
  :group 'centered-window
  :type '(list function))

(define-obsolete-variable-alias
  'centered-window-mode-hooks
  'cwm-hooks "1.3.0")
(defcustom cwm-hooks
  nil
  "Hooks to run every time window is centered (be careful)."
  :group 'centered-window
  :type 'hook)

(defun cwm--set-and-recenter-windows (var val)
  "Set customizable variable VAR to VAL and recenter windows.

All windows in all frames are recentered.

This is intended for use as the `setfunction' of a
`defcustom'. See Info node `(elisp) Variable Definitions'."
  (set-default var val)
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (cwm-center-windows))))

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
  (add-hook 'window-size-change-functions #'cwm-center-windows-frame)
  (cwm-center-windows)
  (when cwm-use-vertical-padding
    (set-frame-parameter nil 'internal-border-width cwm-frame-internal-border))
  (cwm-bind-fringe-mouse-events))

(defun cwm-turn-off ()
  (remove-hook 'window-configuration-change-hook #'cwm-center-windows)
  (remove-hook 'window-size-change-functions #'cwm-center-windows-frame)
  (cwm-center-windows)
  (set-frame-parameter nil 'internal-border-width 0)
  (cwm-unbind-fringe-mouse-events))

(defun cwm-center-windows-frame (frame)
  (when (frame-size-changed-p frame)
    (cwm-center-windows)))

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
         (window-width (window-total-width window))
         (n  (if mode-active-p
               (max
                (/ (- window-width cwm-centered-window-width)
                   2)
                (if cwm-incremental-padding
                    (/ (* window-width cwm-incremental-padding-%)
                       100)
                  0))
               0))
         (ratio (/ (* n cwm-left-fringe-ratio) 100))
         (left-width (* pixel (if (> n 0) (+ n ratio) n)))
         (right-width (* pixel (if (> n 0) (- n ratio) n))))
    `(,left-width . ,right-width)))

(defun cwm-toggle-bind-fringe-mouse-events (&optional bind direction-command-alist)
  (dolist (fringe '("left" "right"))
    (dolist (wheel-speed '("" "double" "triple"))
      (dolist (scroll-direction '("left" "right" "up" "down"))
        (let ((key-name (kbd (concat
                              "<" fringe "-fringe> "
                              "<" wheel-speed
                              (if (string= wheel-speed "") "" "-")
                              "wheel-" scroll-direction ">"))))
          (if bind
              (global-set-key key-name
                              (alist-get (intern-soft scroll-direction)
                                         direction-command-alist))
            (global-unset-key key-name)))))))

(defun cwm-bind-fringe-mouse-events ()
  (cond ((and (eq window-system 'mac) (featurep 'mac-win))
         (cwm-toggle-bind-fringe-mouse-events
          t
          '((left  . mac-mwheel-scroll)
            (right . mac-mwheel-scroll)
            (up    . mac-mwheel-scroll)
            (down  . mac-mwheel-scroll))))
        ((eq window-system nil) nil)
        (t
         (cwm-toggle-bind-fringe-mouse-events
          t
          '((left  . mwheel-scroll)
            (right . mwheel-scroll)
            (up    . mwheel-scroll)
            (down  . mwheel-scroll))))))

(defun cwm-unbind-fringe-mouse-events ()
  (cond ((and (eq window-system 'mac) (featurep 'mac-win))
         (cwm-toggle-bind-fringe-mouse-events nil))
        ((eq window-system nil) nil)
        (t (cwm-toggle-bind-fringe-mouse-events nil))))

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
(provide 'centered-window)
;;; centered-window.el ends here
