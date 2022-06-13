;;; sideline.el --- Show informations on the side  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-06-13 22:08:26

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Show informations on the side
;; Keyword: sideline
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/jcs-elpa/sideline

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Show informations on the side.
;;

;;; Code:

(require 'rect)
(require 'subr-x)

(defgroup sideline nil
  "Show informations on the side."
  :prefix "sideline-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/sideline"))

(defcustom sideline-backends-left nil
  "The list of active backends to display sideline on the left."
  :type 'list
  :group 'sideline)

(defcustom sideline-backends-right nil
  "The list of active backends to display sideline on the right."
  :type 'list
  :group 'sideline)

(defface sideline-default
  '((((background light)) :foreground "DarkOrange")
    (t :foreground "yellow"))
  "Face used to highlight action text."
  :group 'sideline)

(defcustom sideline-backends-skip-current-line nil
  "Don't display at line."
  :type 'boolean
  :group 'sideline)

(defcustom sideline-format "   %s"
  "Format entire candidate string."
  :type 'string
  :group 'sideline)

(defvar-local sideline--overlays nil
  "Displayed overlays.")

(defvar-local sideline--last-bounds nil
  "Record of last bound; if this isn't the same, clean up overlays.")

(defvar-local sideline--occupied-lines-left nil
  "List of lines occupied by an overlay on the left.")

(defvar-local sideline--occupied-lines-right nil
  "List of lines occupied by an overlay on the left.")

;;
;; (@* "Util" )
;;

(defun sideline--line-number-display-width ()
  "Safe way to get value from function `line-number-display-width'."
  (if (bound-and-true-p display-line-numbers-mode)
      (+ (or (ignore-errors (line-number-display-width)) 0) 2)
    0))

(defun sideline--margin-width ()
  "General calculation of margin width."
  (+ (if fringes-outside-margins right-margin-width 0)
     (or (and (boundp 'fringe-mode)
              (consp fringe-mode)
              (or (equal (car fringe-mode) 0)
                  (equal (cdr fringe-mode) 0))
              1)
         (and (boundp 'fringe-mode) (equal fringe-mode 0) 1)
         0)
     (let ((win-fringes (window-fringes)))
       (if (or (equal (car win-fringes) 0)
               (equal (cadr win-fringes) 0))
           2
         0))
     (if (< emacs-major-version 27)
         ;; This was necessary with emacs < 27, recent versions take
         ;; into account the display-line width with :align-to
         (sideline--line-number-display-width)
       0)
     (if (or (bound-and-true-p whitespace-mode)
             (bound-and-true-p global-whitespace-mode))
         1
       0)))

(defun sideline--window-width ()
  "Correct window width for sideline."
  (- (min (window-text-width) (window-body-width))
     (sideline--margin-width)
     (or (and (>= emacs-major-version 27)
              ;; We still need this number when calculating available space
              ;; even with emacs >= 27
              (sideline--line-number-display-width))
         0)))

(defun sideline--align (&rest lengths)
  "Align sideline string by LENGTHS from the right of the window."
  (+ (apply '+ lengths)
     (if (display-graphic-p) 1 2)))

(defun sideline--compute-height nil
  "Return a fixed size for text in sideline."
  (if (null text-scale-mode-remapping)
      '(height 1)
    ;; Readjust height when text-scale-mode is used
    (list 'height
          (/ 1 (or (plist-get (cdr text-scale-mode-remapping) :height)
                   1)))))

(defun sideline--calc-space (str-len on-left)
  "Calculate space in current line.

Argument STR-LEN is the string size.

If argument ON-LEFT is non-nil, we calculate to the left side.  Otherwise,
calculate to the right side."
  (if on-left
      (let ((column-start (window-hscroll))
            (pos-first (progn (back-to-indentation) (current-column))))
        (< str-len (- pos-first column-start)))
    (let* ((column-start (window-hscroll))
           (column-end (+ column-start (sideline--window-width)))
           (pos-end (progn (move-end-of-line nil) (current-column))))
      (< str-len (- column-end pos-end)))))

(defun sideline--find-line (str-len on-left)
  "Find a line where the string can be inserted."
  (let ((inhibit-field-text-motion t))
    (save-excursion

      )))

;;
;; (@* "Overlays" )
;;

(defun sideline--delete-ovs ()
  "Clean up all overlays."
  (mapc #'delete-overlay sideline--overlays))

(defun sideline--create-ov (candidate action face on-left)
  "Create information (CANDIDATE) overlay."
  (when-let*
      ((title
        (progn
          (add-face-text-property 0 len face nil candidate)
          (when action
            (let* ((map (make-sparse-keymap))
                   (keymap (progn (define-key map [down-mouse-1]
                                              (lambda ()
                                                (interactive)
                                                (funcall action candidate)))
                                  map)))
              (add-text-properties 0 len `(keymap ,keymap mouse-face highlight) candidate)))
          (format sideline-format candidate)))
       (align (if on-left 'left-fringe 'right-fringe))
       (string (concat
                (propertize " " 'display `(space :align-to (- ,align ,(sideline--align (+ len (length image)) margin))))
                (propertize title 'display (sideline--compute-height))))
       (pos-ov (sideline--find-line (1+ (length title)) on-left))
       (ov (make-overlay (car pos-ov) (car pos-ov))))
    (overlay-put ov 'after-string string)
    (overlay-put ov 'intangible t)
    (overlay-put ov 'position (car pos-ov))
    (overlay-put ov 'window (get-buffer-window))
    (push ov sideline--overlays)))

;;
;; (@* "Core" )
;;

(defun sideline--call-backend (backend command on-left)
  "Return BACKEND's result with COMMAND."
  (funcall backend command))

(defun sideline--render-backends (backends on-left)
  "Render a list of backends."
  (dolist (backend backends)
    (let ((candidates (sideline--call-backend backend 'candidates))
          (action (sideline--call-backend backend 'action))
          (face (or (sideline--call-backend backend 'face) sideline-default)))
      (dolist (candidate candidates)
        (sideline--create-ov candidate action face on-left)))))

(defun sideline--post-command ()
  "Post command."
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (unless (equal sideline--last-bounds bound)
      (sideline--delete-ovs)
      (setq sideline--last-bounds bound)))
  (sideline--render-backends sideline-backends-left t)
  (sideline--render-backends sideline-backends-right nil))

;;
;; (@* "Entry" )
;;

(defun sideline--enable ()
  "Enable `sideline' in current buffer."
  (add-hook 'post-command-hook #'sideline--post-command nil t))

(defun sideline--disable ()
  "Disable `sideline' in current buffer."
  (remove-hook 'post-command-hook #'sideline--post-command t)
  (sideline--delete-ovs))

;;;###autoload
(define-minor-mode sideline-mode
  "Minor mode 'sideline-mode'."
  :lighter " Sideline"
  :group sideline
  (if sideline-mode (sideline--enable) (sideline--disable)))

(defun sideline--turn-on-sideline-mode ()
  "Turn on the 'sideline-mode'."
  (sideline-mode 1))

;;;###autoload
(define-globalized-minor-mode global-sideline-mode
  sideline-mode sideline--turn-on-sideline-mode
  :require 'sideline)

(provide 'sideline)
;;; sideline.el ends here
