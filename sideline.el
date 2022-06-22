;;; sideline.el --- Show informations on the side  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shen, Jen-Chieh
;; Created date 2022-06-13 22:08:26

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/sideline
;; Version: 0.1.1
;; Package-Requires: ((emacs "29"))
;; Keywords: sideline

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

(require 'cl-lib)
(require 'face-remap)
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

(defcustom sideline-order-left 'down
  "Display order on the left sidelines."
  :type '(choice (const :tag "Search up" up)
                 (const :tag "Search down" down))
  :group 'sideline)

(defcustom sideline-order-right 'up
  "Display order on the right sidelines."
  :type '(choice (const :tag "Search up" up)
                 (const :tag "Search down" down))
  :group 'sideline)

(defface sideline-default
  '((((background light)) :foreground "DarkOrange")
    (t :foreground "yellow"))
  "Face used to highlight action text."
  :group 'sideline)

(defcustom sideline-backends-left-skip-current-line t
  "Don't display left sideline in current line."
  :type 'boolean
  :group 'sideline)

(defcustom sideline-backends-right-skip-current-line t
  "Don't display right sideline in current line."
  :type 'boolean
  :group 'sideline)

(defcustom sideline-format-left "%s   "
  "Format candidate string for left alignment."
  :type 'string
  :group 'sideline)

(defcustom sideline-format-right "   %s"
  "Format candidate string for right alignment."
  :type 'string
  :group 'sideline)

(defcustom sideline-priority 100
  "Overlays' priority."
  :type 'integer
  :group 'sideline)

(defcustom sideline-delay 0.2
  "Number of seconds to wait before showing sideline."
  :type 'number
  :group 'sideline)

(defcustom sideline-pre-render-hook nil
  "Hooks runs before rendering sidelines."
  :type 'hook
  :group 'sideline)

(defcustom sideline-post-render-hook nil
  "Hooks runs after rendering sidelines."
  :type 'hook
  :group 'sideline)

(defcustom sideline-reset-hook nil
  "Hooks runs once the sideline is reset in `post-command-hook'."
  :type 'hook
  :group 'sideline)

(defcustom sideline-inhibit-display-function #'sideline-stop-p
  "Function call to determine weather to display sideline or not."
  :type 'function
  :group 'sideline)

(defvar-local sideline--overlays nil
  "Displayed overlays.")

(defvar-local sideline--ex-bound-or-point nil
  "Record of last bound; if this isn't the same, clean up overlays.")

(defvar-local sideline--occupied-lines-left nil
  "Occupied lines on the left.")

(defvar-local sideline--occupied-lines-right nil
  "Occupied lines on the right.")

(defvar-local sideline--text-scale-mode-amount nil
  "Record of last variable `text-scale-mode-amount'.")

;;
;; (@* "Entry" )
;;

(defun sideline--enable ()
  "Enable `sideline' in current buffer."
  (setq sideline--ex-bound-or-point t  ; render immediately
        sideline--text-scale-mode-amount text-scale-mode-amount)
  (add-hook 'post-command-hook #'sideline--post-command nil t))

(defun sideline--disable ()
  "Disable `sideline' in current buffer."
  (remove-hook 'post-command-hook #'sideline--post-command t)
  (sideline--reset))

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

;;
;; (@* "Util" )
;;

(defmacro sideline--with-buffer (buffer-or-name &rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p ,buffer-or-name)
     (with-current-buffer ,buffer-or-name ,@body)))

(defun sideline--kill-timer (timer)
  "Kill TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun sideline--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (string-pixel-width " "))
        (len (string-pixel-width (substring-no-properties str))))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun sideline--line-str ()
  "Return line string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun sideline--line-length ()
  "Return the length of current line."
  (sideline--str-len (sideline--line-str)))

(defun sideline--window-width ()
  "Correct window width for sideline."
  (window-max-chars-per-line))

(defun sideline--compute-height ()
  "Return a fixed size for text in sideline."
  (if (null text-scale-mode-remapping) 1
    ;; Readjust height when text-scale-mode is used
    (or (plist-get (cdar text-scale-mode-remapping) :height) 1)))

(defun sideline--apply-text-scale (val)
  "Calculate VAL with text-scale applied."
  (ceiling (/ (float val) (sideline--compute-height))))

(defun sideline--window-hscroll ()
  "Return correct hscroll."
  (sideline--apply-text-scale (window-hscroll)))

(defun sideline--calc-space (str-len on-left)
  "Calculate space in current line.

Argument STR-LEN is the string size.

If argument ON-LEFT is non-nil, we calculate to the left side.  Otherwise,
calculate to the right side."
  (if on-left
      (let ((left-edge (sideline--window-hscroll))
            (pos-first (save-excursion (back-to-indentation) (current-column)))
            (line-len (sideline--line-length)))
        (cond ((< str-len (- pos-first left-edge))
               (cons (line-beginning-position) pos-first))
              ((= pos-first line-len)
               (cons (line-beginning-position) (line-beginning-position)))))
    (let* ((left-edge (sideline--window-hscroll))
           (right-edge (+ left-edge (sideline--window-width)))
           (line-len (sideline--line-length)))
      (when (< str-len (- right-edge line-len))
        (cons (line-end-position) (line-end-position))))))

(defun sideline--find-line (str-len on-left &optional direction exceeded)
  "Find a line where the string can be inserted.

Argument STR-LEN is the length of the message, use to calculate the alignment.

If argument ON-LEFT is non-nil, it will align to the left instead of right.

See variable `sideline-order' document string for optional argument DIRECTION
for details.

Optional argument EXCEEDED is set to non-nil when we have already searched
available lines in both directions (up & down)."
  (let ((bol (window-start)) (eol (window-end))
        (occupied-lines (if on-left sideline--occupied-lines-left
                          sideline--occupied-lines-right))
        (going-up (eq direction 'up))
        (skip-first t)
        (break-it)
        (pos-ov))
    (save-excursion
      (while (not break-it)
        (if skip-first (setq skip-first nil)
          (forward-line (if going-up -1 1)))
        (unless (if going-up (<= bol (point)) (<= (point) eol))
          (setq break-it t))
        (when (and (not (memq (line-beginning-position) occupied-lines))
                   (not break-it))
          (when-let ((result (sideline--calc-space str-len on-left)))
            (setq pos-ov result
                  break-it t)
            (push (line-beginning-position) occupied-lines)))
        (when (if going-up (bobp) (eobp)) (setq break-it t))))
    (if on-left
        (setq sideline--occupied-lines-left occupied-lines)
      (setq sideline--occupied-lines-right occupied-lines))
    (or pos-ov
        (and (not exceeded)
             (sideline--find-line str-len on-left (if going-up 'down 'up) t)))))

(defun sideline--create-keymap (action candidate)
  "Create keymap for sideline ACTION.

Argument CANDIDATE is the data for users."
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1]
                (lambda ()
                  (interactive)
                  (funcall action candidate)))
    map))

;;
;; (@* "Overlays" )
;;

(defun sideline--overlays-in (prop name &optional beg end)
  "Return overlays with PROP of NAME, from region BEG to END."
  (let* ((beg (or beg (line-beginning-position)))
         (end (or end (line-end-position)))
         (ovs (overlays-in beg end))
         lst)
    (dolist (ov ovs) (when (eq name (overlay-get ov prop)) (push ov lst)))
    lst))

(defun sideline--delete-ovs ()
  "Clean up all overlays."
  (mapc #'delete-overlay sideline--overlays))

(defun sideline--create-ov (candidate action face on-left order)
  "Create information (CANDIDATE) overlay.

See function `sideline--render-candidates' document string for arguments ACTION,
FACE, ON-LEFT, and ORDER for details."
  (when-let*
      ((len-cand (length candidate))
       (title
        (progn
          (unless (get-text-property 0 'face candidate)  ; If no face, we apply one
            (add-face-text-property 0 len-cand face nil candidate))
          (when action
            (let ((keymap (sideline--create-keymap action candidate)))
              (add-text-properties 0 len-cand `(keymap ,keymap mouse-face highlight) candidate)))
          (if on-left (format sideline-format-left candidate)
            (format sideline-format-right candidate))))
       (len-title (sideline--str-len title))
       (pos-ov (sideline--find-line len-title on-left order))
       (pos-start (car pos-ov)) (pos-end (cdr pos-ov))
       (str (concat
             (propertize
              (if on-left
                  (spaces-string (sideline--window-hscroll))
                (let* ((column-start (sideline--window-hscroll))
                       (right-edge (+ column-start (sideline--window-width)))
                       (line-len (save-excursion
                                   (goto-char pos-start)
                                   (sideline--line-length)))
                       (hidden-spaces (max (- column-start line-len) 0))
                       (left-edge (max line-len column-start))
                       (gap (max (+ (- right-edge left-edge len-title) hidden-spaces)
                                 0)))
                  (spaces-string gap)))
              `cursor t)
             title)))
    ;; Create overlay
    (let* ((len-str (length str))
           (empty-ln (= pos-start pos-end))
           (ov (make-overlay pos-start (if empty-ln pos-start (+ pos-start len-str))
                             nil t t)))
      (save-excursion
        (goto-char pos-start)
        (when-let ((oov (nth 0 (sideline--overlays-in 'creator 'sideline))))
          (move-overlay oov pos-start pos-end)
          (if (overlay-get oov 'on-left)
              (setq str (substring str (sideline--str-len (overlay-get oov 'after-string))))
            (overlay-put oov 'after-string (substring (overlay-get oov 'after-string) len-str)))))
      (cond (on-left
             (if empty-ln
                 (overlay-put ov 'after-string str)
               (overlay-put ov 'display str)
               (overlay-put ov 'invisible t)))
            (t (overlay-put ov 'after-string str)))
      (overlay-put ov 'window (get-buffer-window))
      (overlay-put ov 'priority sideline-priority)
      (overlay-put ov 'on-left on-left)
      (overlay-put ov 'creator 'sideline)
      (push ov sideline--overlays))))

;;
;; (@* "Async" )
;;

(defun sideline--render-candidates (candidates action face on-left order)
  "Render a list of backends (CANDIDATES).

Argument ACTION is the code action callback.

Argument FACE is optional face to render text; default face is
`sideline-default'.

Argument ON-LEFT is a flag indicates rendering alignment; if it's non-nil then
we align to the left, otherwise to the right.

Argument ORDER determined the search order for going up or down."
  (let ((inhibit-field-text-motion t))
    (dolist (candidate candidates)
      (sideline--create-ov candidate action face on-left order))))

;;
;; (@* "Core" )
;;

(defun sideline--call-backend (backend command)
  "Return BACKEND's result with COMMAND."
  (funcall backend command))

(defun sideline--render-backends (backends on-left)
  "Render a list of BACKENDS.

If argument ON-LEFT is non-nil, it will align to the left instead of right."
  (dolist (data backends)
    (let* ((is-cons (consp data))
           (backend (if is-cons (car data) data))
           (order (if is-cons (cdr data)  ; configured
                    ;; fallback to default
                    (if on-left sideline-order-left sideline-order-right)))
           (candidates (sideline--call-backend backend 'candidates))
           (action (sideline--call-backend backend 'action))
           (face (or (sideline--call-backend backend 'face) 'sideline-default))
           (buffer (current-buffer)))  ; for async check
      (if (eq (car candidates) :async)
          (funcall (cdr candidates)
                   (lambda (cands &rest _)
                     (sideline--with-buffer buffer
                                            (when sideline-mode
                                              (sideline--render-candidates cands action face on-left order)))))
        (sideline--render-candidates candidates action face on-left order)))))

(defun sideline-stop-p ()
  "Return non-nil if the sideline should not be display."
  (or (region-active-p)
      (bound-and-true-p company-pseudo-tooltip-overlay)
      (bound-and-true-p lsp-ui-peek--overlay)))

(defun sideline-render (&optional buffer)
  "Render sideline once in the BUFFER."
  (sideline--with-buffer (or buffer (current-buffer))
                         (unless (funcall sideline-inhibit-display-function)
                           (let ((mark (list (line-beginning-position))))
                             (setq sideline--occupied-lines-left
                                   (if sideline-backends-left-skip-current-line mark nil))
                             (setq sideline--occupied-lines-right
                                   (if sideline-backends-right-skip-current-line mark nil)))
                           (sideline--delete-ovs)  ; for function call externally
                           (run-hooks 'sideline-pre-render-hook)
                           (sideline--render-backends sideline-backends-left t)
                           (sideline--render-backends sideline-backends-right nil)
                           (run-hooks 'sideline-post-render-hook))))

(defvar-local sideline--delay-timer nil
  "Timer for delay.")

(defvar-local sideline--ex-window-start nil
  "Holds previouse window start point; this will detect vertical scrolling.")

(defvar-local sideline--ex-window-hscroll nil
  "Holds previouse window hscroll; this will detect horizontal scrolling.")

(defun sideline--do-render-p ()
  "Return non-nil if we should re-render sidelines in the post-command."
  (let ((bound-or-point (or (bounds-of-thing-at-point 'symbol) (point)))
        (win-start (window-start))
        (win-hscroll (window-hscroll)))
    (when  ; conditions allow to re-render sidelines
        (or (not (equal sideline--ex-bound-or-point bound-or-point))
            (not (equal sideline--text-scale-mode-amount text-scale-mode-amount))
            (not (equal sideline--ex-window-start win-start))
            (not (equal sideline--ex-window-hscroll win-hscroll)))
      ;; update
      (setq sideline--ex-bound-or-point bound-or-point
            sideline--text-scale-mode-amount text-scale-mode-amount
            sideline--ex-window-start win-start
            sideline--ex-window-hscroll win-hscroll)
      t)))

(defun sideline--post-command ()
  "Post command."
  (when (sideline--do-render-p)
    (sideline--delete-ovs)
    (sideline--kill-timer sideline--delay-timer)
    (setq sideline--delay-timer
          (run-with-idle-timer sideline-delay nil #'sideline-render (current-buffer)))
    (run-hooks 'sideline-reset-hook)))

(defun sideline--reset ()
  "Clean up for next use."
  (setq sideline--ex-bound-or-point nil)
  (sideline--delete-ovs))

(provide 'sideline)
;;; sideline.el ends here
