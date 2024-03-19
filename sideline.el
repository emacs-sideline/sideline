;;; sideline.el --- Show information on the side  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Shen, Jen-Chieh
;; Created date 2022-06-13 22:08:26

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-sideline/sideline
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1") (ht "2.4"))
;; Keywords: convenience

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
;; This library provides the frontend UI to display information either on the
;; left/right side of the buffer window.
;;
;; 1) You would need to first set up the backends,
;;
;;   (setq sideline-backends-left '(sideline-flycheck))
;;
;; 2) Then enable the sideline in the target buffer,
;;
;;   M-x sideline-mode
;;
;; For backends choice, see https://github.com/emacs-sideline/sideline#-example-projects
;;

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'rect)
(require 'subr-x)

(require 'ht)

(defgroup sideline nil
  "Show information on the side."
  :prefix "sideline-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-sideline/sideline"))

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

(defface sideline-backend
  '((((background light)) :foreground "#7F7F7F")
    (t :foreground "#9B9B9B"))
  "Face used to highlight action text."
  :group 'sideline)

(defcustom sideline-display-backend-name nil
  "Weather to display backend name in the candidate."
  :type 'boolean
  :group 'sideline)

(defcustom sideline-display-backend-type 'outer
  "Method type to display backend name."
  :type '(choice (const :tag "Display on left" left)
                 (const :tag "Display on right" right)
                 (const :tag "Display on inner" inner)
                 (const :tag "Display on outer" outer))
  :group 'sideline)

(defcustom sideline-display-backend-format "[%s]"
  "Format string for candidate and backend name."
  :type 'string
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

(defvar-local sideline--overlays (make-hash-table)
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
;; (@* "Externals" )
;;

(defvar overflow-newline-into-fringe)

(declare-function string-pixel-width "subr-x.el")   ; TODO: remove this after 29.1
(declare-function shr-string-pixel-width "shr.el")  ; TODO: remove this after 29.1

;;
;; (@* "Entry" )
;;

(defun sideline--enable ()
  "Enable `sideline' in current buffer."
  ;; XXX: Still don't know why local variable doesn't work!
  (progn
    (sideline--delete-ovs)
    (setq-local sideline--overlays (make-hash-table)))
  (setq sideline--ex-bound-or-point t  ; render immediately
        sideline--text-scale-mode-amount text-scale-mode-amount)
  (add-hook 'post-command-hook #'sideline--post-command nil t))

(defun sideline--disable ()
  "Disable `sideline' in current buffer."
  (remove-hook 'post-command-hook #'sideline--post-command t)
  (sideline--reset))

;;;###autoload
(define-minor-mode sideline-mode
  "Minor mode `sideline-mode'."
  :lighter " Sideline"
  :group sideline
  (if sideline-mode (sideline--enable) (sideline--disable)))

(defun sideline--turn-on-sideline-mode ()
  "Turn on the `sideline-mode'."
  (sideline-mode 1))

;;;###autoload
(define-globalized-minor-mode global-sideline-mode
  sideline-mode sideline--turn-on-sideline-mode
  :require 'sideline)

;;
;; (@* "Util" )
;;

;; Copied from s.el
(defun sideline--s-replace (old new s)
  "Replace OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defmacro sideline--with-buffer (buffer-or-name &rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p ,buffer-or-name)
     (with-current-buffer ,buffer-or-name ,@body)))

;; TODO: Use function `string-pixel-width' after 29.1
(defun sideline--string-pixel-width (str)
  "Return the width of STR in pixels."
  ;; Text properties may effect the length, remove it!
  (let ((str (substring-no-properties str)))
    (if (fboundp #'string-pixel-width)
        (string-pixel-width str)
      (require 'shr)
      (shr-string-pixel-width str))))

(defun sideline--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (sideline--string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun sideline--kill-timer (timer)
  "Kill TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun sideline--column-to-point (column)
  "Convert COLUMN to point."
  (save-excursion (move-to-column (max column 0)) (point)))

(defun sideline--modeline-height ()
  "Return lines modeline cost."
  (ceiling (/ (float (window-mode-line-height)) (frame-char-height))))

(defun sideline--window-end ()
  "Return the accurate window end position."
  (save-excursion
    (goto-char (window-end))
    (forward-line (- 0 (sideline--modeline-height)))
    (line-beginning-position)))

(defun sideline--window-width ()
  "Correct window width for sideline."
  (window-max-chars-per-line))

(defun sideline--overlays-in (prop name &optional beg end)
  "Return overlays with PROP of NAME, from region BEG to END."
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (lst)
         (ovs (overlays-in beg end)))
    (dolist (ov ovs)
      (when (eq name (overlay-get ov prop))
        (push ov lst)))
    lst))

(defun sideline--overlays-in-line ()
  "Return sideline overlays in line."
  (sideline--overlays-in 'creator 'sideline (line-beginning-position) (line-end-position)))

(defun sideline--opposing-str-len ()
  "Return opposing overlay's content length."
  (if-let* ((ov (car (sideline--overlays-in-line)))  ; always return length of 1
            (str (overlay-get ov 'before-string)))
      (sideline--str-len str)
    0))  ; not found, then return 0

(defun sideline--align-right (str offset)
  "Align sideline STR from the right of the window.

Argument OFFSET is additional calculation from the right alignment."
  (let ((graphic-p (display-graphic-p))
        (fringes (window-fringes)))
    (list (+
           ;; If the sideline text is displayed without at least 1 pixel gap from the right fringe and
           ;; overflow-newline-into-fringe is not true, emacs will line wrap it.
           (if (and graphic-p
                    (> (nth 1 fringes) 0)
                    (not overflow-newline-into-fringe))
               1
             0)
           (* (window-font-width)
              (+ offset (if graphic-p
                            ;; If right fringe deactivated add 1 offset
                            (if (= 0 (nth 1 fringes)) 1 0)
                          1)
                 (sideline--str-len str)))))))

(defun sideline--get-line ()
  "Return current line."
  (sideline--s-replace "\n" "" (thing-at-point 'line t)))

(defun sideline--calc-space (str-len on-left opposing-str-len)
  "Calculate space in current line.

Argument STR-LEN is the string size.  Another argument OPPOSING-STR-LEN is the
string size already occupied.

If argument ON-LEFT is non-nil, we calculate to the left side.  Otherwise,
calculate to the right side."
  ;; XXX: We add up the `str-len' with `opposing-str-len' because they are using
  ;; the same line!
  ;;
  ;; This is smart since we add up the string size before the calculation!
  (setq str-len (+ str-len opposing-str-len))
  ;; Start the calculation!
  (if on-left
      (let* ((line (sideline--get-line))
             (column-start (window-hscroll))
             (pos-first (save-excursion (back-to-indentation) (current-column)))
             (pos-end (- (sideline--str-len line) column-start)))
        (cond ((<= str-len (- pos-first column-start))
               (cons column-start pos-first))
              ((= pos-first pos-end)
               (cons column-start (sideline--window-width)))))
    (let* ((line (sideline--get-line))
           (column-start (window-hscroll))
           (column-end (+ column-start (sideline--window-width)))
           (pos-end (- (sideline--str-len line) column-start)))
      (when (<= str-len (- column-end pos-end))
        (cons column-end pos-end)))))

(defun sideline--find-line (str-len on-left bol eol &optional direction exceeded)
  "Find a line where the string can be inserted.

Argument STR-LEN is the length of the message, use to calculate the alignment.

If argument ON-LEFT is non-nil, it will align to the left instead of right.

Arguments BOL and EOL are cache early for better performance.

See variable `sideline-order' document string for optional argument DIRECTION
for details.

Optional argument EXCEEDED is set to non-nil when we have already searched
available lines in both directions (up & down)."
  (let ((occupied-lines (if on-left sideline--occupied-lines-left
                          sideline--occupied-lines-right))
        (going-up (eq direction 'up))
        (skip-first t)
        (break-it)
        (data))
    (save-excursion
      (while (not break-it)
        (if skip-first (setq skip-first nil)
          (forward-line (if going-up -1 1)))
        (unless (if going-up (<= bol (point)) (<= (point) eol))
          (setq break-it t))
        (when-let* ((occ-bol (line-beginning-position))
                    ((and (not (memq occ-bol occupied-lines))
                          (not break-it)))
                    (col (sideline--calc-space str-len on-left (sideline--opposing-str-len))))
          (setq data (list (sideline--column-to-point (car col))
                           (sideline--column-to-point (cdr col))
                           occ-bol))
          (setq break-it t)
          (push occ-bol occupied-lines))
        (when (if going-up (bobp) (eobp)) (setq break-it t))))
    (if on-left
        (setq sideline--occupied-lines-left occupied-lines)
      (setq sideline--occupied-lines-right occupied-lines))
    (or data
        (and (not exceeded)
             (sideline--find-line str-len on-left bol eol (if going-up 'down 'up) t)))))

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

(defun sideline-backend-ovs (backend)
  "Return overlays for BACKEND."
  (sideline--overlays-in 'backend backend))

(defun sideline-delete-backend-ovs (backend)
  "Delete overlays from BACKEND."
  (dolist (ov (ht-get sideline--overlays backend))
    (let ((on-left (overlay-get ov 'left))
          (occ-pt (overlay-get ov 'occ-pt)))
      (if on-left
          (setq sideline--occupied-lines-left
                (delete occ-pt sideline--occupied-lines-left))
        (setq sideline--occupied-lines-right
              (delete occ-pt sideline--occupied-lines-right))))
    (delete-overlay ov))
  (ht-set sideline--overlays backend nil))

(defun sideline--reset-occupied-lines ()
  "Reset occupied lines."
  (let ((mark (list (line-beginning-position))))
    (setq sideline--occupied-lines-left
          (if sideline-backends-left-skip-current-line mark nil))
    (setq sideline--occupied-lines-right
          (if sideline-backends-right-skip-current-line mark nil))))

(defun sideline--delete-ovs ()
  "Clean up all overlays."
  (sideline--reset-occupied-lines)
  (ht-map (lambda (_key value)
            (mapc #'delete-overlay value))
          sideline--overlays)
  (ht-clear sideline--overlays))

(defun sideline--display-string (on-left backend-str candidate &optional type)
  "Return the display string to render the text correctly.

Argument ON-LEFT is used to calculate the output string.

Arguments BACKEND-STR and CANDIDATE are used to string concatenation, it
produces the result string.

Optional argument TYPE is used for recursive `outer' and `inner'."
  (cl-case (or type sideline-display-backend-type)
    (`left (concat backend-str " " candidate))
    (`right (concat candidate " " backend-str))
    (`inner (sideline--display-string on-left backend-str candidate (if on-left 'right 'left)))
    (`outer (sideline--display-string on-left backend-str candidate (if on-left 'left 'right)))))

(defun sideline--display-starting (on-left backend-str &optional type)
  "Return the starting text position to render the text correctly.

Argument ON-LEFT is used to calculate the starting text position..

Argument BACKEND-STR is used to calculate the starting text position.

Optional argument TYPE is used for recursive `outer' and `inner'."
  (cl-case (or type sideline-display-backend-type)
    (`left (1+ (length backend-str)))
    (`right 0)
    (`inner (sideline--display-starting on-left backend-str (if on-left 'right 'left)))
    (`outer (sideline--display-starting on-left backend-str (if on-left 'left 'right)))))

(defun sideline--create-ov (backend candidate action face name on-left order bol eol)
  "Create information (CANDIDATE) overlay.

Argument BACKEND is used to categorize overlays.

See function `sideline--render-candidates' document string for arguments ACTION,
FACE, NAME, ON-LEFT, and ORDER for details.

Arguments BOL and EOL are cached for faster performance."
  (when-let*
      ((backend-str (format sideline-display-backend-format name))
       (text (if sideline-display-backend-name  ; this is the displayed text
                 (progn
                   (add-face-text-property 0 (length backend-str) 'sideline-backend nil backend-str)
                   (sideline--display-string on-left backend-str candidate))
               candidate))
       (len-text (length text))
       (len-cand (length candidate))
       (title
        (progn
          (unless (get-text-property 0 'face candidate)  ; If no face, we apply one
            (let ((start (if sideline-display-backend-name
                             (sideline--display-starting on-left backend-str)
                           0)))
              (add-face-text-property start (+ start len-cand) face nil text)))
          (when action  ; apply action listener
            (let ((keymap (sideline--create-keymap action candidate)))
              (add-text-properties 0 len-text `(keymap ,keymap mouse-face highlight) text)))
          (if on-left (format sideline-format-left text)
            (format sideline-format-right text))))
       (len-title (sideline--str-len title))
       (data (sideline--find-line len-title on-left bol eol order))
       (pos-start (nth 0 data)) (pos-end (nth 1 data)) (occ-pt (nth 2 data))
       (offset (if (or on-left (zerop (window-hscroll))) 0
                 (save-excursion
                   (goto-char pos-start)
                   (end-of-line)
                   (cond ((zerop (current-column)) 0)
                         ((<= (current-column) (window-hscroll))
                          (- 0 (current-column)))
                         (t (- 0 (window-hscroll)))))))
       (str (concat
             (unless on-left
               (propertize " "
                           'display `((space :align-to
                                             (- right ,(sideline--align-right title offset)))
                                      (space :width 0))
                           `cursor t))
             title)))
    ;; Create overlay
    (let* ((len-str (length str))
           (empty-ln (= pos-start pos-end))
           (ov (make-overlay pos-start (if empty-ln pos-start (+ pos-start len-str))
                             nil t t)))
      (cond (on-left
             (if empty-ln
                 (overlay-put ov 'before-string str)
               (overlay-put ov 'display str)
               (overlay-put ov 'invisible t)))
            (t (overlay-put ov 'before-string str)))
      (overlay-put ov 'window (get-buffer-window))
      (overlay-put ov 'priority (if on-left sideline-priority
                                  ;; Add 1 to render on the same line!
                                  (1+ sideline-priority)))
      (overlay-put ov 'creator 'sideline)
      (overlay-put ov 'backend backend)
      (overlay-put ov 'on-left on-left)
      (overlay-put ov 'occ-pt occ-pt)
      (unless (gethash backend sideline--overlays)
        (setf (gethash backend sideline--overlays) nil))
      (push ov (gethash backend sideline--overlays)))))

;;
;; (@* "Async" )
;;

(defun sideline--guess-backend-name (backend)
  "Guess BACKEND's name."
  (let ((name (format "%s" backend)))
    (setq name (sideline--s-replace "sideline-" "" name)
          name (sideline--s-replace "-sideline" "" name))
    name))

(defun sideline--render-candidates (candidates backend on-left order)
  "Render a list of backends (CANDIDATES).

Argument BACKEND is the backend symbol.

Argument ON-LEFT is a flag indicates rendering alignment; if it's non-nil then
we align to the left, otherwise to the right.

Argument ORDER determined the search order for going up or down."
  (let ((inhibit-field-text-motion t)
        (action (sideline--call-backend backend 'action))
        (face (or (sideline--call-backend backend 'face) 'sideline-default))
        (name (or (sideline--call-backend backend 'name)
                  (sideline--guess-backend-name backend)))
        (bol (window-start)) (eol (sideline--window-end)))
    (dolist (candidate candidates)
      (sideline--create-ov backend candidate action face name on-left order bol eol))))

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
           (buffer (current-buffer)))  ; for async check
      (if (eq (car candidates) :async)
          (funcall (cdr candidates)
                   (lambda (cands &rest _)
                     (sideline--with-buffer buffer
                       (when sideline-mode
                         (sideline--render-candidates cands backend on-left order)))))
        (sideline--render-candidates candidates backend on-left order)))))

(defun sideline-stop-p ()
  "Return non-nil if the sideline should not be display."
  (or (region-active-p)
      (bound-and-true-p company-pseudo-tooltip-overlay)
      (bound-and-true-p lsp-ui-peek--overlay)))

(defun sideline-render (&optional buffer)
  "Render sideline once in the BUFFER."
  (sideline--with-buffer (or buffer (current-buffer))
    (unless (funcall sideline-inhibit-display-function)
      (sideline--delete-ovs)  ; for function call externally
      (run-hooks 'sideline-pre-render-hook)
      (sideline--render-backends sideline-backends-left t)
      (sideline--render-backends sideline-backends-right nil)
      (run-hooks 'sideline-post-render-hook))))

(defvar-local sideline--delay-timer nil
  "Timer for delay.")

(defvar-local sideline--ex-window nil
  "Holds previous window.")

(defvar-local sideline--ex-window-start nil
  "Holds previous window start point; this will detect vertical scrolling.")

(defvar-local sideline--ex-window-hscroll nil
  "Holds previous window hscroll; this will detect horizontal scrolling.")

(defun sideline--do-render-p ()
  "Return non-nil if we should re-render sidelines in the post-command."
  (let ((bound-or-point (or (bounds-of-thing-at-point 'symbol) (point)))
        (window (selected-window))
        (win-start (window-start))
        (win-hscroll (window-hscroll)))
    (when  ; conditions allow to re-render sidelines
        (or (not (equal sideline--ex-bound-or-point bound-or-point))
            (not (equal sideline--text-scale-mode-amount text-scale-mode-amount))
            (not (equal sideline--ex-window window))
            (not (equal sideline--ex-window-start win-start))
            (not (equal sideline--ex-window-hscroll win-hscroll)))
      ;; update
      (setq sideline--ex-bound-or-point bound-or-point
            sideline--text-scale-mode-amount text-scale-mode-amount
            sideline--ex-window window
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
