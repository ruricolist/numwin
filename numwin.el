;;; numwin.el --- generalized window numbering for Emacs

;; Copyright (C) 2009, 2010, 2011, 2012 Paul M. Rodriguez

;; Author: Paul M. Rodriguez <pmr@ruricolist.com>
;; Created: 2010-09-12
;; Version: 0.4
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Several libraries make switching windows in Emacs linear by giving
;; each window a number. This package generalizes that idea to
;; deleting, splitting (vertically and horizontally), zooming (i.e.
;; `delete-other-windows'), growing, shrinking, and swapping windows,
;; and displaying buffers and files in them.

;;; Usage:
;; (require 'numwin)
;; (numwin-mode 1)

;; Window numbers will not appear until they are needed.

;;; Code:

(defvar numwin-window-list nil
  "List of windows from perspective of selected window.")

(defvar numwin-show-p nil)

(defvar numwin-mode-line-form
  '(:eval (if numwin-show-p
              (numwin-window-number-string)))
  "Element for `mode-line-format' that displays window number.")

(defvar numwin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap other-window]
                          'numwin-other)
    (define-key map [remap delete-window]
                          'numwin-delete)
    (define-key map [remap delete-other-windows]
                          'numwin-zoom)
    (define-key map [remap delete-other-windows-vertically]
                          'numwin-zoom-column)
    (define-key map [remap split-window-vertically]
                          'numwin-vsplit)
    (define-key map [remap split-window-horizontally]
                          'numwin-hsplit)
    (define-key map [remap find-file-other-window]
                          'numwin-switch-to-file)
    (define-key map [remap switch-to-buffer-other-window]
                          'numwin-switch-to-buffer)
    (define-key map [remap display-buffer]
                          'numwin-display-buffer)
    (define-key map [remap kill-buffer-and-window]
                          'numwin-kill-buffer-and-window)
    (define-key map [remap shrink-window-if-larger-than-buffer]
                          'numwin-shrink-window-if-larger-than-buffer)
    (define-key map [remap fit-window-to-buffer]
                          'numwin-fit-window-to-buffer)
    (define-key map [remap beginning-of-buffer-other-window]
                          'numwin-bob-other-window)
    (define-key map [remap end-of-buffer-other-window]
                          'numwin-eob-other-window)
    (define-key map [remap scroll-other-window]
                          'numwin-scroll-up)
    (define-key map [remap scroll-other-window-down]
                          'numwin-scroll-down)
    (define-key map [remap enlarge-window]
                          'numwin-vgrow-window)
    (define-key map [remap enlarge-window-horizontally]
                          'numwin-hgrow-window)
    (define-key map [remap shrink-window]
                          'numwin-vshrink-window)
    (define-key map [remap shrink-window-horizontally]
                          'numwin-hshrink-window)
    map)
"Keymap for `numwin-minor-mode '.")

(defface numwin '((t :inherit font-lock-warning-face))
  "Face for displaying window numbers."
  :group 'windows
  :tag "Window Numbering Face")

(defmacro numwin-w/numbering (&rest body)
  "Execute BODY with numbered windows."
  `(let ((numwin-show-p t))
     ;; Since the minibuffer has nowhere to display a number the
     ;; sensible thing to do when the minibuffer is active is to make
     ;; it window 0.
     (if (active-minibuffer-window)
         (select-window (active-minibuffer-window)))
     (setq numwin-window-list (window-list))
     (force-mode-line-update t)
     ,@body))

(defsubst numwin-one-window-p ()
  "Return true when there is one window on frame."
  (null (cdr (window-list))))

(defsubst numwin-two-windows-p ()
  "Return true when there are two windows on frame."
  (and (cadr (window-list))
       (null (nthcdr 2 (window-list)))))

(defsubst numwin-window-list ()
  "Return or initialize window list."
  (or numwin-window-list (setq numwin-window-list (window-list))))

(defsubst numwin-window-number (win)
  "Return number for WIN."
  (let (count match (list (numwin-window-list)))
    (while (and list (not match))
      (if (eq (car list) win)
          (setq match win))
      (setq list (cdr list)
            count (if count (1+ count) 0)))
    count))

(defun numwin-window-number-string ()
  "Return formatted number of current window."
  (concat " "
          (propertize
           (format "%s%d "
                   (if (eq t (window-dedicated-p (selected-window)))
                       "!" "#")
                   (numwin-window-number (selected-window)))
           'face 'numwin)))

(defun numwin-read (prompt)
  "Read a window number with PROMPT.
The selected window (or active minibuffer) is always 0; other
windows are numbered in cyclic order."
  (let ((cursor-in-echo-area t))
    (let* ((len (length (window-list)))
           (num
            (if (> 10 len)
                ;; Side benefit: since `string-to-number' returns 0
                ;; when it can't parse a string, anything that isn't a
                ;; number just selects the current window.
                (string-to-number (char-to-string (read-char prompt)))
              (read-number prompt 0))))
      (nth (mod num len) (numwin-window-list)))))

(defun numwin-other (count)
  "Select another window.
If there is only one other window, select it.

With COUNT, skip COUNT windows in cyclic order.

If there are two or more other windows, prompt for a number."
  (interactive "p")
  (if (or (not (eq count 1))
          (numwin-one-window-p)
          (numwin-two-windows-p))
      (other-window count)
    (numwin-w/numbering
     (select-window
      (numwin-read "Select window #: ")))))

(defun numwin-delete ()
  "Remove a window from its frame."
  (interactive)
  (if (numwin-one-window-p)
      (delete-window)
    (numwin-w/numbering
     (delete-window (numwin-read "Delete window #: ")))))

(defun numwin-zoom ()
  "Make a window fill its frame."
  (interactive)
  (if (numwin-one-window-p)
      (delete-other-windows)
    (numwin-w/numbering
     (delete-other-windows
      (numwin-read "Zoom window #: ")))))

(defun numwin-zoom-column ()
  "Make a window fill its column."
  (interactive)
  (if (numwin-one-window-p)
      (delete-other-windows-vertically)
    (numwin-w/numbering
     (delete-other-windows-vertically
      (numwin-read "Zoom window #: ")))))

(defun numwin-vsplit (&optional size)
  "Split a window into two windows, one above the other."
  (interactive "P")
  (if (numwin-one-window-p)
      (split-window-vertically size)
    (numwin-w/numbering
     (with-selected-window (numwin-read "Vsplit window #: ")
       (split-window-vertically size)))))

(defun numwin-hsplit (&optional size)
  "Split a window into two windows, side by side."
  (interactive)
  (if (numwin-one-window-p)
      (split-window-horizontally size)
    (numwin-w/numbering
     (with-selected-window (numwin-read "Hsplit window #: ")
       (split-window-horizontally size)))))

(defun numwin-switch-to-file (filename &optional wildcards)
  "Edit FILENAME in some other window."
  (interactive
   (find-file-read-args "Find file in other window: "
                        (confirm-nonexistent-file-or-buffer)))
  (if (or (numwin-one-window-p) (numwin-two-windows-p))
      (find-file-other-window filename wildcards)
    (numwin-w/numbering
     (select-window (numwin-read "... in window #: "))
     (find-file filename wildcards))))

(defun numwin-display-buffer (buffer &optional not-this-window)
  "Make BUFFER appear in some other window but don't select it."
  (interactive "BDisplay buffer:\nP")
  (if (or (numwin-one-window-p) (numwin-two-windows-p))
      (display-buffer buffer not-this-window)
    (numwin-w/numbering
     (set-window-buffer (numwin-read "... in window #: ") buffer))))

(defun numwin-switch-to-buffer (buffer)
  "Make BUFFER current and display it in some other window."
  (interactive "BSwitch to buffer: ")
  (if (or (numwin-one-window-p) (numwin-two-windows-p))
      (switch-to-buffer-other-window buffer)
    (numwin-w/numbering
     (let ((w (numwin-read "... in window #: ")))
       (set-window-buffer w buffer)
       (select-window w)))))

(defun numwin-kill-buffer-and-window ()
  "Delete a window and kill its buffer."
  (interactive)
  (if (numwin-one-window-p)
      (call-interactively 'kill-buffer-and-window)
    (numwin-w/numbering
     (select-window (numwin-read "Delete window and kill its buffer: "))
     (call-interactively 'kill-buffer-and-window))))

(defun numwin-shrink-window-if-larger-than-buffer ()
  "Shrink a window if its buffer doesn't need so many lines."
  (interactive)
  (if (numwin-one-window-p)
      (shrink-window-if-larger-than-buffer)
    (numwin-w/numbering
     (shrink-window-if-larger-than-buffer
      (numwin-read "Shrink window # to fit: ")))))

(defun numwin-fit-window-to-buffer ()
  "Adjust height of a window to display it's buffer's contents exactly."
  (interactive)
  (if (numwin-one-window-p)
      (fit-window-to-buffer)
    (numwin-w/numbering
     (fit-window-to-buffer
      (numwin-read "Fit window #: ")))))

(defun numwin-bob-other-window (arg)
  "Move point to the beginning of buffer in a window.
Like `beginning-of-buffer-other-window'."
  (interactive "P")
  (if (or (numwin-one-window-p) (numwin-two-windows-p))
      (call-interactively 'beginning-of-buffer-other-window)
    (numwin-w/numbering
     (with-selected-window (numwin-read "Move to beginning of window #: ")
       (call-interactively 'beginning-of-buffer)))))

(defun numwin-eob-other-window (arg)
  "Move point to the end of the buffer in a window.
Like `end-of-buffer-other-window'."
  (interactive "P")
  (if (or (numwin-one-window-p) (numwin-two-windows-p))
      (call-interactively 'end-of-buffer-other-window)
    (numwin-w/numbering
     (with-selected-window (numwin-read "Move to end of window #: ")
       (call-interactively 'end-of-buffer)))))

(defun numwin-scroll-up (lines)
  "Scroll a window up LINES."
  (interactive "P")
  (if (or (numwin-one-window-p) (numwin-two-windows-p))
      (scroll-other-window lines)
    (numwin-w/numbering
     (with-selected-window (numwin-read "Scroll window #: ")
       (scroll-up lines)))))

(defun numwin-scroll-down (lines)
  "Scroll a window up LINES."
  (interactive "P")
  (numwin-scroll-up
   (if (eq lines '-) nil
     (if (null lines) '-
       (- (prefix-numeric-value lines))))))

(defun numwin-vgrow-window (size)
  "Make a window SIZE lines taller."
  (interactive "p")
  (if (numwin-one-window-p)
      (enlarge-window size)
    (numwin-w/numbering
     (with-selected-window (numwin-read "Grow window #: ")
       (enlarge-window size)))))

(defun numwin-hgrow-window (columns)
  "Make a window COLUMNS wider."
  (interactive "p")
  (if (numwin-one-window-p)
      (enlarge-window-horizontally columns)
    (numwin-w/numbering
     (with-selected-window (numwin-read "Widen window #: ")
       (enlarge-window-horizontally columns)))))

(defun numwin-vshrink-window (size)
  "Make a window SIZE lines shorter."
  (interactive "p")
  (if (numwin-one-window-p)
      (shrink-window size)
    (numwin-w/numbering
     (with-selected-window (numwin-read "Shorten window #: ")
       (shrink-window size)))))

(defun numwin-hshrink-window (columns)
  "Make a window COLUMNS narrower."
  (interactive "p")
  (if (numwin-one-window-p)
      (shrink-window-horizontally columns)
    (numwin-w/numbering
     (with-selected-window (numwin-read "Narrow window #: ")
       (shrink-window-horizontally columns)))))

(defun numwin-swap-windows (win1 win2)
  "Transpose buffer in WIN1 with buffer in WIN2."
  (interactive
   (if (numwin-one-window-p)
       (error "Only one window")
     (if (numwin-two-windows-p)
         (list (selected-window) (next-window))
       (numwin-w/numbering
        (list (numwin-read "Swap window #: ")
              (numwin-read "... with window #: "))))))
  (let ((b1 (window-buffer win1))
        (b2 (window-buffer win2)))
    (set-window-buffer win1 b2)
    (set-window-buffer win2 b1)))

(defun numwin-detach-window (window)
  "Switch to WINDOW in its own frame."
  (interactive
   (if (numwin-one-window-p)
       (error "Only one window")
     (numwin-w/numbering
      (list (numwin-read "Detach window #: ")))))
  (let ((buffer (window-buffer window)))
    (delete-window window)
    (switch-to-buffer-other-frame buffer)))

(defun numwin-dedicate-window (window)
  "Toggle whether WINDOW is (strongly) dedicated.
See `set-window-dedicated-p'."
  (interactive
   (list
    (if (numwin-one-window-p) (selected-window)
      (numwin-w/numbering
       (numwin-read "Dedicate window #: ")))))
  (if (set-window-dedicated-p window (null (window-dedicated-p window)))
      (message "Window dedicated")
    (message "Window not dedicated")))

;;;###autoload
(define-minor-mode numwin-mode
  "Minor mode for numbering windows when needed."
  :init-value nil
  :global t
  :keymap numwin-mode-map
  ;; A regular minor-mode lighter would not always be visible.
  (if numwin-mode
      (setq-default mode-line-format
                    (cons numwin-mode-line-form
                          (delete numwin-mode-line-form mode-line-format)))
    (setq-default mode-line-format
                  (delete numwin-mode-line-form mode-line-format))))

(provide 'numwin)

;;; numwin.el ends here
