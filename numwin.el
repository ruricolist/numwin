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

(eval-when-compile (require 'cl))

(defvar numwin-window-list nil
  "List of windows from perspective of selected window.")

(defvar numwin-visible-p nil)

(defvar numwin-mode-line-form
  '(:eval (if numwin-visible-p
              (numwin-window-number-string)))
  "Element for `mode-line-format' that displays window number.")

(defstruct (numwin-command (:type list))
  name verb threshold movesp offsetp)

(defcustom numwin-commands
  '((other-window "select" 2 t t)
    (delete-window "delete" 1)
    (delete-other-windows "zoom" 1)
    (delete-other-windows-vertically "zoom" 1)
    (split-window-vertically "split" 1)
    (split-window-below "split" 1)
    (split-window-horizontally "split" 1)
    (split-window-right "split" 1)
    (find-file-other-window "use" 2 t t)
    (ido-find-file-other-window "use" 2 t t)
    (display-buffer "use" 2)
    (ido-display-buffer "use" 2)
    (switch-to-buffer-other-window "use" 2 t t)
    (ido-switch-buffer-other-window "use" 2 t t)
    (kill-buffer-and-window "kill" 1)
    (shrink-window-if-larger-than-buffer "shrink" 1)
    (fit-window-to-buffer "fit" 1)
    (beginning-of-buffer-other-window "move" 2 nil t)
    (end-of-buffer-other-window "move" 2 nil t)
    (scroll-other-window "scroll" 2 nil t)
    (scroll-other-window-down "scroll" 2 nil t)
    (enlarge-window "enlarge" 1)
    (enlarge-window-horizontally "enlarge" 1)
    (shrink-window "shrink" 1)
    (shrink-window-horizontally "shrink" 1))
  "Table of commands, with the minimum number of windows to
  trigger numbering, whether to stay in the new window, and
  whether to start from the previous window."
  :group 'windows
  :type '(repeat
          (list (function :tag "Command")
                (string :tag "Verb (for prompt)")
                (integer :tag "Threshold")
                (list :inline t
                      (boolean :tag "Moves to new window")
                      (boolean :tag "Call with offset")))))

(defvar numwin-last nil
  "The window we started in.")

(defface numwin '((t :inherit font-lock-warning-face))
  "Face for displaying window numbers."
  :group 'windows
  :tag "Window Numbering Face")

(defmacro numwin-with-numbering (&rest body)
  "Execute BODY with numbered windows."
  `(let ((numwin-visible-p t))
     ;; Since the minibuffer has nowhere to display a number the
     ;; sensible thing to do when the minibuffer is active is to make
     ;; it window 0.
     (if (active-minibuffer-window)
         (select-window (active-minibuffer-window)))
     (setq numwin-window-list (window-list))
     (force-mode-line-update t)
     ,@body))

(defsubst numwin-window-list ()
  "Return or initialize window list."
  (or numwin-window-list (setq numwin-window-list (window-list))))

(defsubst numwin-window-number (window)
  "Return number for WINDOW."
  (let (count match (list (numwin-window-list)))
    (while (and list (not match))
      (if (eq (car list) window)
          (setq match window))
      (setq list (cdr list)
            count (if count (1+ count) 0)))
    count))

(defun numwin-window-number-string ()
  "Return formatted number of current window."
  (concat " "
          (propertize
           (format "#%d" (numwin-window-number (selected-window)))
           'face 'numwin)))

(defun numwin-read-window-number (prompt)
  "Read a window number with PROMPT.
The selected window (or active minibuffer) is always 0; other
windows are numbered in cyclic order."
  (let ((cursor-in-echo-area t)
        (len (count-windows)))
    (if (> 10 len)
        ;; Side benefit: since `string-to-number' returns 0 when it
        ;; can't parse a string, anything that isn't a number just
        ;; selects the current window.
        (string-to-number (char-to-string (read-char prompt)))
      (read-number prompt 0))))

(defun numwin-nth-window (number)
  (nth (mod number (count-windows)) (numwin-window-list)))

(defun numwin-switch ()
  (let ((match (assq this-command numwin-commands)))
    (when (and match (nthcdr (numwin-command-threshold match)
                             (window-list)))
      (setq numwin-last (selected-window))
      (numwin-with-numbering
       (let ((window-number
              (numwin-read-window-number
               (format "Window to %s: " (numwin-command-verb match)))))
         (select-window (numwin-nth-window
                         (if (numwin-command-offsetp match)
                             (1- window-number)
                           window-number))))))))

(defun numwin-switch-back ()
  (when (and numwin-last (window-live-p numwin-last))
    (let ((match (assq this-command numwin-commands)))
      (when (and match (not (numwin-command-movesp match)))
        (select-window
         (prog1 numwin-last
           (setq numwin-last nil)))))))

;;;###autoload
(define-minor-mode numwin-mode
  "Minor mode for generalized window numbering."
  :init-value nil
  :global t
  (if numwin-mode
      (progn
        (setq-default mode-line-format
                      (cons numwin-mode-line-form
                            (delete numwin-mode-line-form mode-line-format)))
        (add-hook 'pre-command-hook 'numwin-switch)
        (add-hook 'post-command-hook 'numwin-switch-back))
    (progn
      (remove-hook 'pre-command-hook 'numwin-switch)
      (remove-hook 'post-command-hook 'numwin-switch-back)
      (setq-default mode-line-format
                    (delete numwin-mode-line-form mode-line-format)))))

(provide 'numwin)

;;; numwin.el ends here
