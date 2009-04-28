;;; em-zle --- ZSH Line Editor compatibility functions for EShell
;; Time-stamp: <2003-07-08 19:20:57 ecl>

;; Copyright (C) 2003 Emilio C. Lopes.

;; Author: Emilio Lopes <eclig@gmx.net>
;; Created: 2003-06-25
;; Version: 0.9
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; If you have not received a copy of the GNU General Public License
;; along with this software, it can be obtained from the GNU Project's
;; World Wide Web server (http://www.gnu.org/copyleft/gpl.html), from
;; its FTP server (ftp://ftp.gnu.org/pub/gnu/GPL), by sending an eletronic
;; mail to this program's maintainer or by writting to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;; If you find this program useful please consider making a donation to
;; the Free Software Foundation. See http://www.fsf.org/help/donate.html
;; (USA), http://www.fsfeurope.org/help/donate.en.html (Europe) or
;; http://fsf.org.in (India) for details on how to accomplish this.
;; In some countries your donation is tax-deductible.

;;; Commentary:

;; Installation
;; ============
;;
;; Put this file ("em-zle.el") in a directory listed in your
;; `load-path' and byte-compile it.
;;
;; Add the following to your "~/.emacs":
;;
;;      (add-hook 'eshell-load-hook (lambda () (require 'em-zle)))
;;
;; Alternatively you can add `eshell-zle' to `eshell-modules-list'.
;; You might also want to bind some of these functions to appropriate
;; keys. See the GNU Emacs on-line documentation for
;; details.
;;
;; Suggested Key bindings
;; ======================
;;
;; (define-key eshell-command-map (kbd "M-.") 'eshell-zle-insert-last-word)
;; (define-key eshell-command-map (kbd "M-_") 'eshell-zle-copy-prev-shell-word)
;; (define-key eshell-command-map (kbd "M-RET") 'eshell-zle-push-line)
;; (define-key eshell-command-map (kbd "M-g") 'eshell-zle-get-line)
;; (define-key eshell-command-map (kbd "M-a") 'eshell-zle-accept-and-hold)
;; (define-key eshell-command-map (kbd "M-?") 'eshell-zle-which-command)
;; (define-key eshell-mode-map (kbd "<up>") 'eshell-zle-up-history)
;; (define-key eshell-mode-map (kbd "<down>") 'eshell-zle-down-history)
;; (define-key eshell-command-map (kbd "M-<") 'eshell-zle-beginning-of-history)
;; (define-key eshell-command-map (kbd "M->") 'eshell-zle-end-of-history)

;;; Code:

(require 'em-hist)
(require 'ring)

;;; User Variables:
(defcustom eshell-zle-which-command "which"
  "*Command used to locate another command."
  :type 'string
  :group 'eshell-mode)

(defcustom eshell-zle-cyclic-history nil
  "*Determine if the ZLE-like history fetching commands should be cyclic."
  :type 'boolean
  :group 'eshell-mode)

;;; Internal Variables:

(defvar eshell-zle-insert-last-word-hist-index 0)
(defvar eshell-zle-insert-last-word-previous-point nil)
(defvar eshell-zle-input-stack nil)

;;; Functions:

(defun eshell-zle-insert-last-word (arg)
  ;; Try to mimic the behavior of the ZSH function as much as
  ;; possible. The docstring was taken from the ZSH man page.
  "Insert the last word from the previous history event at the cursor position.
If a positive numeric argument is given, insert that word from the end
of the previous history event. If the argument is zero or negative
insert that word from the left (zero inserts the previous command
word). Repeating this command replaces the word just inserted with the
last word from the history event prior to the one just used; numeric
arguments can be used in the same way to pick a word from that event."
  (interactive "*p")
  (if (eq last-command this-command)
      (setq eshell-zle-insert-last-word-hist-index (1+ eshell-zle-insert-last-word-hist-index))
    (setq eshell-zle-insert-last-word-hist-index 0))
  (let* ((eshell-history-index nil)
         (last (eshell-previous-input-string eshell-zle-insert-last-word-hist-index)))
    (setq last (car (with-temp-buffer
                      (insert last)
                      (eshell-hist-parse-arguments 'silent (point-min) (point-max)))))
    (if (> arg 0)
        (progn
          (setq last (nreverse last))
          (setq arg (1- arg)))
      (setq arg (- arg)))
    (if (eq last-command this-command)
        (delete-region eshell-zle-insert-last-word-previous-point (point))
      (setq eshell-zle-insert-last-word-previous-point (point)))
    (if (> arg (length last))
        (progn
          (setq this-command 'error)
          (error "Not that many words in previous command"))
      (insert (nth arg last)))))

(defun eshell-zle-copy-prev-shell-word ()
  ;; maybe allow for ARG with the same meaning as in
  ;; `eshell-zle-insert-last-word'?
  "Duplicate the word to the left of the cursor.
The word is found by using shell parsing."
  (interactive)
  (let ((word (car (nreverse (car (eshell-hist-parse-arguments 'silent))))))
    (unless word
      (error "No previous word"))
    (insert word)))

(defun eshell-zle-push-line (&optional push-only)
  "Push current line onto the input stack and return to the top-level prompt.
It will be inserted at the next prompt or when popped with `eshell-zle-get-line'
\(`\\[eshell-zle-get-line]').
With optional argument PUSH-ONLY (a prefix when run interactively) just
push current line onto the stack without deleting it from the EShell buffer."
  (interactive "*P")
  (let ((beg (save-excursion (eshell-bol) (point)))
        (end (point-at-eol)))
    (unless (= beg end)
      (setq eshell-zle-input-stack (cons (buffer-substring-no-properties beg end) eshell-zle-input-stack))
      (unless (or push-only (< (point) eshell-last-output-end))
        (delete-region beg end))
      (add-hook 'eshell-after-prompt-hook 'eshell-zle-get-line))))

(defun eshell-zle-get-line ()
  "Pop the top line off the input stack and insert it at the cursor position."
  (interactive "*")
  (let ((input (car-safe eshell-zle-input-stack)))
    (if input
        (progn
          (if (stringp input)
              (insert input))
          (setq eshell-zle-input-stack (cdr eshell-zle-input-stack)))
      (when (interactive-p) ; stack is empty and we were explicitly called
        (error "Input stack empty"))
      ;; why `this-command' doesn't work here?
      (remove-hook 'eshell-after-prompt-hook 'eshell-zle-get-line))))

(defun eshell-zle-accept-and-hold ()
  "Push the contents of the buffer on the buffer stack and execute it."
  (interactive "*")
  (eshell-zle-push-line 'push-only)
  (eshell-send-input))

(defun eshell-zle-kill-whole-line ()
  "Kill the current line."
  (interactive "*")
  (kill-region (save-excursion (eshell-bol) (point)) (point-at-eol)))

(defun eshell-zle-which-command ()
"Execute `eshell-zle-which-command' on the current command.
`eshell-zle-which-command' is normally \"which\". The current input
is pushed onto the input stack."
  (interactive "*")
  (let ((cmd (caar (eshell-hist-parse-arguments
                    'silent
                    (save-excursion (eshell-bol) (point))
                    (point-at-eol)))))
    (when cmd
      (eshell-zle-push-line)
      (insert (or eshell-zle-which-command "which") " " cmd)
      (eshell-send-input))))

(defun eshell-zle-up-history (arg)
  "Move to the previous event in the history list.
If `eshell-zle-cyclic-history' is non nil cycle though input history."
  (interactive "*p")
  (when (and eshell-hist-move-to-end
             (< (point) eshell-last-output-end))
    (goto-char eshell-last-output-end))
  (end-of-line)
  (if (and (not eshell-zle-cyclic-history) eshell-history-index
           (= eshell-history-index (1- eshell-history-size)))
      (and (interactive-p) (message "History item: 1"))
    (setq this-command 'eshell-previous-input)
    (eshell-previous-input arg)))

(defun eshell-zle-down-history (arg)
  "Move to the next event in the history list.
If `eshell-zle-cyclic-history' is non nil cycle though input history."
  (interactive "*p")
  (when (and eshell-hist-move-to-end
             (< (point) eshell-last-output-end))
    (goto-char eshell-last-output-end))
  (end-of-line)
  (if (and (not eshell-zle-cyclic-history)
           (or (null eshell-history-index) (zerop eshell-history-index)))
      (and (interactive-p)
           (message "History item: %d" (ring-length eshell-history-ring)))
    (setq this-command 'eshell-next-input)
    (eshell-next-input arg)))

;; reset `eshell-history-index' also when one types <return> without
;; any input:
(add-hook 'eshell-post-command-hook 'eshell-zle-reset-history-index nil t)

(defun eshell-zle-reset-history-index ()
  "Reset `eshell-history-index' to nil."
  (setq eshell-history-index nil))

(defun eshell-zle-beginning-of-history ()
  "Move to the first event in the history list."
  (interactive "*")
  (when (and eshell-hist-move-to-end
             (< (point) eshell-last-output-end))
    (goto-char eshell-last-output-end))
  (setq eshell-history-index (1- eshell-history-size))
  (unless (or (not (ring-p eshell-history-ring))
              (ring-empty-p eshell-history-ring))
    (delete-region (save-excursion (eshell-bol) (point)) (point-at-eol))
    (insert-and-inherit
     (ring-ref eshell-history-ring (1- (ring-size eshell-history-ring))))
    (and (interactive-p) (message "History item: 1"))))

(defun eshell-zle-end-of-history ()
  "Move to the last event in the history list."
  (interactive "*")
  (when (and eshell-hist-move-to-end
             (< (point) eshell-last-output-end))
    (goto-char eshell-last-output-end))
  (setq eshell-history-index 0)
  (unless (or (not (ring-p eshell-history-ring))
              (ring-empty-p eshell-history-ring))
    (delete-region (save-excursion (eshell-bol) (point)) (point-at-eol))
    (insert-and-inherit (ring-ref eshell-history-ring 0))
    (and (interactive-p) (message "History item: %d" (ring-size eshell-history-ring)))))

(provide 'em-zle)

;;; em-zle.el ends here
