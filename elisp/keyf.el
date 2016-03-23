;;; keyf.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Caio Augusto Araujo Oliveira

;; Author: Caio Augusto Araujo Oliveira <caiooliveira@Caios-MacBook-Pro-2.local>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'keyfreq)

(defun turnon-keyfreq-mode ()
  (interactive)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defun turnoff-keyfreq-mode ()
  (interactive)
  (keyfreq-mode -1)
  (keyfreq-autosave-mode -1))

(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        ace-jump-done
        ace-jump-move
        ace-window
        avy-goto-line
        backward-char
        backward-kill-word
        backward-word
        browse-kill-ring-insert-and-quit
        browse-kill-ring-quit
        clipboard-kill-ring-save
        company-complete-common
        company-complete-number
        company-complete-selection
        company-ignore
        delete-backward-char
        describe-variable
        erase-message-buffer
        eval-buffer
        exit-minibuffer
        ffip
        forward-char
        forward-word
        gnus
        ido-complete
        ido-delete-backward-updir
        ido-exit-minibuffer
        ido-switch-buffer
        indent-new-comment-line
        isearch-abort
        isearch-backward-regexp
        isearch-cancel
        isearch-delete-char
        isearch-exit
        isearch-forward-regexp
        isearch-printing-char
        isearch-repeat-forward
        isearch-ring-retreat
        ispell-minor-check
        ivy-backward-delete-char
        ivy-done
        ivy-next-line
        ivy-previous-line
        js-mode
        js2-line-break
        keyboard-escape-quit
        keyboard-quit
        keyfreq-mode
        keyfreq-save-now
        keyfreq-show
        kill-sentence
        left-char
        minibuffer-complete
        minibuffer-complete-and-exit
        minibuffer-keyboard-quit
        move-beginning-of-line
        move-end-of-line
        mwheel-scroll
        newline-and-indent
        next-history-element
        next-line
        org-beginning-of-line
        org-ctrl-c-ctrl-c
        org-cycle
        org-end-of-line
        org-force-self-insert
        org-return
        org-self-insert-command
        org-todo
        package-menu-execute
        paredit-backward-delete
        paredit-backward-kill-word
        paredit-close-round
        paredit-newline
        paredit-open-round
        paredit-semicolon
        pcomplete
        previous-history-element
        previous-line
        push-button
        quit-window
        right-char
        save-buffer
        save-buffers-kill-terminal
        scroll-down-command
        scroll-up-command
        select-window-0
        select-window-1
        select-window-2
        select-window-3
        select-window-4
        select-window-5
        select-window-6
        select-window-7
        select-window-8
        select-window-9
        self-insert-command
        smarter-move-beginning-of-line
        smex
        suspend-frame
        term-send-raw
        turnon-keyfreq-mode
        undefined ;; lambda function
        undo-tree-redo
        undo-tree-undo
        w3m-goto-url
        w3m-next-anchor
        w3m-view-this-url
        yas-compile-directory
        yas-expand
        yas-next-field-or-maybe-expand
        yank
        ))

(unless (file-exists-p (file-truename keyfreq-file))
  (with-temp-buffer
    (insert "()")
    (write-file (file-truename keyfreq-file))))

;; And use keyfreq-show to see how many times you used a command.
;; comment out below line if there is performance impact
(turnon-keyfreq-mode)

(provide 'keyf)
;;; keyf.el ends here
