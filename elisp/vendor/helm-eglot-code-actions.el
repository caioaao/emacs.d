;;; helm-eglot-code-actions.el --- Eglot "code actions" command using helm  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Caio

;; Author: Caio
;; Keywords: tools, extensions, languages

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

;;; Code:

(require 'eglot)

(defun eglot--code-actions-menu-items (beg &optional end)
  "List code actions between BEG and END.
Interactively, if a region is active, BEG and END are its bounds,
else BEG is point and END is nil, which results in a request for
code actions at point"
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions
          (jsonrpc-request
           server
           :textDocument/codeAction
           (list :textDocument (eglot--TextDocumentIdentifier)
                 :range (list :start (eglot--pos-to-lsp-position beg)
                              :end (eglot--pos-to-lsp-position end))
                 :context
                 `(:diagnostics
                   [,@(cl-loop for diag in (flymake-diagnostics beg end)
                               when (cdr (assoc 'eglot-lsp-diag (eglot--diag-data diag)))
                               collect it)]))))
         (menu-items
          (or (mapcar (jsonrpc-lambda (&rest all &key title &allow-other-keys)
                        (cons title all))
                      actions)
              (eglot--error "No code actions here"))))
    menu-items))

(defun eglot--code-actions-execute-action (action)
  "Execute `ACTION` current server"
  (let ((server (eglot--current-server-or-lose)))
    (eglot--dcase action
      (((Command) command arguments)
       (eglot-execute-command server (intern command) arguments))
      (((CodeAction) edit command)
       (when edit (eglot--apply-workspace-edit edit))
       (when command
         (eglot--dbind ((Command) command arguments) command
           (eglot-execute-command server (intern command) arguments)))))))

(defun helm-eglot--build-code-actions-source (menu-items)
  (let* ((execute-selected-action (lambda (action) (eglot--code-actions-execute-action (cdr (assoc action menu-items))))))
    (helm-build-sync-source "Eglot Code actions"
      :candidates
      (lambda () (mapcar #'car menu-items))
      :action `(("Execute action" . ,execute-selected-action)))))

(defun helm-eglot-code-actions (beg &optional end action-kind)
  "Offer to execute code actions between BEG and END using helm.
Interactively, if a region is active, BEG and END are its bounds,
else BEG is point and END is nil, which results in a request for
code actions at point"
  (interactive
   `(,@(eglot--region-bounds)
     ,(and current-prefix-arg
           (completing-read "[eglot] Action kind: "
                            '("quickfix" "refactor.extract" "refactor.inline"
                              "refactor.rewrite" "source.organizeImports")))))
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((menu-items (eglot--code-actions-menu-items beg end)))
    (helm :sources (helm-eglot--build-code-actions-source menu-items)
          :buffer "*helm Eglot Code Actions*")))

(provide 'helm-eglot-code-actions)
;;; helm-eglot-code-actions.el ends here
