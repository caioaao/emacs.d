;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("todolist" "\n#+TODO: TODO DOING | DONE CANCELED\n* [/] Todo\n  - [ ] $1\n* [/] Doing\n\n* [/] Done\n\n" "todo list" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        nil nil nil)))


;;; Do not edit! File generated at Sun May 24 23:30:51 2015
