;;; Compiled snippets and support files for `makefile-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'makefile-mode
                     '(("compet" "run: comp\n	./sol && cat out.txt\ndbg: comp\n	gdb sol\ncomp:\n	g++ -std=c++11 -g -Wall -o sol sol.cpp -D DEBUG\nclean:\n	-rm sol_dbg.cpp\n	-rm sol" "compet" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        nil nil nil)))


;;; Do not edit! File generated at Sat May  9 10:20:57 2015
