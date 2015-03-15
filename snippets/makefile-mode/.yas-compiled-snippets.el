;;; Compiled snippets and support files for `makefile-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'makefile-mode
                     '(("compet" "run: clean comp\n	./sol && cat out.txt\ndbg: clean comp_dbg\n	db sol\ncomp: gen_dbg_source\n	g++ -O2 -Wall -o sol sol_dbg.cpp\ncomp_dbg: gen_dbg_source\n	g++ -g -Wall -o sol sol_dbg.cpp\ngen_dbg_source:\n	echo \"#define DEBUG\" > sol_dbg.cpp\n	cat sol.cpp >> sol_dbg.cpp\nclean:\n	-rm sol_dbg.cpp\n	-rm sol" "compet" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Wed Mar 11 22:07:07 2015
