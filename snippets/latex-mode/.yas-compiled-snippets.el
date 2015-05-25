;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("deriv" "\\dfrac{d ${1}}{d${2:t}}" "deriv" nil nil nil nil nil nil)
                       ("drom" "\\dfrac{\\partial ${1}}{\\partial ${2}}" "drom" nil nil nil nil nil nil)
                       ("equation" "\\begin{equation}\n${1:equation}\n\\label{${2:label}}\n\\end{equation}" "equation" nil nil nil nil nil nil)
                       ("figure" "\\begin{figure}[htb]\n\\caption{\\label{${1:label}} ${2:caption}}\n\\begin{center}\n\\includegraphics[scale=${3:0.75}]{img/${4:file_name}}\n\\end{center}\n\\legend{${5:legend}}\n\\end{figure}\n" "figure" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sun May 24 23:30:51 2015
