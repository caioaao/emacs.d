;;; Compiled snippets and support files for `go-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
                     '(("const" "const ${1:name type} = ${2:val}\n$0" "const" nil nil nil nil nil nil)
                       ("const" "const (\n  ${1:name type} = ${2:val}\n)" "const (...)" nil nil nil nil nil nil)
                       ("dd" "fmt.Printf(\"%+v\\n\", $1) // output for debug\n$0" "debug fmt.Printf" nil nil nil nil nil nil)
                       ("def" "default:\n$0" "default" nil nil nil nil nil nil)
                       ("el" " else {\n  $0\n}" "else" nil nil nil nil nil nil)
                       ("for" "for $1 {\n  $0\n}\n" "for" nil nil nil nil nil nil)
                       ("func" "//\nfunc ${1:name}(${2:args}) $3 {\n  $0\n}" "func" nil nil nil nil nil nil)
                       ("if" "if $1 {\n  $0\n}" "if" nil nil nil nil nil nil)
                       ("iferr" "if err != $1 {\n  $0\n}\n" "if error" nil nil nil nil nil nil)
                       ("imp" "import \"$1\"\n$0" "import" nil nil nil nil nil nil)
                       ("imp" "import (\n  \"$0\"\n)\n" "import (...)" nil nil nil nil nil nil)
                       ("lambda" "func(${1:args}) $2 {\n  $0\n}()" "lambda func" nil nil nil nil nil nil)
                       ("main" "func main() {\n  $0\n}" "func main()" nil nil nil nil nil nil)
                       ("map" "map[${1:type}]${2:type}\n" "map" nil nil nil nil nil nil)
                       ("pr" "fmt.Printf(\"${1:%s}\\n\", ${2:args})\n$0" "fmt.Printf(...)" nil nil nil nil nil nil)
                       ("sel" "select {\ncase $1:\n  $0\n}" "select" nil nil nil nil nil nil)
                       ("sw" "switch $1 {\ncase $2:\n  $0\n}" "switch" nil nil nil nil nil nil)
                       ("type" "type $1 $2 {\n  $0\n}" "type" nil nil nil nil nil nil)
                       ("var" "var ${1:name} ${2:type}\n$0" "var" nil nil nil nil nil nil)
                       ("var" "var (\n  ${1:name} ${2:type}\n)" "var (...)" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Wed Mar 11 22:07:05 2015
