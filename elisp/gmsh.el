
(require 'cl)

;; Remove previous associations of file extension .pro with idlwave:
(setq auto-mode-alist (delete-if '(lambda (x) (equal (car x) "\\.pro\\'")) auto-mode-alist))

(defvar gmsh/getdp-functions-list
  '("Tanh" "Tan" "Sinh" "Sin" "Sqrt" "Rand" "Modulo" "Log" "Hypot" "Floor" "Fmod" "Fabs" "Exp" "Cosh" "Cos" "Ceil" "Atan" "Asin" "Acos"
    "Printf" "Print")
  "Function identifier common to gmsh and getpd")

(defvar gmsh/getdp-keywords-list
  '("Function" "For" "EndFor" "If" "EndIf" "Include")
  "Keyword identifiers common to gmsh and getdp")

(defvar gmsh/getdp-constants-list
  '("Pi")
  "Constant identifiers common to gmsh and getdp")

(defvar gmsh-keywords-list
  '("Return" "Call" "Exit"
    "Include" "SetName" "NonBlockingSystemCall" "SystemCall" "Sleep" "Mesh" "Delete" "BoundingBox" "Merge" "Error"
    "Point" "Physical" "Line" "Loop" "Surface" "Volume" "Compound"
    ;; new region numbers:
    "newp" "newl" "news" "newv" "newll" "newsl" "newreg")
  "Gmsh key words")

(defvar gmsh-functions-list
  '(
    "Extrude" "Dilate" "Rotate" "Symmetry" "Translate" "Boundary" "CombinedBoundary" "Duplicata"
    )
  "List of function identifiers specific to Gmsh (see also `gmsh/getdp-functions-list').")

(defvar gmsh-constants-list
  '()
  "Gmsh constants.")

(defvar getdp-keywords-list
  '(
    ;; Types of:
    "Adapt" "Adaptation" "AliasOf" "Assign" "AssignFromResolution" "AssociatedWith" "BF_CurlEdge" "BF_CurlGroupOfEdges" "BF_CurlGroupOfPerpendicularEdge" "BF_CurlPerpendicularEdge" "BF_dGlobal" "BF_DivFacet" "BF_DivPerpendicularFacet" "BF_Edge" "BF_Facet" "BF_Global" "BF_GradGroupOfNodes" "BF_GradNode" "BF_GroupOfEdges" "BF_GroupOfNodes" "BF_GroupOfPerpendicularEdge" "BF_Node" "BF_NodeX" "BF_NodeY" "BF_NodeZ" "BF_One" "BF_PerpendicularEdge" "BF_PerpendicularFacet" "BF_Region" "BF_RegionX" "BF_RegionY" "BF_RegionZ" "BF_Volume" "BF_Zero" "Break" "ChangeOfCoordinates" "ChangeOfValues" "DecomposeInSimplex" "Depth" "deRham" "Dimension" "Dt" "DtDof" "DtDofJacNL" "DtDt" "DtDtDof" "DualEdgesOf" "DualFacetsOf" "DualNodesOf" "DualVolumesOf" "EdgesOf" "EdgesOfTreeIn" "EigenSolve" "EigenvalueLegend" "ElementsOf" "Else" "Evaluate" "FacetsOf" "FacetsOfTreeIn" "FemEquation" "File" "Form0" "Form1" "Form1P" "Form2" "Form2P" "Form3" "Format" "FourierTransform" "Frequency" "FrequencyLegend" "Galerkin" "Gauss" "GaussLegendre" "Generate" "GenerateJac" "GenerateOnly" "GenerateOnlyJac" "GenerateSeparate" "Global" "Global" "Gmsh" "GmshParsed" "Gnuplot" "GroupsOfEdgesOf" "GroupsOfEdgesOnNodesOf" "GroupsOfNodesOf" "HarmonicToTime" "Hexahedron" "If" "Init" "InitFromResolution" "InitSolution" "Integral" "Integral" "Iso" "IterativeLoop" "IterativeLoopN" "JacNL" "Lanczos" "LastTimeStepOnly" "Lin" "Line" "Link" "LinkCplx" "Local" "Local" "Network" "NeverDt" "NodesOf" "NodeTable" "NoNewLine" "OnBox" "OnElementsOf" "OnGlobal" "OnGrid" "OnLine" "OnPlane" "OnPoint" "OnRegion" "OnSection" "Point" "PostOperation" "Prism" "Pyramid" "Quadrangle" "Region" "SaveSolution" "SaveSolutions" "Scalar" "SetFrequency" "SetTime" "SimpleTable" "Skin" "Smoothing" "Solve" "SolveJac" "Sort" "StoreInField" "StoreInRegister" "Sur" "SurAxi" "SystemCommand" "Table" "Target" "Tetrahedron" "TimeLegend" "TimeLoopAdaptive" "TimeLoopNewmark" "TimeLoopTheta" "TimeStep" "TimeTable" "TransferInitSolution" "TransferSolution" "Triangle" "Update" "UpdateConstraint" "Value" "Vector" "Vol" "VolAxi" "VolAxiRectShell" "VolAxiSphShell" "VolAxiSqu" "VolAxiSquRectShell" "VolAxiSquSphShell" "VolRectShell" "VolSphShell" "VolumesOf" "List" "ListAlt" "Type" "Value" "Vector" "TimeFunction" "UsingPost"
    ;; keywords associated to objects:
    "All" "Analytic" "BasisFunction" "Case" "Constraint" "Criterion" "DefineFunction" "DefineConstant" "DefineGroup" "DestinationSystem" "Entity" "EntitySubType" "EntityType" "Equation" "Format" "Formulation" "Frequency" "FunctionSpace" "GeoElement" "GlobalEquation" "GlobalQuantity" "GlobalTerm" "Group" "In" "IndexOfSystem" "Integral" "Integration" "Jacobian" "Local" "Loop" "Name" "NameOfBasisFunction" "NameOfCoef" "NameOfConstraint" "NameOfFormulation" "NameOfMesh" "NameOfPostProcessing" "NameOfSpace" "NameOfSystem" "Network" "Node" "NumberOfPoints" "Operation" "OriginSystem" "PostOperation" "PostProcessing" "Quantity" "Region" "Resolution" "Solver" "SubRegion" "SubSpace" "Support" "Symmetry" "System"
    )
  "List of keyword identifiers specific to getdp (see also `gmsh/getdp-keywords-list')")

(defvar getdp-functions-list
  '("Log10" "Atan2" "Transpose" "TTrace" "Unit" "Complex" "CompX" "CompXX" "CompXY" "CompXZ" "CompY" "CompYX" "CompYY" "CompYZ" "CompZ" "CompZX" "CompZY" "CompZZ" "Im" "Re" "Tensor" "TensorDiag" "TensorSym" "TensorV"
    ;;
    "Cross" "F_Cos_wt_p" "F_Period" "F_Sin_wt_p" "Fabs" "Fmod" "Hypot" "Norm" "SquNorm"
    ;; fields:
    "BF" "Curl" "CurlInv" "dInv" "Div" "DivInv" "Dof" "Grad" "GradInv" "Rot" "RotInv"
    ;; current Values:
    "$DTime" "$EigenvalueImag" "$EigenvalueReal" "$Iteration" "$Theta" "$Time" "$TimeStep" "$X" "$XS" "$Y" "$YS" "$Z" "$ZS"
    ;; misc functions:
    "dInterpolationAkima" "dInterpolationLinear" "F_CompElementNum" "Order" "Rand"
    ;; Green functions:
    "Helmholtz" "Laplace"
    )
  "List with function identifiers specific to getdp (see also `gmsh/getdp-functions-list').")

(defvar getdp-constants-list
  '("0D" "1D" "2D" "3D" )
  "Gmsh constants.")

(defvar gmsh/getdp-block-statements '(("For" "EndFor")
				      ("If" "EndIf"))
  "List of block statements that are common to gmsh and getdp.
This list is mainly used for indentation.  Each entry is a list
with the begin and end statement for a block.")

(defvar gmsh-block-statements  '(("Function" "Return"))
  "Gmsh block statements. Mainly for indentation.
In each entry the first element is the block start the second the block end.")

(defvar getdp-block-statements '()
  "Gmsh block statements. Mainly for indentation.
In each entry the first element is the block start the second the block end.")

(defun gmsh/getdp-highlight-In-after-For (lim)
  "Find In after For"
  (interactive)
  (let (found)
    (catch 0
      (while (setq found (search-forward-regexp "\\_<In\\_>" lim 'noErr))
	(and
	 (null (syntax-ppss-context (syntax-ppss)))
	 (let ((inhibit-changing-match-data t)) (looking-back "\\_<For[[:blank:]]+\\(\\sw\\|\\s_\\)+[[:blank:]]+In"))
	 (throw 0 found))))))

(defvar indent-amount 2
  "Number of columns to insert additionally for each indentation level.")

(defun nonspace-before (p)
  "Return list (c p l) with character c position p and number of
lines l when traveling backwards stopping at position p behind
the first non-space character c. Thereby, l is the number of
lines passed on the way."
  (let (c (l 0))
    (while (and
	    (setq c (char-before p))
	    (or (when (= c ?\n) (setq l (1+ l)))
		(= (char-syntax c) ?\ )))
      (setq p (1- p)))
    (list c p l)))

(defvar indent-relative-blocks nil
  "List of block statement descriptions recognized by `indent-relative-function'.
Each block statement description is itself a list containing two
strings.  The first string is the identifier for the beginning of
the block the second that one for the end.")

(make-variable-buffer-local 'indent-relative-blocks)

(defun indent-relative-function ()
  "Indent current line relative to previous one."
  (interactive)
  (save-excursion
    (if (eq (syntax-ppss-context (syntax-ppss ;; note: syntax-ppss can move point!
				  (line-beginning-position))) 'string)
	'noindent ;; line starts in string ==> return 'noindent
      ;; calculate indent level
      ;; search for the previous non-empty line:
      (let (b e prevIndent relDepth (p (point))
	      to
	      (re-block (regexp-opt (apply 'append indent-relative-blocks) 'words))
	      (re-end (concat "\\(\\s)\\|" (regexp-opt (mapcar 'cadr indent-relative-blocks) 'words) "\\)")))
	(save-excursion
	  (setq relDepth (syntax-ppss-depth (syntax-ppss)))
	  (beginning-of-line (- 1 (nth 2 (nonspace-before (line-beginning-position)))))
	  (setq prevIndent (current-indentation))
	  (move-to-column prevIndent)
	  (setq relDepth (- relDepth (syntax-ppss-depth (syntax-ppss))))
	  (when (and
		 (null (member (syntax-ppss-context (syntax-ppss)) '(string comment)))
		 (looking-at-p re-end))
	    (setq relDepth (1+ relDepth)))
	  ;; look for block keywords:
	  (setq e (line-end-position))
	  (while (search-forward-regexp re-block e 'noErr)
	    (unless (syntax-ppss-context (syntax-ppss))
	      (if (assoc (match-string-no-properties 0) indent-relative-blocks)
		  (setq relDepth (1+ relDepth))
		(setq relDepth (1- relDepth))
		))))
	(goto-char p)
	(beginning-of-line)
	(delete-horizontal-space)
	(when (looking-at-p re-end)
	  (setq relDepth (1- relDepth)))
	(setq to (+ prevIndent (* relDepth indent-amount)))
	(unless (= to (current-indentation))
	  (indent-to to)))))
  (when (< (current-column) (current-indentation))
    (move-to-column (current-indentation))))

(defmacro key-with-indent (k)
  "Define k as key with indent."
  (let ((sym (make-symbol (concat "electric-char-" (char-to-string k)))))
  `(progn
     (defun ,sym ()
       "Run `self-insert-command' and `indent-relative-function' for this character."
       (interactive)
       (self-insert-command 1)
       (indent-relative-function)
       )
     (local-set-key [,k] (quote ,sym))
     )))

(defun gmsh/getdp-common-settings ()
  "Common settings for gmsh and getdp."
  (setq font-lock-defaults (list
			    ;;;;;;;;;;
			    ;; KEYWORDS:
			    (list
			     (cons (regexp-opt gmsh/getdp-keywords-list 'symbols) 'font-lock-keyword-face)
			     (cons (regexp-opt gmsh/getdp-functions-list 'symbols) 'font-lock-function-name-face)
			     (cons (regexp-opt gmsh/getdp-constants-list 'symbols) 'font-lock-constant-face)
			     (cons 'gmsh/getdp-highlight-In-after-For 'font-lock-keyword-face))
			    ;;;;;;;;;;
			    nil ; keywords-only
			    nil ; case-fold
			    ;;;;;;;;;;
			    ;; syntax-list:
			    '(
			      ("_." . "w")
			      (?/ . ". 1456")
			      (?/ . ". 124b")
			      (?* . ". 23")
			      (?\n . "> b")
			      (?= . ".") (?+ . ".") (?- . ".")
			      (?$ . "_")
			      )
			    ))
  (local-set-key (kbd "<return>") '(lambda () (interactive) (newline) (indent-relative-function)))
  (key-with-indent ?}) (key-with-indent ?{)
  (key-with-indent ?() (key-with-indent ?))
  (key-with-indent ?[)  (key-with-indent ?])
  (setq indent-line-function 'indent-relative-function))

(if (version-list-< (version-to-list emacs-version) '(24 0))
    (defadvice regexp-opt (after symbols activate)
      (if (eq paren 'symbols)
	  (setq ad-return-value (concat "\\_<" ad-return-value "\\_>")))))

(defun last-cons (list)
  "Return last cons in list. Note, that list must contain at least one element."
  (unless (and list (listp list))
    (error "Call of last-cons with non-list or empty list argument."))
  (while (cdr list)
    (setq list (cdr list)))
  list)

(define-derived-mode gmsh-mode fundamental-mode "gmsh"
  "Major mode for editing gmsh geometry definitions."
  (gmsh/getdp-common-settings)
  (setcdr (last-cons (car font-lock-defaults))
	  (list (cons (regexp-opt gmsh-keywords-list 'symbols) 'font-lock-keyword-face)
		(cons (regexp-opt gmsh-functions-list 'symbols) 'font-lock-function-name-face)
		(cons (regexp-opt gmsh-constants-list 'symbols) 'font-lock-constant-face)
		))
  (setq indent-relative-blocks (append gmsh/getdp-block-statements gmsh-block-statements))
  (font-lock-mode 1)
  )

(define-derived-mode getdp-mode fundamental-mode "getdp"
  "Major mode for editing gmsh geometry definitions."
  (gmsh/getdp-common-settings)
  (setcdr (last-cons (car font-lock-defaults))
	  (list (cons (regexp-opt getdp-keywords-list 'symbols) 'font-lock-keyword-face)
		(cons (regexp-opt getdp-functions-list 'symbols) 'font-lock-function-name-face)
		(cons (regexp-opt getdp-constants-list 'symbols) 'font-lock-constant-face)
		))
  (setq indent-relative-blocks (append gmsh/getdp-block-statements getdp-block-statements))
  )

(add-to-list 'auto-mode-alist '("\\.pro\\'" . getdp-mode))

(add-to-list 'auto-mode-alist '("\\.geo\\'" . gmsh-mode))

(require 'info-look)

(info-lookup-add-help
 :mode 'getdp-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec '(("(getdp)Syntax Index" nil "")))

(info-lookup-add-help
 :mode 'gmsh-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec '(("(gmsh)Syntax Index" nil "")))

(defmacro stl2geo-vertex (pts cnt str name scale)
  "Read one vertex from stl file. Put it into hashtable pts with
index value cnt if it does not yet exist there else return its
index value in pts. Point moves behind the vertex."
  `(progn
     (search-forward "vertex")
     (let* ((b (current-buffer))
	    (pt (list (read b) (read b) (read b)))
	    (idx (gethash pt ,pts)))
       (unless idx
	 (puthash pt ,cnt ,pts)
	 (setq idx ,cnt)
	 (setq ,str (concat ,str "\np" ,name "[" (number-to-string idx) "]=newp; Point(p" ,name "[" (number-to-string idx) "])={"
			    (number-to-string (* ,scale (nth 0 pt))) ","
			    (number-to-string (* ,scale (nth 1 pt))) ","
			    (number-to-string (* ,scale (nth 2 pt))) ",mshSize};"))
	 (setq ,cnt (1+ ,cnt)))
       idx)))

(defmacro stl2geo-edge (edges p1 p2 cnt str name)
  `(let* (idx)
     (setq idx (gethash (cons ,p1 ,p2) ,edges))
     (unless idx
       (setq idx (gethash (cons ,p2 ,p1) ,edges))
       (when idx (setq idx (- idx))))
     (unless idx
       (puthash (cons ,p1 ,p2) ,cnt ,edges)
       (setq idx ,cnt)
       (setq ,str (concat ,str "\nl" ,name "[" (number-to-string idx) "]=newl; Line(l" ,name "[" (number-to-string idx)
			  "])={p" ,name "[" (number-to-string ,p1)
			  "],p" ,name "[" (number-to-string ,p2) "]};"))
       (setq ,cnt (1+ ,cnt)))
     idx))

(defun stl2geo (name scale geo)
  "Transformes stl grid of current buffer into gmsh geo file."
  (interactive "sName of entity:\nnScale factor:\nBGeo-buffer (without extension .geo):")
  (save-excursion
    (goto-char (point-min))
    (let* (facet
	   str
	   lineloop
	   (cntPts 0)
	   (cntEdges 1)
	   (cntLineLoop 0)
	   (size (/ (line-number-at-pos (point-max)) 7))
	   (pts (make-hash-table :test 'equal :size size))
	   (edges (make-hash-table :test 'equal :size size))
	   (geobuf (get-buffer-create geo)))
      (while (search-forward-regexp "^outer loop" nil 'noErr)
	(setq str nil)
	(setq facet (list (stl2geo-vertex pts cntPts str name scale)
			  (stl2geo-vertex pts cntPts str name scale)
			  (stl2geo-vertex pts cntPts str name scale)))
	(setq lineloop (list (stl2geo-edge edges (nth 0 facet) (nth 1 facet) cntEdges str name)
			     (stl2geo-edge edges (nth 1 facet) (nth 2 facet) cntEdges str name)
			     (stl2geo-edge edges (nth 2 facet) (nth 0 facet) cntEdges str name)))
	(setq str (concat str
			  "\nll" name "[" (number-to-string cntLineLoop) "]=newll; Line Loop(ll" name "[" (number-to-string cntLineLoop) "])={"
			  (if (< (nth 0 lineloop) 0 ) "-l" "l") name "[" (number-to-string (abs (nth 0 lineloop))) "],"
			  (if (< (nth 1 lineloop) 0 ) "-l" "l") name "[" (number-to-string (abs (nth 1 lineloop))) "],"
			  (if (< (nth 2 lineloop) 0 ) "-l" "l")  name "[" (number-to-string (abs (nth 2 lineloop))) "]};\n"
			  "s" name "[" (number-to-string cntLineLoop) "]=news; Plane Surface(s" name "[" (number-to-string cntLineLoop) "])={ll" name "[" (number-to-string cntLineLoop) "]};"
			  ))
	(setq cntLineLoop (1+ cntLineLoop))
	(with-current-buffer geobuf
	  (insert str)
	  )))))

(defun gmsh-import-stl (stl-file-name geo-entity-name scale)
  "Insert ASCII-STL mesh from file `stl-file-name' at point.
The geometrical entities are suffixed by `geo-entity-name'. The points are scaled by `scale'."
  (interactive "fSTL file:
sName suffix for geometrical entities:
nScale factor:")
  (let ((geo-buf (current-buffer)))
    (with-temp-buffer
      (insert-file-contents stl-file-name)
      (stl2geo geo-entity-name scale geo-buf))))

(easy-menu-define nil gmsh-mode-map
  "Menu for gmsh (import of ascii-stl)."
  '("Gmsh"
    ["Import ASCII STL" gmsh-import-stl t]))

(provide 'gmsh)
