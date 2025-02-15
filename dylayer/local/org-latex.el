;;; init-org-latex.el --- latex
;;; Commentary:
;;; Code:

;;org-export latex
(setq org-alphabetical-lists t)

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %f"
	"bibtex %b"  ;; 解决编译后参考文献显示？的问题
	"xelatex -interaction nonstopmode %f"
	"xelatex -interaction nonstopmode %f"
	"rm -fr %b.out %b.log %b.brf %b.bbl auto"
	))

(setq org-latex-compiler "xelatex")
(setq org-confirm-babel-evaluate nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org-latex-classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-export latex
;; above 16 usepackage commonly used mostly.
;; [NO-DEFAULT-PACKAGES] --- donnot load default usepackage.
(add-to-list 'org-latex-classes
	     '("article-basic"
	       "\\documentclass{article}
                \\usepackage{graphicx}
                \\usepackage{xcolor}
                \\usepackage{xeCJK}
                \\usepackage{fixltx2e}
                \\usepackage{longtable}
                \\usepackage{float}
                \\usepackage{tikz}
                \\usepackage{wrapfig}
                \\usepackage{latexsym,amssymb,amsmath}
                \\usepackage{textcomp}
                \\usepackage{listings}
                \\usepackage{marvosym}
                \\usepackage{textcomp}
                \\usepackage{latexsym}
                \\usepackage{natbib}
                \\usepackage{geometry}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("article-prefer"
               "\\documentclass{article}
                \\usepackage[utf8]{inputenc}
                \\usepackage{graphicx}
                \\usepackage{grffile}
                \\usepackage{longtable}
                \\usepackage{wrapfig}
                \\usepackage{rotating}
                \\usepackage[normalem]{ulem}
                \\usepackage{amsmath}
                \\usepackage{textcomp}
                \\usepackage{amssymb}
                \\usepackage{capt-of}
                \\usepackage{lscape}

                \\usepackage{ctex}
                \\usepackage{geometry}
                \\geometry{top=2cm,bottom=2cm,right=2cm,left=2.5cm}
                \\setcounter{secnumdepth}{3}
                \\setlength{\\parindent}{2em}
                \\setlength{\\parskip}{1ex}
                \\usepackage[colorlinks,linkcolor=black,anchorcolor=blue,citecolor=cyan]{hyperref}

                \\usepackage{fancyhdr}
                \\pagestyle{fancy}
                %\\lhead{left header}
                %\\chead{Techyauld}
                %\\rhead{right header}
                %\\lfoot{http://www.techyauld.com}
                %\\cfoot{第\\thepage页}
                %\\rfoot{right footer，modify \\today}

                \\usepackage{xcolor}
                \\usepackage{framed}
                \\usepackage{lipsum}
                \\colorlet{shadecolor}{blue!20}
                \\usepackage{tcolorbox}
                \\newtcbox{\\HL}[1][red]
                   {on line, arc = 0pt, outer arc = 0pt,
                    colback = #1!10!white, colframe = #1!50!black,
                    boxsep = 0pt, left = 1pt, right = 1pt, top = 2pt, bottom = 2pt,
                    boxrule = 0pt}

                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; dissertation template
(add-to-list 'org-latex-classes
	           '("dissertation"
		           "\\documentclass[UTF8,twoside,a4paper,12pt,openright]{ctexrep}
               \\input{~/.spacemacs.d/dylayer/option/latex-article-dissertation.tex
               %\\input{D:/Program Files (x86)/Emacs/spacemacs-develop/.spacemacs.d/dylayer/option/latex-article-dissertation.tex}

                [NO-DEFAULT-PACKAGES]
                [NO-PACKAGES]
                [EXTRA]"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org-export-latex-classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; boundp -- judge a variables if true or nil. 
;; (unless (boundp 'org-export-latex-classes)
;;   (setq org-export-latex-classes nil))
;; (add-to-list 'org-export-latex-classes
`
(add-to-list 'org-latex-classes
             '("article-cn"
               "\\documentclass[10pt,a4paper]{article}
               \\input{~/.spacemacs.d/dylayer/option/latex-article-cn.tex
               %\\input{D:/Program Files (x86)/Emacs/spacemacs-develop/.spacemacs.d/dylayer/option/latex-article-cn.tex}

                [NO-DEFAULT-PACKAGES]
                [NO-PACKAGES]"
	       ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; listing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; format code in listing usepackage 
(setq org-export-latex-listings t)
;; Options for \lset command（reference to listing Manual)
(setq org-export-latex-listings-options
      '(
        ("basicstyle" "\\color{foreground}\\small\\mono")           ; 源代码字体样式
        ("keywordstyle" "\\color{function}\\bfseries\\small\\mono") ; 关键词字体样式
        ("identifierstyle" "\\color{doc}\\small\\mono")
        ("commentstyle" "\\color{comment}\\small\\itshape")         ; 批注样式
        ("stringstyle" "\\color{string}\\small")                    ; 字符串样式
        ("showstringspaces" "false")                                ; 字符串空格显示
        ("numbers" "left")                                          ; 行号显示
        ("numberstyle" "\\color{preprocess}")                       ; 行号样式
        ("stepnumber" "1")                                          ; 行号递增
        ("backgroundcolor" "\\color{background}")                   ; 代码框背景色
        ("tabsize" "4")                                             ; TAB等效空格数
        ("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
        ("breaklines" "true")                                       ; 自动断行
        ("breakatwhitespace" "true")                                ; 只在空格分行
        ("showspaces" "false")                                      ; 显示空格
        ("columns" "flexible")                                      ; 列样式
        ("frame" "single")                                          ; 代码框：阴影盒
        ("frameround" "tttt")                                       ; 代码框： 圆角
        ("framesep" "0pt")
        ("framerule" "8pt")
        ("rulecolor" "\\color{background}")
        ("fillcolor" "\\color{white}")
        ("rulesepcolor" "\\color{comdil}")
        ("framexleftmargin" "10mm")
        ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-latex)
;;; org-latex.el ends here
