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
                
                \\usepackage{ctex}
                \\usepackage{geometry}
                \\setcounter{secnumdepth}{3}
                \\setlength{\\parindent}{2em}
                \\setlength{\\parskip}{1ex}
                \\usepackage[colorlinks,linkcolor=black,anchorcolor=blue,citecolor=cyan]{hyperref}
                
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
                  \\setcounter{secnumdepth}{4}
                  \\usepackage[linkcolor=blue,citecolor=blue,backref=page]{hyperref}
                  \\hypersetup{hidelinks}
                  \\usepackage{xeCJK}
                  \\usepackage{fontspec}
                  \\setCJKmainfont{SimSun}
                  \\setCJKmonofont{SimSun}
                  \\setCJKfamilyfont{kaiti}{KaiTi}
                  \\newcommand{\\KaiTi}{\\CJKfamily{kaiti}}
                  \\setmainfont{Times New Roman}
                  \\usepackage[namelimits]{amsmath}
                  \\usepackage{amssymb}
                  \\usepackage{mathrsfs}
                  \\newcommand{\\chuhao}{\\fontsize{42.2pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\xiaochu}{\\fontsize{36.1pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\yihao}{\\fontsize{26.1pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\xiaoyi}{\\fontsize{24.1pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\erhao}{\\fontsize{22.1pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\xiaoer}{\\fontsize{18.1pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\sanhao}{\\fontsize{16.1pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\xiaosan}{\\fontsize{15.1pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\sihao}{\\fontsize{14.1pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\xiaosi}{\\fontsize{12.1pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\wuhao}{\\fontsize{10.5pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\xiaowu}{\\fontsize{9.0pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\liuhao}{\\fontsize{7.5pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\xiaoliu}{\\fontsize{6.5pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\qihao}{\\fontsize{5.5pt}{\\baselineskip}\\selectfont}
                  \\newcommand{\\bahao}{\\fontsize{5.0pt}{\\baselineskip}\\selectfont}
                  \\usepackage{color}
                  \\usepackage{geometry}
                  \\geometry{top=2cm,bottom=2cm,right=2cm,left=2.5cm}
                  \\geometry{headsep=0.5cm}
                  \\usepackage{setspace}
                  \\setlength{\\baselineskip}{22pt}
                  \\setlength{\\parskip}{0pt}
                  \\usepackage{enumerate}
                  \\usepackage{enumitem}
                  \\setenumerate[1]{itemsep=0pt,partopsep=0pt,parsep=\\parskip,topsep=5pt}
                  \\setitemize[1]{itemsep=0pt,partopsep=0pt,parsep=\\parskip,topsep=5pt}
                  \\setdescription{itemsep=0pt,partopsep=0pt,parsep=\\parskip,topsep=5pt}
                  \\usepackage{fancyhdr}
                        \\pagestyle{fancy}
                        \\fancyhead{}
                        \\fancyhead[CE]{\\KaiTi \\wuhao xxxx}
                        \\fancyhead[CO]{\\KaiTi \\wuhao xxxxxx}
                        \\fancypagestyle{plain}{\\pagestyle{fancy}}
                  \\ctexset{contentsname=\\heiti{目{\\quad}录}}
                  \\ctexset{section={format=\\raggedright}}
                  \\usepackage{titlesec}
                        \\titleformat{\\chapter}[block]{\\normalfont\\xiaoer\\bfseries\\centering\\heiti}{第{\\zhnumber{\\thechapter}}章}{10pt}{\\xiaoer}
                        \\titleformat{\\section}[block]{\\normalfont\\xiaosan\\bfseries\\heiti}{\\thesection}{10pt}{\\xiaosan}
                        \\titleformat{\\subsection}[block]{\\normalfont\\sihao\\bfseries\\heiti}{\\thesubsection}{10pt}{\\sihao}
                        \\titleformat{\\subsubsection}[block]{\\normalfont\\sihao\\bfseries\\heiti}{\\thesubsubsection}{10pt}{\\sihao}
                        \\titlespacing{\\chapter} {0pt}{-22pt}{0pt}{}
                        \\titlespacing{\\section} {0pt}{0pt}{0pt}
                        \\titlespacing{\\subsection} {0pt}{0pt}{0pt}
                        \\titlespacing{\\subsubsection} {0pt}{0pt}{0pt}
                  \\usepackage[super,square,numbers,sort&compress]{natbib}
                  \\usepackage{graphicx}
                  \\usepackage{subfigure}
                  \\usepackage{caption}
                  \\captionsetup{font={small}}
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
                \\usepackage{graphicx}
                \\usepackage{xcolor}
                \\usepackage{xeCJK}
                \\usepackage{lmodern}
                \\usepackage{verbatim}
                \\usepackage{fixltx2e}
                \\usepackage{longtable}
                \\usepackage{float}
                \\usepackage{tikz}
                \\usepackage{wrapfig}
                \\usepackage{soul}
                \\usepackage{textcomp}
                \\usepackage{listings}
                \\usepackage{geometry}
                \\usepackage{algorithm}
                \\usepackage{algorithmic}
                \\usepackage{marvosym}
                \\usepackage{wasysym}
                \\usepackage{latexsym}
                \\usepackage{natbib}
                \\usepackage{fancyhdr}
                \\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
                    linkcolor=blue,
                    urlcolor=blue,
                    menucolor=blue]{hyperref}
                \\usepackage{fontspec,xunicode,xltxtra}
                \\setmainfont[BoldFont=Adobe Heiti Std]{Adobe Song Std}  
                \\setsansfont[BoldFont=Adobe Heiti Std]{AR PL UKai CN}  
                \\setmonofont{Bitstream Vera Sans Mono}  
                \\newcommand\\fontnamemono{AR PL UKai CN}%等宽字体
                \\newfontinstance\\MONO{\\fontnamemono}
                \\newcommand{\\mono}[1]{{\\MONO #1}}
                \\setCJKmainfont[Scale=0.9]{Adobe Heiti Std}%中文字体
                \\setCJKmonofont[Scale=0.9]{Adobe Heiti Std}
                \\hypersetup{unicode=true}
                \\geometry{a4paper, textwidth=6.5in, textheight=10in,
                    marginparsep=7pt, marginparwidth=.6in}
                \\definecolor{foreground}{RGB}{220220,204}%浅灰
                \\definecolor{background}{RGB}{62,62,62}%浅黑
                \\definecolor{preprocess}{RGB}{250,187,249}%浅紫
                \\definecolor{var}{RGB}{239,224,174}%浅肉色
                \\definecolor{string}{RGB}{154,150,230}%浅紫色
                \\definecolor{type}{RGB}{225,225,116}%浅黄
                \\definecolor{function}{RGB}{140,206,211}%浅天蓝
                \\definecolor{keyword}{RGB}{239,224,174}%浅肉色
                \\definecolor{comment}{RGB}{180,98,4}%深褐色
                \\definecolor{doc}{RGB}{175,215,175}%浅铅绿
                \\definecolor{comdil}{RGB}{111,128,111}%深灰
                \\definecolor{constant}{RGB}{220,162,170}%粉红
                \\definecolor{buildin}{RGB}{127,159,127}%深铅绿
                \\punctstyle{kaiming}
                \\title{}
                \\fancyfoot[C]{\\bfseries\\thepage}
                \\chead{\\MakeUppercase\\sectionmark}
                \\pagestyle{fancy}
                \\tolerance=1000
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
