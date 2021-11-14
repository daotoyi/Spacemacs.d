;;; init-org-beamer.el --- beamer
;;; Commentary:
;;; Code:

;; 解决两个“@”不能生成alert效果
(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>") 
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
				 ("~" org-verbatim "<code>" "</code>" verbatim)
                                 ("+" (:strike-through t) "<del>" "</del>")
                                 ("@" org-warning "<b>" "</b>")))
      org-export-latex-emphasis-alist (quote 
                                       (("*" "\\textbf{%s}" nil)
                                        ("/" "\\emph{%s}" nil) 
                                        ("_" "\\underline{%s}" nil)
                                        ("+" "\\texttt{%s}" nil)
                                        ("=" "\\verb=%s=" nil)
                                        ("~" "\\verb~%s~" t)
                                        ("@" "\\alert{%s}" nil))))

(setq ps-paper-type 'a4
      ps-font-size 16.0
      ps-print-header nil
      ps-landscape-mode nil)

;; #+LaTeX_CLASS: beamer
(add-to-list 'org-latex-classes
             '("beamer-prefer"
               "\\documentclass[xcolor=dvipsnames,svgnames,x11names,11pt,bigger,presentation]{beamer}
                \\usepackage[UTF8,space,hyperref]{ctex}
                %\\usepackage[dvipsnames,svgnames,x11names]{xcolor}
                \\usepackage{graphicx}
                \\usepackage{grffile}
                \\usepackage{wrapfig}
                \\usepackage{capt-of}
                \\usepackage{longtable}
                \\usepackage[abs]{overpic}
                \\usepackage{hyperref}
                \\usepackage{rotating}
                \\usepackage[none]{hyphenat}
                \\usepackage[normalem]{ulem}
                \\usepackage{fontspec}
                \\usepackage{amsmath}
                \\usepackage{textcomp}
                \\usepackage{amssymb}
                \\hypersetup{colorlinks=true,linkcolor=red}
                \\usepackage[backend = biber, natbib=true, style = science, sorting = none]{biblatex}

               % --- theme --- % --- setbeamer --- % ---------------- % ---------------- %
 
                \\usetheme{CambridgeUS}    % red    space
                %\\usetheme{AnnArbor}      % yellow space
                %\\usetheme{Berlin}        % top three item, overview
                %\\usetheme{Boadilla}      % purple theme
                %\\usetheme{Darmstadt}     % blue, top double item, overview
                
                \\usecolortheme[named=blue]{structure}
                %\\usecolortheme{wolverine}	  % [blue & yellow & orange] item; {crane}% orange
                %\\useoutertheme{infolines}
                
                \\usefonttheme[onlymath]{serif}                
                
                \\setbeamersize{text margin left=10mm, text margin right=10mm}
                \\setbeamertemplate{items}[circle]     % default=triangle, ball, circle, rectangle
                \\setbeamertemplate{blocks}[rounded][shadow=true]
                \\setbeamertemplate{bibliography item}[text]
                %\\setbeamertemplate{navigation symbols}{}    % Remove navigation icon
                \\setbeamercolor{normal text}{fg=black}	      % foreground color,default black
                \\setbeamercolor{structure}{fg=blue}
                \\graphicspath{pic file folder}
                
                \\AtBeginBibliography{\\footnotesize}
                \\AtBeginSection{\\begin{frame}{Outline}   
                \\tableofcontents[currentsection]
                \\end{frame} }

                % --- titile --- % ---------------- % ---------------- % ---------------- %
                \\title[BeamerTitle]{Beamer template}
                \\subtitle[option]{Unity of knowledge and action}
                \\author{SHI WENHUA}
                \\date{\\today}
                \\institute[Techyauld]{
                  Department of Customer Service\\\\
                  Beijing techyauld Technology Development Co. Ltd.\\\\
                  6th Zone 2nd Floor, Building 21st, Zpark, Haidian District, Beijing 100193, P.R.China\\\\[1ex]
                  \\texttt{whshi@techyauld}}

                % --- footnote --- % ---------------- % ---------------- % ---------------- %
                \\usepackage[absolute,overlay]{textpos}
                \\newenvironment{reference}[2]{
                  \\begin{textblock*}{
                     \\textwidth}(#1,#2)
                     \\footnotesize\\it\\bgroup\\color{red!50!black}}{\\egroup
                 \\end{textblock*}}"

	       ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

;; #+LaTeX_CLASS: beamer
(add-to-list 'org-latex-classes
             '("beamer-input"
               "\\documentclass[compress,xcolor=dvipsnames,svgnames,x11names,11pt,bigger,presentation,notheorems]{beamer}
               \\input{D:/Program Files (x86)/Emacs/myemacs/.emacs.d/option/preface-for-beamer.tex}"
	       ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-beamer)
;;; org-beamer.el ends here
