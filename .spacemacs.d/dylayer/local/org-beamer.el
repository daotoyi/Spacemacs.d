;;; init-org-beamer.el --- beamer
;;; Commentary:
;;; Code:


(setq ps-paper-type 'a4
      ps-font-size 16.0
      ps-print-header nil
      ps-landscape-mode nil)

;; #+LaTeX_CLASS: beamer (basic)
(eval-after-load "ox-latex"
  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure
  '(add-to-list 'org-latex-classes
                `("beamer"
                  ,(concat "\\documentclass[presentation]{beamer}\n"
                           "[DEFAULT-PACKAGES]"
                           "[PACKAGES]"
                           "[EXTRA]\n"
                           "\\usepackage{xeCJK}"
                           )
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; #+LaTeX_CLASS: beamer (input method)
(add-to-list 'org-latex-classes
             '("beamer-awesome"
               "\\documentclass[compress,xcolor=dvipsnames,svgnames,x11names,11pt,bigger,presentation,notheorems]{beamer}
               \\input{D:/Program Files (x86)/Emacs/spacemacs-develop/.spacemacs.d/dylayer/option/preface-for-beamer.tex}"

	             ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")

               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

;; #+LaTeX_CLASS: beamer business (input method)
(add-to-list 'org-latex-classes
             '("beamer-business"
               "\\documentclass[compress,xcolor=dvipsnames,svgnames,x11names,10pt,bigger,presentation,notheorems]{beamer}
               \\input{D:/Program Files (x86)/Emacs/spacemacs-develop/.spacemacs.d/dylayer/option/preface-for-beamer-business.tex}"

	             ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")

               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

;; #+LaTeX_CLASS: beamer (customize here)
(add-to-list 'org-latex-classes
             '("beamer-prefer"
               "\\documentclass[xcolor=dvipsnames,svgnames,x11names,10pt,bigger,presentation]{beamer}
                \\usepackage[UTF8,space,hyperref]{ctex}
                %\\usepackage[dvipsnames,svgnames,x11names]{xcolor}

                \\usepackage{grffile}
                \\usepackage[abs]{overpic}
                \\usepackage{hyperref}
                \\usepackage{rotating}
                \\usepackage[none]{hyphenat}
                \\usepackage{fontspec}
                \\usepackage{textcomp}
                \\usepackage{setspace}
                %\\hypersetup{colorlinks=true,linkcolor=red}
                \\usepackage[backend = biber, natbib=true, style = science, sorting = none]{biblatex}

               % --- theme --- % --- setbeamer --- % ---------------- % ---------------- %

                \\usetheme{CambridgeUS}    % red    space
                %\\usetheme{AnnArbor}      % yellow space
                %\\usetheme{Berlin}        % top three item, overview
                %\\usetheme{Boadilla}      % purple theme
                %\\usetheme{Darmstadt}     % blue, top double item, overview

                %\\usecolortheme[named=blue]{structure}
                \\usecolortheme{wolverine}	  % [blue & yellow & orange] item; {crane}% orange
                \\useoutertheme{infolines}
                \\usefonttheme[onlymath]{serif}

                \\setbeamercolor{normal text}{fg=black}	      % foreground color,default black
                \\setbeamercolor{structure}{fg=blue}          % color of itemize/enumerate symbol

                \\graphicspath{figure/}
                \\setbeamersize{text margin left=10mm, text margin right=10mm}
                \\setbeamertemplate{items}[circle]           % default=triangle, ball, circle, rectangle
                \\setbeamertemplate{blocks}[rounded][shadow=true]
                \\setbeamertemplate{bibliography item}[text]
                %\\setbeamertemplate{navigation symbols}{}    % Remove navigation icon

                % --- table of content ------------ % ---------------- % ---------------- %
                \\AtBeginBibliography{\\footnotesize}
                \\AtBeginSection{\\begin{frame}{Outline}
                \\tableofcontents[currentsection]
                \\end{frame} }

                %\\setbeamertemplate{section in toc}[square]
                %\\setbeamercolor*{item projected}{fg=black, bg=blue!50!blue} % color of symbol before section
                %\\setbeamercolor{section in toc}{fg=black}
                %\\setbeamertemplate{subsection in toc}[circle]
                %\\setbeamercolor{subsection in toc}{fg=blue}

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
                     \\footnotesize\\it\\bgroup\\color{black!50!black}}{\\egroup
                 \\end{textblock*}}"

               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-beamer)
;;; org-beamer.el ends here
