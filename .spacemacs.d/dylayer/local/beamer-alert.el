;;; beamer-alert.el --- beamer
;;; Commentary:
;;; Code:


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'beamer-alert)
;;; org-beamer.el ends here
