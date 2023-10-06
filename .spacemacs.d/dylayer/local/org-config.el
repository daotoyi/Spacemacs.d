;;; org-config.el
;;; Commentary:
;;; Code:


(when (eq system-type 'windows-nt)
  (setq refine-directory "E:/Refine/"
        org-directory "E:/Refine/Org/"
        newencryption "type nul>"
        ))
(when (eq system-type 'gnu/linux)
  (setq refine-directory "/mnt/e/Refine/"
        org-directory "/mnt/e/Refine/Org/"
        newencryption "touch "
        ))


(setq org-startup-indented t
      org-src-fontify-natively t)		;; highlight

(add-hook 'org-mode-hook 'show-alert)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . org-mode))
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.js\\'" . js2-mode))
;;        '(("\\.sh\\'" . org-mode))
;;        '(("\\.el\\'" . emacs-lisp-mode))
;;        auto-mode-list))

(add-hook 'org-mode-hook 'turn-on-font-lock)  ;;; not needed when global-font-lock-mode is on
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))   ;; auto word wrap
;; (add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
;; (add-hook 'org-mode-hook 'turn-on-auto-fill)


;;; org-language
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t)
   (ditaa . t)
   (python . t)
   (latex . t)
   (emacs-lisp . t)
   (js . t)
   (shell . t)
   ;; (matlab . t)
   ;; (R . t)
   ))


(defun show-alert()
  ";; 解决两个“@”不能生成alert效果"
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
                                        ("@" "\\alert{%s}" nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'org-config)
