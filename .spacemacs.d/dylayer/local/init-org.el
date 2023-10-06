;;; init-org.el ---- org entery.
;;; Commentary:
;;; Code:


(use-package org
  :defer nil
  :bind ("<f12>" . org-agenda)
  :config
  (require 'org-install)    ;; autoload function, not when start org.
  (require 'org-config)
  (require 'org-agenda-config)
  (require 'org-agenda-func)
  (require 'beamer-alert)
  (require 'ox-latex)       ;; should be require before org-latex, define usepackage. 
  (require 'org-latex)
  (require 'org-beamer)     ;; should be required after org-latex
  (require 'ox-latex-chinese))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-org)
;;; init-org.el ends here
