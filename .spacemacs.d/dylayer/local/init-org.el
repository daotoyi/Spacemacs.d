;;; init-org.el ---- org & TODO.
;;; Commentary:
;;; Code:

(use-package org
  :defer nil
  :bind ("<f12>" . org-agenda)
  :config
  (add-hook 'org-mode-hook 'show-alert)
  (require 'org-install)    ;; autoload function, not when start org.
  (require 'org-agenda-gtd)
  (require 'ox-latex)       ;; should be require before org-latex, define usepackage. 
  (require 'org-latex)
  (require 'org-beamer)     ;; should be required after org-latex
  (require 'ox-latex-chinese)
  (setq org-startup-indented t
	org-src-fontify-natively t))		;; highlight
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

;;; org-language ----------------------------------------------------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t)
   (ditaa . t)
   (python . t)
   (latex . t)
   (emacs-lisp . t)
   ;; (sh . t)
   ;; (matlab . t)
   ;; (R . t)
   ;; (js . t)
   ))
;;; org-todo ----------------------------------------------------------------------------

;; (setq org-enforce-todo-dependencies t)    ;; main task cannot set done if subtask not finished.

(setq	org-todo-keywords '((sequence "TODO(t!)" "NEXT(n)" "WAIT(w)" "|" "CANC(c@/!)" "DONE(d!)"))
	    org-todo-keyword-faces '(("NEXT"     . "cyan")
	                             ("WAIT"  . "yellow")
	                             ("DONE"     . "green" )
	                             ("CANC" . (:foreground "orange" :weight bold))))
(setq org-log-done 'time)
;; (setq org-log-done 'note)

;;; org-tags ----------------------------------------------------------------------------
(setq org-tag-alist '(
                      (:startgroup . nil)
		                  ("Inbox" . ?i) ("Context" . ?c) ("Waiting" . ?w) ("Project" . ?p) ("Someday" . ?s) ("Reference" . ?r) ("Transh" . ?t)
		                  (:endgroup . nil)

		                  (:startgroup . nil)
		                  ("@Product" . ?P)
		                  (:grouptags . nil)
		                  ("P@tpe4k" . ?4) ("P@tpe5k" . ?5) ("P@tpe6k" . ?6) ("P@tre" . ?7) ("P@tsw" . ?8) ("P@other" . ?9)
		                  (:endgroup . nil)

                      ("Work" . ?W)   ("Hobby" . ?H)   ("Refine" . ?R) ("Invest" . ?I) ("Output" . ?O)
		                  (:newline . nil)
		                  ("ZhouYi" . ?Y) ("ZhongYi" . ?Z) ("Code" . ?C)
		                  (:newline . nil)
		                  ("1Quadrant" . ?1) ("2Quadrant" . ?2) ("3Quadrant" . ?3)
                      ))

(setq org-tags-exclude-from-inheritance '("Project"))
(setq org-tags-match-list-sublevels nil)

;;; org-capture --------------------------------------------------------------------------
(setq org-default-notes-file (concat refine-directory "GTD/inbox.org"))
(setq org-capture-templates
      '(
        ("1" "Task@quadrant1"  entry (file+headline org-agenda-file-task "Task&quadrant1")  "* TODO [#A] %? :1Quadrant:\n SCHEDULED: %T\n %i\n" :empty-lines 1)
        ("2" "Task@quadrant2"  entry (file+headline org-agenda-file-task "Task&quadrant2")  "* TODO [#B] %? :2Quadrant:\n SCHEDULED: %T\n %i\n" :empty-lines 1)
        ("3" "Task@quadrant3"  entry (file+headline org-agenda-file-task "Task&quadrant3")  "* TODO [#C] %? :3Quadrant:\n SCHEDULED: %T\n %i\n" :empty-lines 1)
        ;; ("w" "Task@work&Goal"  entry (file+headline org-agenda-file-task "Task@work&goal")  "* TODO [#A] %?            \n SCHEDULED: %T\n %i\n" :empty-lines 1)
        ("o" "Task@others"     entry (file+headline org-agenda-file-task "Task@others")     "* TODO [#C] %?            \n SCHEDULED: %T\n %i\n" :empty-lines 1)
        ("t" "Learning@tools"  entry (file+headline org-agenda-file-task "Learning@tools")  "* TODO [#D] %?            \n SCHEDULED: %T\n %i\n" :empty-lines 1)

        ;; ("n" "Notes"           entry (file+headline org-agenda-file-note "QuickNotes") "* [#C] %t %? :Inbox:\n %i\n"     :empty-lines 1)
        ;; ("i" "Ideas"           entry (file+headline org-agenda-file-note "QuickIdeas") "* %t %? :Inbox:\n %i\n"          :empty-lines 1)
        ("n" "Notes"           entry (file+headline org-agenda-file-note "QuickNotes") "* %t %? \n %i\n"     :empty-lines 1)
        ("i" "Ideas"           entry (file+headline org-agenda-file-note "QuickIdeas") "* %t %? \n %i\n"     :empty-lines 1)

        ("W" "Proj@work"     entry (file+headline org-agenda-file-project "Proj@work")   "* TODO [#B] [/] %? \n CREATED: %T\n %i\n"     :empty-lines 1)
        ("R" "Proj@read"     entry (file+headline org-agenda-file-project "Proj@read")   "* TODO [#D] %?     \n CREATED: %T\n %i\n"     :empty-lines 1)
        ("I" "Proj@invest"   entry (file+headline org-agenda-file-project "Proj@invest") "* TODO [#D] %?     \n CREATED: %T\n %i\n"     :empty-lines 1)
        ("H" "Proj@hobby"    entry (file+headline org-agenda-file-project "Proj@hobby")  "* TODO [#D] %?     \n CREATED: %T\n %i\n"     :empty-lines 1)
        ("O" "Proj@other"    entry (file+headline org-agenda-file-project "Proj@other")  "* TODO [#E] %?     \n CREATED: %T\n %i\n"     :empty-lines 1)
        ("r" "Proj@refine"   entry (file+headline org-agenda-file-project "Proj@refine") "* TODO [#D] %?     \n SCHEDULED: %T\n %i\n"   :empty-lines 1)
        )
      )

;;;; org-remember --->> org-capture
;; (autoload 'remember "remember" nil t)
;; (define-key global-map [f8] 'remember)
;; ;; (org-remember-insinuate)
;; (setq org-remember-templates '(("New"   ?n "* %? %t \n %i\n %a"   "E:/Refine/GTD/inbox.org" )
;; 			       ("Task"     ?t "** TODO %?\n %i\n %a" "E:/Refine/GTD/task.org" "Tasks")
;; 			       ("Calendar" ?c "** TODO %?\n %i\n %a" "E:/Refine/GTD/task.org" "Tasks")
;; 			       ("Idea"     ?i "** %?\n %i\n %a"      "E:/Refine/GTD/task.org" "Ideas")
;; 			       ("Note"     ?r "* %?\n %i\n %a"       "E:/Refine/GTD/note.org" )tps
;; 			       ("Project"  ?p "** %?\n %i\n %a"      "E:/Refine/GTD/project.org" %g)
;; 			       ))

;;; org-refile --------------------------------------------------------------------------
(define-key global-map "\C-cr" 'org-refile)
(setq org-agenda-files (list (concat refine-directory "GTD/task.org")
                             (concat refine-directory "GTD/note.org")
                             (concat refine-directory "GTD/project.org")
			     ))

(setq org-agenda-diary-file (concat refine-directory "GTD/diary.org")
      diary-file (concat refine-directory "GTD/diary.org"))

;; (setq org-refile-targets  '((org-agenda-file-finished :maxlevel . 2)
;;                             ;; (org-agenda-file-canceled :maxlevel . 9)
;;                            ))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-refile-use-outline-path t)            ;; file directly with IDO
(setq org-outline-path-complete-in-steps nil)   ;; Targets complete directly with IDO
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
;; (setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
;; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;;; Archive --------------------------------------------------------------------------
;;; Command: C-c C-x C-s
;;location for archive
(setq org-archive-location
      (concat refine-directory "GTD/_archive/"
              (format-time-string "%Y%m") "_archive.org::datetree/* Archive from %s"))
;;information added to property when a subtree is moved
(setq org-archive-save-context-info '(time file ltags itags todo category olpath))

;;; org-mobile -------------------------------------------------------------------------
(setq org-mobile-directory  (concat refine-directory "GTD/MobileOrg/"))
(setq org-mobile-encryption-tempfile (concat org-mobile-directory "orgtmpcrypt") )
(unless (file-exists-p org-mobile-encryption-tempfile)
  (shell-command (concat newencryption org-mobile-encryption-tempfile))
  )

(setq org-mobile-files org-agenda-files)
;; (setq org-mobile-files (list (concat refine-directory "GTD/")))
(setq org-mobile-inbox-for-pull (concat refine-directory "GTD/from_mobile.org"))
;; (setq org-mobile-inbox-for-pull (concat refine-directory "GTD/from-mobile.org"))
;;      (unless (file-exists-p org-mobile-inbox-for-pull)
;;        (shell-command (concat "touch " org-mobile-inbox-for-pull)))

;; sync on emacs init(pull) or exit(push)
(add-hook 'after-init-hook 'org-mobile-pull)
;; (add-hook 'kill-emacs-hook 'org-mobile-push) 

;;; moble sync -------------------------------------------------------------------------
(defvar org-mobile-sync-timer nil)
;; (defvar org-mobile-sync-idle-secs (* 60 10))
(defvar org-mobile-sync-idle-secs (* 60 60))
(defun org-mobile-sync ()
  "enable mobile org idle sync"
  (interactive)
  (org-mobile-pull)
  (org-mobile-push))

(defun org-mobile-sync-enable ()
  "enable mobile org idle sync"
  (interactive)
  (setq org-mobile-sync-timer
        (run-with-idle-timer org-mobile-sync-idle-secs t
                             'org-mobile-sync)))

(defun org-mobile-sync-disable ()
  "disable mobile org idle sync"
  (interactive)
  (cancel-timer org-mobile-sync-timer))

(org-mobile-sync-enable)

;;; org-pomodoro --------------------------------------------------------------------
;;; remove to dylayer/package#init-org-pomodoro.
;; C-c C-x C-i :start clock（donnot show on spaceline）/ clock in
;; C-c C-x C-o :stop clock                             / clock out
;; C-c C-x C-r :make dynamic table
;; C-c C-x C-r :refresh manual 

;; (global-set-key "\C-xps" 'org-pomodoro)                       ;; start org-pomodoro
;; (global-set-key "\C-xpv" 'spaceline-toggle-org-pomodoro-off)  ;; turn-off org-pomodoro
;; (global-set-key "\C-xpk" 'org-pomodoro-kill)                  ;; stop?
;; (global-set-key "\C-xpx" 'org-pomodoro-extend-last-clock)     ;; stop
;; (use-package org-pomodoro
;;   :config
;;   (setq org-pomodoro-audio-player "mplayer")
;;   (setq org-pomodoro-finished-sound-args "-volume 0.7")
;;   (setq org-pomodoro-long-break-sound-args "-volume 0.7")
;;   (setq org-pomodoro-short-break-sound-args "-volume 0.7")
;;   (setq org-pomodoro-ticking-sound-args "-volume 0.3")
;;   )

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
(provide 'init-org)
;;; init-org.el ends here
