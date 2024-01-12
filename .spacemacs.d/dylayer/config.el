
;;; config.el for dylayer


;; basic setting.
;; --------------------------------------------------------------------------------------------

;; default directory.
;; (when (eq system-type 'windows-nt)
;;   (setq default-directory "e:/Refine/"))
;; (when (eq system-type 'gnu/linux)
;;   (setq default-directory "/mnt/e/Refine/"))

;; improve startup speed.
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; emacs title
;; (setq-default frame-title-format '("%f"))
;; (setq frame-title-format "Spacemacs(daotoyi)@%b")
(setq frame-title-format
	    '(:eval (concat "Spacemacs - "
			         (if (and buffer-file-name (buffer-modified-p)) "•")
			         (buffer-name)
			         (if buffer-file-name
				           (concat " - [" (directory-file-name (abbreviate-file-name default-directory)) "]"))
			         ))
	    )


;; emoji
;; (set-fontset-font t 'symbol (font-spec :family "EmojiOne Color") nil 'prepend)
(set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend)

;;(setq mouse-yank-at-point t)
(mouse-avoidance-mode 'animate)

;; reload file modified by other editor.
(global-auto-revert-mode t)

;; save-some-buffers(C-x s) without querying user
(setq save-silently-p t)
(setq auto-save-silent t)

;; all backups goto ~/.backups instead in the current directory
(setq backup-directory-alist (quote (("." . "e:/TMP/TmpFiles"))))


;; Dired
;; -----------------------------------------------------------------------
;; dired, show file(.org) with yellow in buffer.
;; (setq dired-recursive-deletes 'always)
;; (setq dired-recursive-copies 'always)
(add-hook 'dired-mode-hook
          (lambda ()
            (highlight-lines-matching-regexp "\.org$" 'hi-yellow)))

;; suppress (dired)warning and skip setup silently
(setq dired-quick-sort-setup t)
(setq dired-quick-sort-suppress-setup-warning t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq initial-major-mode 'org   ;; lisp-interaction-mode  ;; spacemacs default text-mode
      initial-scratch-message
      "\n# Configuration by <github.com/daotoyi/spacemacs.d>.\n# Enjoy!\n\n")

;; dired-mode always occupy a buffer
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; -----------------------------------------------------------------------
;; org
;; -----------------------------------------------------------------------
;; display Chinese date (eg.2021-12-24 周五 冬月廿一)
(setq org-agenda-format-date 'dy/org-agenda-format-date-aligned)

(setq org-enforce-todo-dependencies t)
;; highlight in code-block
(setq org-src-fontify-natively t)
;; (evil-set-initial-state 'calendar-mode 'emacs)
;; (evil-set-initial-state 'dired-mode 'emacs)

;; calendar
;; --------------------------------------------------------------------------------------------
;; (setq calendar-load-hook
;;       '(lambda ()
;;          (set-face-foreground 'diary-face "skyblue")
;;          ;; (set-face-background 'holiday-face "slate blue")
;;          ;; (set-face-foreground 'holiday-face "white")
;;          ))

;; -----------------------------------------------------------------------
(setq cal-china-x-general-holidays
      '(;; green background
        (holiday-fixed 5 1 "劳动节")
        (holiday-fixed 5 2 "劳动节")
        (holiday-fixed 5 3 "劳动节")
        (holiday-fixed 5 4 "劳动节")
        (holiday-fixed 5 5 "劳动节")
        (holiday-fixed 10 1 "国庆节")
        (holiday-fixed 10 2 "国庆节")
        (holiday-fixed 10 3 "国庆节")
        (holiday-fixed 10 4 "国庆节")
        (holiday-fixed 10 5 "国庆节")
        (holiday-fixed 10 6 "国庆节")
        (holiday-fixed 10 7 "国庆节")
        (holiday-lunar 12 30 "除夕" 0)
        (holiday-lunar 1 1 "春节" 0)
        (holiday-lunar 1 2 "春节" 0)
        (holiday-lunar 1 3 "春节" 0)
        (holiday-lunar 1 4 "春节" 0)
        (holiday-lunar 1 5 "春节" 0)
        (holiday-lunar 1 6 "春节" 0)
        ))
(setq cal-china-x-solar-term
      '(;; yellow background
        (holiday-solar-term "立春" "立春")
        (holiday-solar-term "春分" "春分")
        (holiday-solar-term "立夏" "立夏")
        (holiday-solar-term "夏至" "夏至")
        (holiday-solar-term "立秋" "立秋")
        (holiday-solar-term "秋分" "秋分")
        (holiday-solar-term "立冬" "立冬")
        (holiday-solar-term "冬至" "冬至")
        ))
(setq general-holidays
      '((holiday-lunar 1 5 "破五" 0)
        (holiday-lunar 2 2 "龙抬头" 0)
        (holiday-lunar 1 15 "元宵节" 0)
        (holiday-fixed 2 14 "情人节")
        (holiday-lunar 3 3 "上巳节" 0)
        (holiday-lunar 4 8 "佛诞" 0)
        (holiday-float 5 0 2 "母亲节")
        (holiday-float 6 0 3 "父亲节")
        (holiday-lunar 7 7 "七夕" 0)
        (holiday-lunar 7 15 "中元节" 0)
        (holiday-lunar 9 9 "重阳节" 0)
        (holiday-fixed 9 10 "教师节")
        (holiday-lunar 10 15 "下元节")
        (holiday-lunar 12 8 "腊八" 0)
        (holiday-lunar 12 23 "小年" 0)
        (holiday-fixed 12 24 "平安夜")
        ;; (holiday-lunar 12 30 "除夕" 0)
        ))
(setq cal-china-x-dy-holidays
      '(;; cyan background. anniversay
        (holiday-lunar 3 15 "father birthday" 0)
        (holiday-lunar 3 28 "juan&little birthday"  0)
        (holiday-lunar 7 9 "lili birthday" 0)
        (holiday-lunar 7 12 "mother birthday" 0)
        (holiday-lunar 12 11 "daoyi birthday" 0)
        ))


;; -----------------------------------------------------------------------
;; flyspell
(setq ispell-dictionary nil)
(setq ispell-program-name "D:/Soft/hunspell/bin/hunspell.exe")
;; "en_US" is key to lookup in `ispell-local-dictionary-alist'.
;; Please note it will be passed as default value to hunspell CLI `-d` option
;; if you don't manually setup `-d` in `ispell-local-dictionary-alist`
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US"
         "[[:alpha:]]"
         "[^[:alpha:]]"
         "[']"
         nil
         ("-d" "en_US")
         nil
         utf-8)))
(setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)


;; atuo switch input method in evil iner and nromal mode
(defun emacs-ime-disable ()
  (w32-set-ime-open-status nil))

(defun emacs-ime-enable ()
  (w32-set-ime-open-status t))

(add-hook 'evil-insert-state-entry-hook 'emacs-ime-enable)
(add-hook 'evil-insert-state-exit-hook 'emacs-ime-disable)


;; newsticker
(setq newsticker-frontend 'newsticker-plainview)

(add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)
(setq newsticker-retrieval-interval 0
      newsticker-ticker-interval 0)
(setq newsticker-url-list-defaults nil)    ;remove default list (i.e. emacswiki)
(setq newsticker-automatically-mark-items-as-old nil)
;; extern exec
(setq newsticker-retrieval-method 'extern)
(setq newsticker-wget-name "curl")
(setq newsticker-wget-arguments '("--disable" "--silent" "--location" ))
(setq newsticker-url-list
      '(
        ;; formatj: ("title" "URL" other options)
        ("Planet Emacs Life" "https://planet.emacslife.com/atom.xml" nil nil nil)
        ("Emcas Info"        "https://jherrlin.github.io/index.xml" nil nil nil)
        ("Ruan Yifeng"       "http://www.ruanyifeng.com/blog/atom.xml" nil nil nil)
        ))

