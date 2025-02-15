;;; packages.el --- dylayer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author:  <SWH@HUAWEI>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `dylayer-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `dylayer/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `dylayer/pre-init-PACKAGE' and/or
;;   `dylayer/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst dylayer-packages
  '()
  "The list of Lisp packages required by the dylayer layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; ==================================================================================================
(add-to-list 'load-path "~/.spacemacs.d/dylayer/local/")

;; add package
(defconst dylayer-packages
  '(
    ;; location: from build-in. e.g.:
    ;; (occur-mode :location built-in)

    ;; location: from github
    ;; (volume :location (recipe :fetcher github :repo "dbrock/volume.el"))

    ;; volume
    general
    org
    org-pomodoro
    ox-hugo
    easy-hugo
    org-download
    org-attach-screenshot
    elfeed-dashboard
    newsticker
    ivy     ;; ivy / swiper multiple-cursor / sounsel
    ;; multiple-cursors
    evil
    ;; youdao-dictionary
    fanyi
    disable-mouse
    super-save
    ;; python
    ;; flymd
    ;; company-jedi
    ;; bongo
    emms
    yasnippet
    htmlize
    ;; grip-mode
    ;; rime
    emojify
    ;; (popweb :location built-in) ;; performace better on linux
    ))

;; ---------------------------------------------------------------------
(defun dylayer/init-volume()
    (use-package volume
      :init
      :config
      (setq volume-backend 'volume-aumix-backend)
      (when (eq system-type 'windows-nt)
        (setq volume-backend 'volume-mixer-backend)) ;; mixer.exe deprated. not available
      ))

(defun dylayer/init-htmlize()
  (use-package htmlize
    :init))

(defun dylayer/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :init
    (setq url-automatic-caching t)
    ;; (setq app_id  "00034f28dc3b90c2")
    ;; (setq app_key "gsMj3KBIjisTz8J8TdPmB23EDsOje6W8")
    ;; (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
    ;; (global-set-key (kbd "C-q") 'youdao-dictionary-search-at-point+)
    ))

(defun dylayer/init-fanyi ()
  (use-package fanyi
    :ensure t
    ;; :defer t
    :init
    ;; (setq fanyi-auto-select nil)
    (add-to-list 'exec-path "d:/Program Files/MPlayer for Windows/")
    (setq fanyi-sound-player "mplayer")
    (setq fanyi-sound-player-support-https t)
    :custom
    (fanyi-providers '(fanyi-haici-provider
                       fanyi-youdao-thesaurus-provider
                       fanyi-etymon-provider
                       fanyi-longman-provider
                       ;; fanyi-libre-provider
                       ))))

;; init occur mode (from built-in)
;; ---------------------------------------------------------------------
;; M-n / M-p go to previous / next position in source-buffers.
(defun dylayer/init-occur-mode ()
  "List lines matching regexp in new buffer"
  (evilified-state-evilify-map occur-mode-map
    :mode occur-mmode))

;; init (from github)
;; ---------------------------------------------------------------------
;; (defun dylayer/init-gulpjs ()
;;   (use-package gulpjs
;;     :init))

;; post-init org-agenda
;; ---------------------------------------------------------------------
(defun dylayer/init-org()
  ;; <s tab company, not work. replaced by org-insert-structure-template.
  ;; (require 'org-tempo)

  (use-package org
   :defer t
   :config
   (require 'init-org)
   ;; (require 'init-roll)

   ;; (setq org-startup-folded 'overview)
   (setq org-startup-folded t) ;; overview

   ;; don't work conflict with yasnipptes.
   ;; (require 'ox-publish)

   ;; visual alignment for Org Mode, Markdown and table.el tables on GUI Emacs. 
   ;; the effect in not good, deperated.
   ;; (require 'valign)
   ;; (add-hook 'org-mode-hook #'valign-mode)
   ;; (add-hook 'markdown-mode-hook #'valign-mode)

   (require 'cal-china-x)
   ;; (require 'cal-china-x-with-term)
   (setq calendar-latitude +39.54
         calendar-longitude +116.28
         calendar-location-name "北京")
   (setq mark-holidays-in-calendar t)
   (setq calendar-mark-holidays-flag t)
   (setq calendar-week-start-day 1)            ;; Monday is first day of weak.
   (setq mark-diary-entries-in-calendar t)     ;; mark the day for diary
   (setq view-calendar-holidays-initially t)
   (setq cal-china-x-important-holidays
         cal-china-x-chinese-holidays)
   (setq calendar-holidays
         (append cal-china-x-important-holidays
                 ;; holiday-general-holidays ;; foreign holidays
                 holiday-christian-holidays
                 ;; dy/config
                 cal-china-x-general-holidays
                 cal-china-x-solar-term
                 general-holidays
                 cal-china-x-dy-holidays
          ))
   ))

(defun dylayer/init-org-pomodoro()
  (use-package org-pomodoro
    :config
    ;; (add-to-list 'exec-path "d:/Program Files/MPlayer for Windows/")
    (setq org-pomodoro-audio-player "mplayer")
    (setq org-pomodoro-finished-sound-args "-volume 0.7")
    (setq org-pomodoro-long-break-sound-args "-volume 0.7")
    (setq org-pomodoro-short-break-sound-args "-volume 0.7")
    (setq org-pomodoro-ticking-sound-args "-volume 0.3")
    (global-set-key "\C-xps" 'org-pomodoro)                       ;; start org-pomodoro
    (global-set-key "\C-xpv" 'spaceline-toggle-org-pomodoro-off)  ;; turn-off org-pomodoro
    (global-set-key "\C-xpk" 'org-pomodoro-kill)                  ;; stop?
    (global-set-key "\C-xpx" 'org-pomodoro-extend-last-clock)     ;; stop
    ))

(defun dylayer/init-ox-hugo()
  (use-package ox-hugo
    :ensure t
    :after ox
    :config
    ;; (setq org-hugo-section "post"
    ;;       org-hugo-auto-set-lastmod	t)

    ;; when save org-fils, auto-export to post"
    (org-hugo-auto-export-mode)
    ;; put bebow to "blog/.dir-locals.el"
    ;; (("content-org/"
    ;;   . ((org-mode . ((eval . (org-hugo-auto-export-mode)))))))
    ))

(defun dylayer/init-easy-hugo()
  (use-package easy-hugo
    :init
    (setq easy-hugo-default-ext ".org")
    (setq easy-hugo-basedir "~/refine/"
	        easy-hugo-url "https://daotoyi.github.io")
    (unless (file-exists-p easy-hugo-basedir)
      (make-directory easy-hugo-basedir))

    ;; (setq easy-hugo-bloglist
    ;;       '(;; blog for github pages
    ;;         ((easy-hugo-basedir . "E:/Refine/daotoyi.github.io/FIRE/")
    ;;          (easy-hugo-url . "https://fire.daotoyi.cn"))
    ;;         ((easy-hugo-basedir . "E:/Refine/daotoyi.github.io/Blog/")
    ;;          (easy-hugo-url . "https://daotoyi.github.io")
    ;;          )))
    :config
    (easy-hugo-enable-menu)
    ))

(defun dylayer/init-org-download()
  (use-package org-download
    :ensure t
    :bind
    ("C-S-y" . org-download-yank)
    :config
    ;; (require 'org-download)
    ;; Drag and drop to Dired
    (add-hook 'dired-mode-hook 'org-download-enable)

    ;; (setq org-download-method 'directory)
    ;; (setq-default org-download-image-dir "E:/Refine/daotoyi.github.io/OrgBlog/images/")

    ;; (setq org-download-timestamp t
    ;;       org-download-heading-lvl nil
    ;;       org-download-screenshot-method "convert"
    ;;       ;; org-download-screenshot-method "magick import"
    ;;       )

    (defun org-mode-after-save-hook()
      "do sth after save hook in org mode major"
      (when (eq major-mode 'org-mode)
        (progn
          (org-display-inline-images t))))
    (add-hook 'after-save-hook 'org-mode-after-save-hook)
    ))

(defun dylayer/init-org-attach-screenshot()
  (use-package org-attach-screenshot
    :bind ("C-c s s" . org-attach-screenshot)
    :config
    (setq ;;org-attach-screenshot-auto-refresh always
          org-attach-screenshot-relative-links t)
    (setq org-attach-screenshot-dirfunction
		      (lambda ()
		        (progn (cl-assert (buffer-file-name))
			             (concat (file-name-sans-extension (buffer-file-name))
				                   ""
				                   ;; "-att"
                           )))
		      org-attach-screenshot-command-line "convert %f")
		;; org-attach-screenshot-command-line "magick import %f")
    (setq org-attach-screenshot-insertfunction
          (lambda (linkfilename)
            ;; (insert (concat "[[file:" linkfilename "]]\n\n")) ))
            (insert (concat "[[file:" linkfilename "]]")) ))
    ))

(defun dylayer/init-elfeed-dashboard()
  (use-package elfeed-dashboard
    :ensure t
    :config
    (setq elfeed-dashboard-file "~/.spacemacs.d/dylayer/elfeed/elfeed-dashboard.org")
    (setq flycheck-global-modes '(not . (elfeed-search-mode)))
    ;; update feed counts on elfeed-quit
    (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links)
  ))

(defun dylayer/init-newsticker()
  (use-package newsticker 
    :ensure t
    :config
    (require 'newsticker)
    ;; (setq newsticker-frontend 'newsticker-plainview) ;; default newsticker-treeview
    ;; not auto-fresh on backend
    (setq newsticker-retrieval-interval 0
          newsticker-ticker-interval 0)
    ;; remove default list (i.e. emacswiki)
    ;; (setq newsticker-url-list-defaults nil)
    (setq newsticker-automatically-mark-items-as-old nil)
    ))
;; init-xxx : xxx must be exit package/
;; ---------------------------------------------------------------------
(defun dylayer/init-ivy()
  (use-package ivy
    :demand
    :config
    (setq ivy-use-virtual-buffers t
          ivy-count-format "%d/%d "))

  (use-package swiper
    :after ivy
    :init
    :bind (("C-s" . swiper)
          ("C-r" . swiper-isearch-backward))
    :config (setq swiper-action-recenter t
                  swiper-include-line-number-in-search t))
  ;; swiper muliiple-cursor(Ctrl 7)

  (use-package counsel
    :after (ivy)
    :bind (
          ;; ("M-x" . counsel-M-x)            ;; helm functon
          ;; ("C-x C-f" . counsel-find-file)  ;; helm function
          ("C-c f" . counsel-recentf)
          ("C-c g" . counsel-git)))

  (use-package drag-stuff
    :bind (("<M-up>". drag-stuff-up)
           ("<M-down>" . drag-stuff-down)))
  )

(defun dylayer/post-init-evil()
  ;; clear hotkey in insert state map and use Emacs State
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  (evil-define-command dy/maybe-exit ()
    "Exit from insert to normal in org-mode."
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert "j")
      (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                             nil 0.5)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?j))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events
                                                (list evt))))))))
  )

(defun dylayer/init-disable-mouse()
  ;; show different bewteen evil-mode and emacs-mod
  ;; ---------- use-package
  ;; (use-package disable-mouse
  ;;   :init
  ;;   (global-disable-mouse-mode))

  ;; ---------- spacemacs (only) 
  (xterm-mouse-mode -1 )

  ;; ---------- re-define
  (define-minor-mode disable-mouse-mode
    "A minor-mode that disables all mouse keybinds."
    :global t
    :lighter " 🐭"
    :keymap (make-sparse-keymap))

  (dolist (type '(mouse down-mouse drag-mouse
                        double-mouse triple-mouse))
    (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
      ;; Yes, I actually HAD to go up to 7 here.
      (dotimes (n 7)
        (let ((k (format "%s%s-%s" prefix type n)))
          (define-key disable-mouse-mode-map
            (vector (intern k)) #'ignore)))))
  (disable-mouse-mode 1)
  )

(defun dylayer/init-yasnippet()
  (use-package yasnippet
    :config
    ;; (add-to-list 'yas-snippet-dirs "～/.spacemacs.d/snippets")
    (cond ((eq system-type 'darwin)
           (add-to-list 'yas-snippet-dirs "/Users/wenhua/.spacemacs.d/snippets")
           ))
    (cond ((eq system-type 'windows-nt)
           (add-to-list 'yas-snippet-dirs (concat EamcsConfDir ".spacemacs.d/snippets/"))
           ))
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    (yas-global-mode 1)
    ))

(defun dylayer/init-bongo()
  (use-package bongo
    :defer t
    :config
    (setq default-process-coding-system '(utf-8-unix . chinese-gbk-dos)) ;; recognize chinese
    (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "d:/Program Files/MPlayer for Windows/")
     (setq bongo-default-directory "e:/Recreation/Music/"))
   :custom
   (bongo-enabled-backends '(mplayer))
   ))

(defun dylayer/init-emms()
  (use-package emms
    :ensure t
    :defer t
    :config
    (progn
      (require 'emms-setup)  ;; Emms set
      (emms-standard)        ;; minimalistic, standard, all/devel
      )
    (setq default-process-coding-system '(utf-8-unix . chinese-gbk-dos)) ;; recognize chinese
    (setq emms-player-list '(emms-player-mplayer)
          emms-player-mplayer-command-name "d:/Program Files/MPlayer for Windows/mplayer.exe"
          emms-player-mplayer-parameters '("-slave")

          emms-show-format "♪ %s"
          emms-source-file-default-directory "e:/Recreation/Music/"
    )))

;; general --- set-key
(defun dylayer/init-general()
  (use-package general
    ;; https://github.com/noctuid/general.el
    :defer t
    :config
    ;; Global Keybindings(because ":keymaps 'global" is the default)
    ;; --------------------------
    (general-define-key
     ;; bind "C-c a" to 'org-agenda
     :prefix "C-c"
     "a" 'org-agenda
     )

    ;;  Mode Keybindings
    ;; --------------------------
    ;; `general-define-key' is comparable to `define-key' when :keymaps is specified
    (general-define-key
     ;; NOTE: keymaps specified with :keymaps must be quoted
     :keymaps 'org-mode-map
     "C-c C-q" 'counsel-org-tag
     ;;
     )))

(defun dylayer/init-super-save()
  (use-package super-save
    :ensure t
    :config
    (super-save-mode +1)
    (setq auto-save-default nil)
    ;; (setq super-save-auto-save-when-idle t)
    ;; (setq super-save-idle-duration 5) ; interval
    ))

;; python-autocomplete
;; ---------------------------------------------------------------------
(defun dylayer/post-init-python()
  ;; (use-package company-jedi
  ;;   :if (configuration-layer/package-usedp 'company)
  ;;   :defer t
  ;;   :init
  ;;   (push 'company-jedi company-backends-python-mode))

  (use-package py-autopep8
    :config
    (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
    ;; (setq py-autopep8-options '("--max-line-length=100"))

    ;; example set-leader-keys in major-mode
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "cc" 'spacemacs/python-execute-file))
    ))
(defun dylayer/init-company-jedi()
  (use-package company-jedi
    :config
    (defun dy/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'dy/python-mode-hook)
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-transformers '(company-sort-by-occurrence)
          company-tooltip-align-annotations t
          company-selection-wrap-around t
          company-show-numbers t
          company-idle-delay 0.2)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
    ))

;; markdown-preview
(defun dylayer/init-grip-mode()
  (use-package grip-mode
    :config
    (defun markdown-to-html ()
      "markdown-previe realtime"
      (interactive)
      (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
      (browse-url (format "http://localhost:5000/%s.%s"
                          (file-name-base) (file-name-extension (buffer-file-name)))))
    (spacemacs/set-leader-keys "omh" 'markdown-to-html)
    (spacemacs/set-leader-keys "oml" 'markdown-live-preview-mode)
    ))

;; not work
(defun dylayer/init-flymd()
  (use-package flymd
    :init))

(defun dylayer/post-init-multiple-cursors()
  (use-package multiple-cursors
    :init)
  )

(defun dylayer/init-rime()
  (use-package rime
    :custom
    (default-input-method "rime")
    (cond ((eq system-type 'windows-nt)
           (rime-librime-root "D:/Scoop/apps/librime/current")))
    ))

(defun dylayer/init-emojify()
  (use-package emojify
    :config
    (when (member "Segoe UI Emoji" (font-family-list))
      (set-fontset-font
       t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
    (setq emojify-display-style 'unicode)
    (setq emojify-emoji-styles '(unicode))
    (bind-key* (kbd "C-c e") #'emojify-insert-emoji) ; override binding in any mode
    ))


(defun dylayer/init-popweb()
  "Performace better on linux platform. org-roam, latex show, translate"
  (use-package org-roam
    :ensure t
    :init)
  ;; (setenv "HOME" "D:/Program Files (x86)/Emacs/spacemacs-develop/")
  (add-to-list 'load-path "~/.spacemacs.d/dylayer/popweb/")
  (add-to-list 'load-path "~/.spacemacs.d/dylayer/popweb/extension/org-roam/")
  (add-to-list 'load-path "~/.spacemacs.d/dylayer/popweb/extension/latex/")
  (add-to-list 'load-path "~/.spacemacs.d/dylayer/popweb/extension/dict/")
  (require 'popweb-org-roam-link)
  (require 'popweb-latex)
  (add-hook 'latex-mode-hook #'popweb-latex-mode)
  (require 'popweb-dict) 
  ;; (require 'popweb-dict-bing) ; Translation using Bing
  ;; (require 'popweb-dict-youdao) ; Translation using Youdao
  )
