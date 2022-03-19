;;; keybindings.el for dylayer
;; --------------------------------------------------------------------------------------------
;; keybingings global-set-key
;; adjust text-scale
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)
(global-set-key (kbd "M-s o") 'occur-dwim)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; spacevim
;; (global-set-key [F2] 'imenu-list)
;; (global-set-key [F3] 'treemacs)

;; timestamp
(global-set-key (kbd "C-c t b") 'dy/insert-blog-time-timestamp)
(global-set-key (kbd "C-c t t") 'dy/insert-current-time-timestamp)
(global-set-key (kbd "C-c t d") 'dy/insert-current-date-timestamp)

; adjust windows width
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)

;; multimedia
(global-set-key (kbd "C-c m b") 'bongo)
(global-set-key (kbd "C-c m e") 'emms)
;; org-----------------------------------------------------------------------------------------
;; mobile-sync
(global-set-key "\C-cmp" 'org-mobile-push)
(global-set-key "\C-cmg" 'org-mobile-pull)

(global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-cb" 'org-iswitchb)   ;; switch between org-mode buffers.(obsolete)

;; other
(global-set-key (kbd "C-c C-j") 'dired-up-directory)

;; --------------------------------------------------------------------------------------------
(general-define-key
 :prefix "C-c"
 "a" 'org-agenda
 "b" 'org-iswitchb
 "c" 'org-capture ;; not work in emacs-mode

 :keymaps 'org-mode-map
 ;; org-mode ;; setup links anywhere in emacs. && "\C-c \C-l" invoke
 "l" 'org-store-link
 "L" 'org-insert-link-global
 "o" 'org-open-at-point-global
 )

;; evil
;; evil-define-command dy/maybe-exit () in dy/packages#evil 
(define-key evil-insert-state-map "j" #'dy/maybe-exit) 

;; --------------------------------------------------------------------------------------------
;; spacemacs/set-leader-keys:
(spacemacs/set-leader-keys
  "ard" 'elfeed-dashboard
  "amb" 'bongo
  "ame" 'emms
  "amp" 'emms-play-directory
  "ah" 'easy-hugo
  "av" 'org-capture

  "oc" 'calendar
  "oi" 'imenu-list
  "or" 'dy/remove-dos-eol
  "oy" 'youdao-dictionary-search-at-point+

  "odd" 'dy/goto-Ddisk-dir
  "ode" 'dy/goto-Edisk-dir
  "odb" 'dy/goto-blog-dir
  "odo" 'dy/goto-notes-dir
  "odr" 'dy/goto-refine-dir
  "odg" 'find-grep-dired
  "odn" 'find-name-dired

  "oem" 'evil-mc-make-all-cursors
  "oeq" 'evil-mc-undo-all-cursors
  "oen" 'evil-mc-skip-and-goto-next-match
  "oep" 'evil-mc-skip-and-goto-prev-match
  "oeN" 'evil-mc-skip-and-goto-next-cursor
  "oeP" 'evil-mc-skip-and-goto-prev-cursor

  "oho" 'helm-occur

  "omi" 'iimage-mode
  "omj" 'javascript-mode
  "oml" 'lisp-interaction-mode
  "omm" 'markdown-mode
  "omc" 'occur-mode
  "omo" 'org-mode
  "oms" 'shell-script-mode
  "omt" 'conf-toml-mode

  "osg" 'counsel-git-grep
  "osr" 'dy/recursive-grep

  "ooe" 'org-edit-src-block
  "ooi" 'org-insert-structure-template
  "ooI" 'dy/org-insert-src-block
  "ost" 'yas/expand-from-trigger-key
  "oog" 'org-mobile-pull
  "oop" 'org-mobile-push
  "ooo" 'occur
  "ooO" 'org-occur
  "oom" 'multi-occur  ;; ctrl-enter
  ;; "oog" 'get-buffers-matching-mode
  ;; "oos" 'multi-occur-in-this-mode

  ;; set in dylayer/packages.el
  ;; "omh" 'markdown-to-html
  ;; "oml" 'markdown-live-preview-mode
  )

;; --------------------------------------------------------------------------------------------
;; spacemacs/set-leader-keys-for-major-mode ;; `SPC->m->c->c/C'  must load after major-mode.

;; (spacemacs/set-leader-keys-for-major-mode 'python-mode
;;   "cc" 'spacemacs/python-execute-file)

;; --------------------------------------------------------------------------------------------
;; keybingings local-set-key && define-key
(define-key c-mode-map (kbd "F9") 'compile)

;; change  move-method in occur-buffer to (HJKL),set in dotspacemacs/user-config()~/.spacemacs)
;; (evilified-state-evilify-map occur-mode-map
;;   :mode occur-mode)
