;; keybindings.el for dylayer
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
  "arn" 'newsticker-treeview
  "arq" 'newsticker-treeview-quit
  "amb" 'bongo
  "amd" 'emms-play-directory
  "ah" 'easy-hugo
  "av" 'org-capture

  "oc" 'calendar
  "oi" 'imenu-list
  ;; "oy" 'youdao-dictionary-search-at-point+
  "oy" 'fanyi-dwim2

  "obb" 'bongo
  "obg" 'bongo-start
  "obs" 'bongo-stop
  "obp" 'bongo-previous
  "obn" 'bongo-next
  "obf" 'nbongo-show
  "ob>" 'bongo-seek-forward
  "ob<" 'bongo-seek-backward
  "obr" 'bongo-play-random
  "obR" 'bongo-repeating-playback-mode
  "obP" 'bongo-pause/resume ; bongo mode default <SPC>

  "oEe" 'emms
  "oEg" 'emms-start
  "oEs" 'emms-stop
  "oEd" 'emms-play-directory
  "oEP" 'emms-pause
  "oEp" 'emms-previous
  "oEn" 'emms-next
  "oEf" 'emms-show
  ;; emms volume seek
  "oE+" 'emms-volume-raise
  "oE-" 'emms-volume-lower
  "oE>" 'emms-seek-forward
  "oE<" 'emms-seek-backward
  ;; emms sort
  "oESL" 'emms-playlist-sort-by-list
  "oESN" 'emms-playlist-sort-by-name
  "oESY" 'emms-playlist-sort-by-info-year
  "oESt" 'emms-playlist-sort-by-info-title
  "oESe" 'emms-playlist-sort-by-file-extension

  "ofr" 'rename-file

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

  "omb" 'org-beamer-mode
  "omg" 'ggtags-mode
  "omi" 'iimage-mode
  "omj" 'javascript-mode
  "oml" 'lisp-interaction-mode
  "omm" 'markdown-mode
  "omc" 'occur-mode
  "omo" 'org-mode
  "omp" 'pangu-spacing-mode
  "oms" 'shell-script-mode
  "omS" 'super-save-mode
  "omt" 'conf-toml-mode

  ;; set in dylayer/packages.el
  ;; "omh" 'markdown-to-html
  ;; "oml" 'markdown-live-preview-mode

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

  "ord" 'elfeed-dashboard
  "ore" 'elfeed
  "orn" 'newsticker-treeview
  "orq" 'newsticker-treeview-quit

  "oty" 'fanyi-dwim2
  "ott" 'fanyi-dwim
  "oth" 'fanyi-from-history
  "otc" 'fanyi-copy-query-word
  "otu" 'dy/remove-dos-eol
  ; "ou" 'dy/remove-dos-eol

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
