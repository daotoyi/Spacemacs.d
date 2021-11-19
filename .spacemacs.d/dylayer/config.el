;; basic setting.
;; --------------------------------------------------------------------------------------------

;; default directory.
;; (when (eq system-type 'windows-nt)
;;   (setq default-directory "e:/Refine/"))
;; (when (eq system-type 'gnu/linux)
;;   (setq default-directory "/mnt/e/Refine/"))

(setq-default frame-title-format '("%f"))
(setq frame-title-format "Spacemacs(daotoyi)@%b")

(defalias 'yes-or-no-p 'y-or-n-p)

;;  "Replace DOS eolns CR LF with Unix eolns CR"
(defun remove-dos-eol ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; all backups goto ~/.backups instead in the current directory
(setq backup-directory-alist (quote (("." . "e:/TMP/TmpFiles"))))

;; dired, show file(.org) with yellow in buffer.
(add-hook 'dired-mode-hook
          (lambda ()
            (highlight-lines-matching-regexp "\.org$" 'hi-yellow)))
(setq initial-scratch-message
      "\n;; Configuration by Daoyi <gitee.com/daotoyi/spacemacs.d>. \n;; Enjoy!\n\n")

;; keybingings
;; --------------------------------------------------------------------------------------------
;; adjust text-scale
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)
(global-set-key (kbd "M-s o") 'occur-dwim)
;; (global-set-key (kbd "C-c f") 'counsel-recentf) ; spacemacs/counsel-recentf
;; (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)

;; change  move-method in  occur-buffer to (HJKL)
;; set in  dotspacemacs/user-config in ~/.spacemacs
;; (evilified-state-evilify-map occur-mode-map
;;   :mode occur-mode)
