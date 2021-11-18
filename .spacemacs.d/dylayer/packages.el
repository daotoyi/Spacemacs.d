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

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; ==================================================================================================
;; (add-to-list 'load-path "D:/Program Files (x86)/Emacs/spacemacs-develop/.spacemacs.d/dylayer/local/")
(when (eq system-type 'windows-nt)
  (setenv "HOME" "D:/Program Files (x86)/Emacs/spacemacs-develop/"))
(add-to-list 'load-path "~/.spacemacs.d/dylayer/local/")
;; (load-file "~/spacemacs.d/dylayer/packages.el")

;; add package
(defconst dylayer-packages
  '(
    ;; location: from build-in. e.g.:
    ;; (occur-mode :location built-in)

    ;; location: from github
    ;; (gulpjs :location (recipe :fetcher github :repo "zilongshanren/emacs-gulpjs"))

    ;; (nose :location (recipe :fetcher github :repo "syl20bnr/nose.el"))

    youdao-dictionary
    org
    ivy
    evil
    )
  )

;; init package
(defun dylayer/post-init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :init
    ;; (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
    (global-set-key (kbd "C-q") 'youdao-dictionary-search-at-point+)
    ))

;; init occur mode (from built-in)
(defun dylayer/init-occur-mode ()
  (evilified-state-evilify-map occur-mode-map
    :mode occur-mmode))

;; init (from github)
(defun dylayer/init-gulpjs ()
  (use-package gulpjs
    :init))

;; post-init org-agenda
(defun dylayer/init-org()
  (require 'init-org)
  (require 'init-roll)
  )

;; init-xxx : xxx must be exit package/
(defun dylayer/init-ivy()
  (use-package drag-stuff
    :bind (("<M-up>". drag-stuff-up)
           ("<M-down>" . drag-stuff-down)))
  (use-package swiper
    :after ivy
    :bind (("C-s" . swiper)
          ("C-r" . swiper-isearch-backward))
    :config (setq swiper-action-recenter t
                  swiper-include-line-number-in-search t))
  (use-package counsel
    :after (ivy)
    :bind (
          ;; ("M-x" . counsel-M-x)            ;; helm functon
          ;; ("C-x C-f" . counsel-find-file)  ;; helm function
          ("C-c f" . counsel-recentf)
          ("C-c g" . counsel-git)))
  )

;; post-evil
(defun dylayer/post-init-evil()
  ;; clear hotkey in insert state map and use Emacs State
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  )

;; support for python
(defun dylayer/init-nose()
  (use-package nose.el
    :init)
  )
