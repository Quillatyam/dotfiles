;;; init -- personal init file
;;; Commentary:
;;; Code:

;;; Custom theme dir
(add-to-list 'custom-theme-load-path (expand-file-name "preload/themes"
                                                       user-emacs-directory))

;; Disable flyspell.
(setq prelude-flyspell nil)

;; Disable guru mode, I'm already a guru ;)
(setq prelude-guru nil)

;; No error bell
(setq ring-bell-function 'ignore)

;; A common frustration with new Emacs users is the filename# files created.
;; This centralises the backup files created as you edit.
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq whitespace-mode -1)

;; I don't use tabs
(setq-default indent-tabs-mode nil)

;; Beginning of line
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)))

;; Better undo
(use-package undo-tree
  :defer 5
  :diminish global-undo-tree-mode
  :config
  (global-undo-tree-mode 1))

;; ace select window
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package buffer-flip
  :ensure t
  :bind  (("M-<tab>" . buffer-flip)
          :map buffer-flip-map
          ( "M-<tab>" .   buffer-flip-forward)
          ( "M-S-<tab>" . buffer-flip-backward)
          ( "M-ESC" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))

(provide 'init)
;;; init ends here
