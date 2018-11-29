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

;; Toggle window splits
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; Clean-up the mode-line
(delight '((abbrev-mode " Abv" "abbrev")
           (company-mode "C")
           (smart-tab-mode " \\t" "smart-tab")
           (eldoc-mode nil "eldoc")
           (rainbow-mode)
           (overwrite-mode " Ov" t)
           (emacs-lisp-mode "Elisp" :major)))

;; php-mode
(add-hook 'php-mode-hook
          (lambda ()
            (setq flycheck-phpcs-standard "PSR2")
            (setq php-mode-coding-style (quote psr2))))

(provide 'init)
;;; init ends here
