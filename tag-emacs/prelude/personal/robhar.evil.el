;;; robhar.evil --- Evil configuration
;;; Commentary:
;;; Code:

(prelude-require-package 'evil)

(setq evil-mode-line-format 'before)
(setq evil-want-C-u-scroll t)

(setq evil-emacs-state-cursor  '("OrangeRed" box))
(setq evil-normal-state-cursor '("DeepSkyBlue" box))
(setq evil-visual-state-cursor '("purple1" box))
(setq evil-insert-state-cursor '("SpringGreen2" bar))
(setq evil-motion-state-cursor '("gold1" box))

(require 'evil)
(evil-mode 1)

;;
;; Evil Surround
;;
(prelude-require-package 'evil-surround)
(require 'evil-surround)
(global-evil-surround-mode 1)

;;
;; Evil search visual selection with *
;;
(prelude-require-package 'evil-visualstar)
(require 'evil-visualstar)

;;
;; Evil Numbers
;;
(prelude-require-package 'evil-numbers)
(require 'evil-numbers)

(define-key evil-normal-state-map (kbd "C-A")
  'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-A")
  'evil-numbers/dec-at-pt)

;;
;; Evil commenting
;;
(require 'evil-commentary)
(evil-commentary-mode)

;;
;; Other useful Commands
;;
(evil-ex-define-cmd "W"     'evil-write-all)
(evil-ex-define-cmd "Tree"  'treemacs)
(evil-ex-define-cmd "linum" 'linum-mode)
(evil-ex-define-cmd "Align" 'align-regexp)


;; Scrolling
(defun evil-scroll-down-other-window ()
  (interactive)
  (scroll-other-window))

(defun evil-scroll-up-other-window ()
  (interactive)
  (scroll-other-window '-))

(define-key evil-normal-state-map
  (kbd "C-S-d") 'evil-scroll-down-other-window)

(define-key evil-normal-state-map
  (kbd "C-S-u") 'evil-scroll-up-other-window)

;;
;; Ace jump mode integration
;;
(define-key evil-motion-state-map "gl" 'evil-ace-jump-line-mode)
;;(define-key evil-motion-state-map "gc" 'evil-ace-jump-char-mode)
(define-key evil-motion-state-map "go" 'evil-ace-jump-word-mode)

;;
;; Magit from avsej
;;
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)
