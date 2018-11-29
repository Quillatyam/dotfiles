;;; ui -- User interface
;;; Commentary:
;;; Code:

;; Defaulft font
;;(set-default-font "Operator Mono Lig 14")
(set-face-attribute 'default nil
                    :family "Operator Mono Lig"
                    :height 125
                    :weight 'semi-bold
                    :width 'normal)
(setq-default line-spacing 0.12)

;; Golden ratio mode
;;(golden-ratio-mode 1)

;; Linum
;;(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))

;;(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; or for treemacs users
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Pretty mode (support for ligatures)
(require 'pretty-mode)
(global-pretty-mode t)
(global-prettify-symbols-mode 1)
(pretty-fonts-set-kwds
 '((pretty-fonts-fira-font prog-mode-hook org-mode-hook)))

(let ((height (face-attribute 'default :height)))
  ;; for all linum/nlinum users
  (set-face-attribute 'linum nil :height height)
  ;; only for `linum-relative' users:
  (set-face-attribute 'linum-relative-current-face nil :height height)
  ;; only for `nlinum-relative' users:
  (set-face-attribute 'nlinum-relative-current-face nil :height height))

;; PANDA POWER
;; (use-package panda-theme
;;  :ensure t
;;  :config
;;  (load-theme 'my-panda t))

;; Smart mode line
(setq sml/theme 'atom-one-dark)
(sml/setup)

;; Disable scrollbars
(scroll-bar-mode -1)

;; FCI mode
(add-hook 'prog-mode-hook 'fci-mode)
(setq-default fill-column 80)

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-git-command-pipe           ""
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-max-git-entries            5000
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-cursor                nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   1
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      40)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(provide 'ui)
;;; ui.el ends here
