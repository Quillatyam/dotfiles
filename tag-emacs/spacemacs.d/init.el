;;; Setup

;; -- Robert den Harink's Spacemacs Configuration --
;; -- MIT License --
;;
;; `init.el' configures spacemacs, defining required `dotspacemacs/...' functions.

(defvar ROBERT-ONLY? t
  "If cloning, set to nil, enable non-layer personal configuration.")

(defvar linux? (eq system-type 'gnu/linux)
  "Are we on a gnu/linux machine?")

(defvar desktop? (= 1440 (display-pixel-height))
  "Am I on my desktop? For determining font size.")

(defun os-path (path)
  "Prepend drive label to PATH if on windows machine."
  (if linux?
      path
    (expand-file-name path "c:")))

;;; Spacemacs/

(defun dotspacemacs/init ()
  "Instantiate Spacemacs core settings."
  (setq dotspacemacs-mode-line-theme '(all-the-icons :separator 'slant))
  (dotspacemacs/init/coding)
  (dotspacemacs/init/display)
  (dotspacemacs/init/evil)
  (dotspacemacs/init/keys)
  (dotspacemacs/init/layouts)
  (dotspacemacs/init/misc)
  (dotspacemacs/init/packages)
  (dotspacemacs/init/startup))

(defun dotspacemacs/layers ()
  "Instantiate Spacemacs layers declarations and package configurations."
  (dotspacemacs/layers/config)
  (dotspacemacs/layers/packages))

(defun dotspacemacs/user-init ()
  "Package independent settings to run before `dotspacemacs/user-config'."
  (setq custom-file
        "./elisp/.custom-settings.el"))

(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegated to layers."
  ;; Doom theme settings
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config)

  ;; Olivetti
  (setq-default olivetti-body-width 80)
  (spacemacs/set-leader-keys "wc" 'olivetti-mode)

  ;; Always use existing emacs window.
  (setq s-pop-up-frames nil)

  ;; Shell popup
  (setq shell-default-full-span nil)

  ;; Dark titlebar on osx
  (when (memq window-system '(mac ns))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

  (dotspacemacs/user-config/toggles)
  (dotspacemacs/user-config/experiments))

;;; Spacemacs/Layers
;;;; Local

(defvar dotspacemacs/layers/local
  '((macros :location local)    ; All local layers inherit these macros

    (config :location local)    ; Org, Avy, Evil, Misc... config
    (display :location local)   ; Pretty-eshell/code/outlines... pkgs
    (langs :location local)     ; Language config
    (personal :location local)  ; Personal pkgs
    )
  "Local layers housed in `~/.spacemacs.d/layers'.")

;;;; Core

(defvar dotspacemacs/layers/core
  '(better-defaults
    git
    syntax-checking

    (auto-completion :variables
                     auto-completion-return-key-behavior 'complete
                     auto-completion-tab-key-behavior 'complete
                     auto-completion-enable-snippets-in-popup t)
    (ivy :variables
         ivy-extra-directories nil)
    (org :variables
         org-want-todo-bindings t)
    (shell :variables
           shell-default-shell 'eshell)
    (version-control :variables
                     version-control-global-margin t
                     version-control-diff-tool 'git-gutter+)
    )
  "Layers I consider core to Spacemacs.")

;;;; Langs

(defvar dotspacemacs/layers/langs
  '(c-c++
    emacs-lisp
    javascript
    rust
    php

    csv
    html
    markdown
    yaml

    (clojure :variables
             clojure-enable-fancify-symbols t)
    (haskell :variables
             haskell-completion-backend 'intero)
    (python :variables
            python-sort-imports-on-save t
            python-test-runner 'pytest
            :packages
            (not hy-mode)  ; I maintain `hy-mode', using local branch
            (not importmagic)
            )
    )
  "Programming and markup language layers.")

;;;; Extra

(defvar dotspacemacs/layers/extra
  '(gnus
    graphviz
    pdf-tools
    ranger
    treemacs
    osx

    (ibuffer :variables
             ibuffer-group-buffers-by 'projects)
    )
  "Miscellaneous layers.")

;;;; Layers/config

(defun dotspacemacs/layers/config ()
  (setq-default

   dotspacemacs-distribution
   'spacemacs

   dotspacemacs-enable-lazy-installation
   'unused

   dotspacemacs-ask-for-lazy-installation
   t

   dotspacemacs-configuration-layer-path
   (list (os-path "~/.spacemacs.d/layers/"))

   dotspacemacs-configuration-layers
   (
    append
    dotspacemacs/layers/local
    dotspacemacs/layers/core
    dotspacemacs/layers/langs
    dotspacemacs/layers/extra
    )
   ))

;;;; Layers/packages

(defun dotspacemacs/layers/packages ()
  (setq-default
   dotspacemacs-additional-packages
   '(
     doom-themes
     olivetti
     faceup
     )

   dotspacemacs-excluded-packages
   '(
     fringe
     hy-mode
     importmagic
     )

   dotspacemacs-frozen-packages
   '()

   dotspacemacs-install-packages
   'used-but-keep-unused
   ))

;;; Spacemacs/Init
;;;; Coding

(defun dotspacemacs/init/coding ()
  (setq-default
   dotspacemacs-search-tools
   '("ag" "rg" "pt" "ack" "grep")

   dotspacemacs-smooth-scrolling
   t

   dotspacemacs-folding-method
   'evil

   dotspacemacs-smartparens-strict-mode
   nil

   dotspacemacs-smart-closing-parenthesis
   nil

   dotspacemacs-highlight-delimiters
   'all

   dotspacemacs-line-numbers
   'relative

   dotspacemacs-whitespace-cleanup
   'trailing
   ))

;;;; Display

(defun dotspacemacs/init/display ()
  (setq-default

   dotspacemacs-themes
   '(
     doom-one
     doom-one-light
     )

   dotspacemacs-default-font
   `(
     "Fira Code"
     :size ,(cond ((not linux?) 13)
                  (desktop? 16)
                  (t 34))
     :powerline-scale 1.5
     )

   dotspacemacs-fullscreen-at-startup
   (if linux? nil nil)

   dotspacemacs-fullscreen-use-non-native
   nil

   dotspacemacs-maximized-at-startup
   nil

   dotspacemacs-active-transparency
   90

   dotspacemacs-inactive-transparency
   90

   dotspacemacs-mode-line-unicode-symbols
   t

   dotspacemacs-zone-out-when-idle
   nil

   dotspacemacs-frame-title-format
   "%I@%S"

   dotspacemacs-icon-title-format
   nil

   dotspacemacs-pretty-docs
   t
   ))

;;;; Evil

(defun dotspacemacs/init/evil ()
  (setq-default

   dotspacemacs-editing-style
   'vim

   dotspacemacs-colorize-cursor-according-to-state
   t

   dotspacemacs-remap-Y-to-y$
   t

   dotspacemacs-retain-visual-state-on-shift
   t

   dotspacemacs-visual-line-move-text
   nil

   dotspacemacs-ex-substitute-global
   nil

   dotspacemacs-enable-paste-transient-state
   nil

   dotspacemacs-show-transient-state-title
   t

   dotspacemacs-show-transient-state-color-guide
   t
   ))

;;;; Keys

(defun dotspacemacs/init/keys ()
  (setq-default

   dotspacemacs-leader-key
   "SPC"

   dotspacemacs-emacs-command-key
   "SPC"

   dotspacemacs-ex-command-key
   ":"

   dotspacemacs-emacs-leader-key
   "M-m"

   dotspacemacs-major-mode-leader-key
   ","

   dotspacemacs-major-mode-emacs-leader-key
   "C-M-m"

   dotspacemacs-which-key-delay
   0.4

   dotspacemacs-which-key-position
   'bottom

   dotspacemacs-distinguish-gui-tab
   nil
   ))

;;;; Layouts

(defun dotspacemacs/init/layouts ()
  (setq-default

   dotspacemacs-scratch-mode
   'org-mode

   dotspacemacs-default-layout-name
   "Default"

   dotspacemacs-display-default-layout
   nil

   dotspacemacs-auto-resume-layouts
   nil

   dotspacemacs-auto-generate-layout-names
   t

   dotspacemacs-switch-to-buffer-prefers-purpose
   nil
   ))

;;;; Misc

(defun dotspacemacs/init/misc ()
  (setq-default

   dotspacemacs-large-file-size
   5

   dotspacemacs-auto-save-file-location
   'cache

   dotspacemacs-max-rollback-slots
   5

   dotspacemacs-persistent-server
   nil

   dotspacemacs-helm-resize
   nil

   dotspacemacs-helm-no-header
   nil

   dotspacemacs-helm-position
   'bottom
   ))

;;;; Packages

(defun dotspacemacs/init/packages ()
  (setq-default

   dotspacemacs-default-package-repository
   nil

   dotspacemacs-elpa-https
   t

   dotspacemacs-elpa-timeout
   5

   dotspacemacs-check-for-update
   nil

   dotspacemacs-elpa-subdirectory
   nil
   ))

;;;; Startup

(defun dotspacemacs/init/startup ()
  (setq-default

   dotspacemacs-verbose-loading
   nil

   dotspacemacs-startup-banner
   'official

   dotspacemacs-startup-lists
   '()

   dotspacemacs-startup-buffer-responsive
   t

   dotspacemacs-loading-progress-bar
   t
   ))

;;; Spacemacs/User-Config
;;;; Toggles

(defun dotspacemacs/user-config/toggles ()
  "Spacemacs toggles not intended to be put into layers."
  (spacemacs/toggle-highlight-long-lines-globally-on)
  (spacemacs/toggle-mode-line-minor-modes-off)
  (spacemacs/toggle-aggressive-indent-globally-on)
  (global-highlight-parentheses-mode 1)
  (rainbow-delimiters-mode-enable)
  (fringe-mode '(0 . 8)))

;;;; Experiments

(defun dotspacemacs/user-config/experiments ()
  "Space for trying out configuration updates."
  (setq nord-comment-brightness 15)
  (setq nord-uniform-mode-lines t)

  (when ROBERT-ONLY?
    ;;(load-file (os-path "~/dev/hy-mode/hy-mode.el"))
    ;;(load-file (os-path "~/dev/hy-mode/spacemacs-hy.el"))
    ;;(load-file (os-path "~/dev/hy-mode/hy-personal.el"))
    ;;(require 'hy-mode)
    ;;(require 'spacemacs-hy)
    ;;(require 'hy-personal)
    ))