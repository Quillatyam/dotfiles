(require 'org)
(require 'org-contacts)
(require 'org-bullets)
(require 'ox-bibtex)
(require 'ox-extra)
(require 'bibtex)
(require 'auth-source-pass)
(require 'password-store)
(require 'org-gcal)

(provide 'org-config)

;;; Bindings and Hooks
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-toggle-blocks)

(spacemacs/set-leader-keys "aof" 'org-open-at-point-global)

(define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)

(evil-define-key '(normal visual motion) org-mode-map
  "gh" 'outline-up-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  "gl" 'outline-next-visible-heading
  "gu" 'outline-previous-visible-heading)

;; Quick refile of project tasks
(setq org-refile-targets '((nil :regexp . "Week of")))

(spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'org-refile)

;;; Theming

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

;; Colors of types
(setq org-todo-keyword-faces
      '(("TODO" . "SpringGreen2")
        ("WAITING" . "goldenrod1")
        ("DONE" . "gray40")
        ("CANCELLED"  . "gray30")
        ))

;; Colors of priorities
(setq org-priority-faces
      '((?A . (:foreground "red1" :weight 'bold))
        (?B . (:foreground "VioletRed1"))
        (?C . (:foreground "orange"))
        (?D . (:foreground "goldenrod1"))
        (?E . (:foreground "gray50"))))


;;(setq org-ellipsis "...")
;;(setq org-bullets-bullet-list '("" "" "" ""))

;;; Templates

(setq
 org-structure-template-alist
 '(("n" "#+NAME: ?")
   ("q" "#+BEGIN_QUOTE\n\n#+END_QUOTE")

   ;; Language Blocks
   ("c" "#+BEGIN_SRC clojure\n\n#+END_SRC")
   ("d" "#+BEGIN_SRC dot\n\n#+END_SRC")
   ("e" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
   ("h" "#+BEGIN_SRC haskell\n\n#+END_SRC")
   ("l" "#+BEGIN_SRC lisp\n\n#+END_SRC")
   ("p" "#+BEGIN_SRC python\n\n#+END_SRC")

   ;; Collapse previous header by default in themed html export
   ("clps" ":PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")
   ;; Hugo title template
   ("b" "#+TITLE: \n#+SLUG: \n#+DATE: 2017-mm-dd
#+CATEGORIES: \n#+SUMMARY: \n#+DRAFT: false")))

;;; Org Blocks

;; Hide all org-blocks, including src, quote, etc. blocks, on buffer load
(defvar org-blocks-hidden nil)
(defun org-toggle-blocks ()
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))

;;; Settings

(ox-extras-activate '(ignore-headlines))

(setq org-directory '("~/org"))
(setq org-contacts-files '("~/org/contacts.org"))
(setq org-agenda-files '("~/org/inbox.org"
                         "~/org/trello"
                         "~/org/calendar"
                         "~/org/projects.org"))
;;(setq org-archive-location '("~/org/archive.org"))

;; Meta return behavior as it should be
(org-defkey org-mode-map [(meta return)] 'org-meta-return)

;; Start on Saturday
(setq org-agenda-start-on-weekday 6)

;; Calendar goes 7 days
(setq org-agenda-span 7)

(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-complete-tags-always-offer-all-agenda-tags t)
;;(setq org-columns-default-format
;;      "%14DEADLINE %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")

;; Refile max level
(setq org-refile-targets '((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 2)))

;; Refile in a single go
(setq org-outline-path-complete-in-steps nil)

;; Refile show full paths
(setq org-refile-use-outline-path 'file)

;; Smartparens
;;(sp-with-modes '(org-mode)
;;  (sp-local-pair "*" "*"))

;; Capture templates
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "~/org/inbox.org" "Inbox")
         "* TODO %?")
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
	       "* %?\n%u" :prepend t)
        ("a" "Appointment" entry (file  "~/org/calendar/tacitic.org")
	       "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("w" "Review: Weekly Review" entry (file+datetree "~/org/reviews.org")
         (file "~/org/templates/weeklyreviewtemplate.org"))))

;; Search through archives
(setq org-agenda-text-search-extra-files '(agenda-archives))

;; Blank line before new item
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))

;; Enforce child items to be completed before a parent can be completed
(setq org-enforce-todo-dependencies t)

;; Log state changes
(setq org-log-done (quote time))
(setq org-log-reschedule (quote time))

(auth-source-pass-enable)

;; Gcal
(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
(eval-after-load 'password-store
  '(progn (setq org-gcal-client-id (password-store-get "tacitic/google/calendar-client-id")
                org-gcal-client-secret (password-store-get "tacitic/google/calendar-client-secret")
                org-gcal-file-alist '(("robert@tacitic.com" .  "~/org/calendar/gcal.org")))))

;; Adding yet further auditing, this option causes Org to insert annotations
;; when you change the deadline of a task, which will note the previous
;; deadline date and when it was changed.
(setq org-log-redeadline (quote time))

;; Custom Day view skip prio function
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
  PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

;; Custom Day view skip habbits
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

;; Custom Day View
(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
        ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (todo "BUG|TODO|IN-PROGRESS|WAITING"
                  ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                  (air-org-skip-subtree-if-priority ?A)
                                                  (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
        ((org-agenda-block-separator (make-string 999 ?= ))))))

(when linux?
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                        ("\\.pdf\\'" . default))))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-minted-options '(("frame" "lines")
                                 ("fontsize" "\\scriptsize")
                                 ("xleftmargin" "\\parindent")
                                 ("linenos" "")))


;; (setq
;;  org-latex-pdf-process
;;  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;(setq reftex-default-bibliography '("~/dev/pop-synth/docs/paper/references.bib"))
;;(setq org-ref-default-bibliography '("~/dev/pop-synth/docs/paper/references.bib"))

(setq org-latex-pdf-process
      '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

;;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages '((python .  t)
                             (haskell . t)
                             (clojure . t)
                             (dot .     t)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-src-window-setup 'current-window)
(setq org-babel-default-header-args:python
      (cons '(:results . "output file replace")
            (assq-delete-all :results org-babel-default-header-args)))
