;;; company.el -- my company-mode config
;;; Commentary:
;;;     Modify company so that tab and S-tab cycle through completions without
;;;     needing to hit enter.
;;; Code:

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))

(setq company-require-match 'never)

(setq company-auto-complete t)

(defun my-company-visible-and-explicit-action-p ()
  (and (company-tooltip-visible-p)
       (company-explicit-action-p)))

(defun company-ac-setup ()
  "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
  (setq company-require-match nil)
  (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  (setq company-frontends '(company-echo-metadata-frontend
                            company-pseudo-tooltip-unless-just-one-frontend-with-delay
                            company-preview-frontend))
  (define-key company-active-map [tab]
    'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "TAB")
    'company-select-next-if-tooltip-visible-or-complete-selection))

(company-ac-setup)
(setq company-tooltip-limit 20)
(setq company-idle-delay .3)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))
(global-company-mode t)


;; Posframe load into a childframe 26.1+
(require 'company-posframe)
(company-posframe-mode 1)

;; (custom-set-faces
;;  '(company-preview ((t (:background nil :underline t ))))
;;  '(company-preview-common ((t (:inherit company-preview))))
;;  '(company-tooltip ((t (:background "lightgray" :foreground "gray22"))))
;;  '(company-tooltip-selection ((t (:background "DeepSkyBlue" :foreground "white"))))
;;  '(company-tooltip-common ((t (:foreground "green" :background: "lightgray"))))
;;  '(company-tooltip-common-selection ((t (:background: "lightgray")))))

(provide 'company)
;;; company.el ends here
