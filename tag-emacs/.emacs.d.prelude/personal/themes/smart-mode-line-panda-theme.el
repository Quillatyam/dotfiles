;;; smart-mode-line-panda-theme.el --- Atom-one-dark theme for smart-mode-line

(deftheme smart-mode-line-panda
  "Panda theme for smart-mode-line.")

(custom-theme-set-faces
 'smart-mode-line-panda
 '(mode-line-inactive ((t :background "#222223" :box (:line-width 3 :color "#222223"))))
 '(mode-line     ((t :background "#1b1b1c" :box (:line-width 3 :color "#1b1b1c"))))
 '(sml/global    ((t :inherit font-lock-preprocessor-face)))
 '(sml/filename  ((t :inherit mode-line-buffer-id)))
 '(sml/prefix    ((t :inherit (font-lock-variable-name-face sml/global))))
 '(sml/read-only ((t :inherit (font-lock-type-face sml/not-modified))))
 '(sml/modes     ((t :foreground nil :inherit sml/filename :weight normal)))
 ;; Helm
 '(helm-candidate-number ((t :background "#404954"))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-panda)
;;; smart-mode-line-panda-theme.el ends here.
