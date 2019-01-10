;;; rust -- rust configuration
;;; Commentary:
;;; Code:

;; Enable cargo minor-mode
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; Rust format
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;;(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
;;(setq racer-rust-src-path "/Users/robert/source/rust/src") ;; Rust source code PATH
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(unless (getenv "RUST_SRC_PATH")
  (setenv "RUST_SRC_PATH" (expand-file-name "~/path/to/rust/src")))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))



;; Experimental RLS.
;;(with-eval-after-load 'lsp-mode
;;   (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;   (require 'lsp-rust))

;; (add-hook 'rust-mode-hook #'lsp-rust-enable)
;; (add-hook 'rust-mode-hook #'flycheck-mode)


(provide 'rust)
;;; rust ends here
