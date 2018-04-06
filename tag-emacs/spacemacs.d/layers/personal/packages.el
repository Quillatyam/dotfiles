;;; Personal Layer

(setq personal-packages
      '(
        (outline-ivy :location local)
        ))

;;; Outline-ivy

(defun personal/init-outline-ivy ()
  (use-package outline-ivy
    :after ivy outshine macros))
