;; ========== html mode ==========
;; indentation
(add-hook 'sgml-mode-hook
          (lambda()
            ;; Default indentation is 2 spaces.
            (setq sgml-basic-offset 4)))
(add-hook 'html-mode-hook
          (lambda()
            (setq indent-line-function 'indent-relative)))
