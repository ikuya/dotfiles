;; ========== html mode ==========
;; indentation
(add-hook 'html-mode-hook
          (lambda()
            ;; Default indentation is 2 spaces.
            (set (make-local-variable 'sgml-basic-offset 4))))

