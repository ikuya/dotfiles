;; ========== PHP mode ==========
(require 'php-mode)
(setq php-mode-hook
      '(lambda()
         (setq indent-tabs-mode nil
               tab-width 4
               c-basic-offset 4)))
