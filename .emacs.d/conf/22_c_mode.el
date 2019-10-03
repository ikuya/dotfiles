;; c-mode, c++-mode

(add-hook 'c-mode-hook
          '(lambda()
             (c-set-style "k&r")
;             (setq indent-tabs-mode t)
             (setq c-basic-offset 4)))

;; 拡張子とmodeの対応
(setq auto-mode-alist
      (append
       '(("\\.c$" . c-mode))
       '(("\\.h$" . c-mode))
       '(("\\.cpp$". c++-mode))
       auto-mode-alist))
