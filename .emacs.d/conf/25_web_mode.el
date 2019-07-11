;; ========== Web mode ==========

;; 公式: http://web-mode.org/
(when (require 'web-mode nil t)
  (setq auto-mode-alist
        (append '(
                  ("\\.\\(html\\|xhtml\\|tpl\\|ejs\\)\\'" . web-mode)
                  )
                auto-mode-alist))
  (defun web-mode-hook()
    "Hooks for Web mode"
    (setq web-mode-markup-indent-offset 4) ;; html indent
    (setq web-mode-css-indent-offset 4)    ;; css indent
    (setq web-mode-code-indent-offset 4)   ;; script indent
    (setq web-mode-enable-auto-pairing t)
    ))
(add-hook 'web-mode-hook
          '(lambda()
             (define-key web-mode-map (kbd "C-c m") 'web-mode)))

;; 閉じタグを自動で追加する
(setq web-mode-enable-auto-closing t)
; ERBのタグ <% %> を自動で閉じる
(setq web-mode-enable-auto-pairing t)
