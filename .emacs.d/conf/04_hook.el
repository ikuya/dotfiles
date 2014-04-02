;; ========== HOOK ==========
;;; ファイルが #! から始まる場合、+xを付けて保存n
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;;; Emacs Lispモード用のhook
;; emacs-lisp-mode-hook用の関数を定義
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (setq mode-name "Elisp")            ;モード名
    (turn-on-eldoc-mode)))
;; elacs-lisp-modeのhookをセット
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; ファイルの保存時に行末の空白文字を削除
;(add-hook 'before-save-hook 'delete-trailing-whitespace)
