;; ========== Markdown mode ==========

;; 公式: http://jblevins.org/projects/markdown-mode/

;; プレビューするためのスクリプト: http://daringfireball.net/projects/markdown/
;;    -> .plスクリプトをpathの通っている場所に "markdown" という名で配置
(autoload 'markdown-mode
  "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
; プレビューでマルチバイト文字を扱うための設定
; cf. http://blog.uskanda.com/2012/02/09/emacs-markdown-mode-preview-ja/
(setq markdown-command-needs-filename t)
; M-n, M-p を上書きしないように、定義を無効化
(setq markdown-mode-hook
      '(lambda()
         (define-key markdown-mode-map (kbd "M-n") nil)
         (define-key markdown-mode-map (kbd "M-p") nil)
         (set (make-local-variable 'whitespace-action) nil)
         ))
; Boldは "__" で挟む(default "**")
(setq markdown-bold-underscore t)
; Italicは "_" で挟む(default "*")
(setq markdown-italic-underscore t)
