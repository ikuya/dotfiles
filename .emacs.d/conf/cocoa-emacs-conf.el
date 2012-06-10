;; Cocoa Emacs用のConfig

;; For Cocoa Emacs: CommandとOptionを入れ替え
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; asciiフォント
(set-face-attribute 'default nil
                    :family "Ricty"
                    :height 130)
;; 日本語フォント
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))

;; テーマ
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-subtle-hacker))

;; 起動時のウィンドウサイズ
(if window-system (progn
                    (setq initial-frame-alist '((width . 45)
                                                (height . 70)
                                                (top . 0)
                                                (left . 600)
                                                ))))
;; ウィンドウの透明化
(add-to-list 'default-frame-alist '(alpha . (0.90 0.90)))
