;========== Cocoa Emacs用のConfig ==========
;; CommandとOptionを入れ替え
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
                                                (left . 650)
                                                ))))
;; ウィンドウの透明化
(add-to-list 'default-frame-alist '(alpha . (0.90 0.90)))

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; 現在行をハイライト
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background "#6c6c6c" t))               ;背景がdarkのときの背景色
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t)) ;背景がlightの時の背景色
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;;; 対応する括弧のハイライト
;; paren-mode: 対応する括弧を強調して表示する
(setq show-paren-delay 0)	; 表示までの秒数(初期値: 0.125)
(show-paren-mode t)			; 有効化
;; parenのスタイル
(setq show-paren-style 'expression)	; expressionは括弧内も強調表示
;; faceを変更する
(set-face-attribute 'show-paren-match-face nil
                    :background nil :foreground nil
                    :underline "#ffff00" :weight 'bold)
; 背景色変更
(set-face-background 'show-paren-match-face nil)
