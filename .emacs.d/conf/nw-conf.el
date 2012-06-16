;;========== No Window 環境用のConfig ==========
;;対応する括弧のハイライト
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
