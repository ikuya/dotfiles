;; Theme
(load-theme 'tsdh-dark t)

;; 透明度を変更するコマンド M-x set-alpha
;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(75))))
(set-alpha '75)

;; ツールバーを非表示
(tool-bar-mode -1)
;; スクロールバーを非表示
(set-scroll-bar-mode nil)

;; 現在行をハイライト
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background "#000000" t))              ;背景がdarkのときの背景色
    (((class color) (background light))
     (:background "DimGray" t)) ;背景がlightの時の背景色
    (t (:bold nil)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;;; 対応する括弧のハイライト
;; paren-mode: 対応する括弧を強調して表示する
(setq show-paren-delay 0)    ; 表示までの秒数(初期値: 0.125)
(show-paren-mode t)          ; 有効化
;; parenのスタイル
(setq show-paren-style 'expression)    ;カッコもカッコ内も強調
;(setq show-paren-style 'parenthesis)  ;カッコのみ強調
;(setq show-paren-style 'mixed)        ;対応するカッコが画面外のときはカッコ内も強調

;; faceを変更する
(set-face-attribute 'show-paren-match nil
                    :background 'unspecified
                    :underline "turquoise")


;; ----------  ElScreen ----------
;; ElScreenのプレフィックス(default: C-z)
(setq elscreen-prefix-key (kbd "C-t"))
(elscreen-start)
;(when (require 'elscreen nil t)
;  ; C-z C-zをタイプした場合にデフォルトのC-zを利用する
;  (if window-system
;      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
;    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))
;; C-z をunset
(global-unset-key (kbd "C-z"))

;; ----- terminal-emulator -----
;; C-tをPrefix-keyとする
(add-hook 'term-mode-hook
          '(lambda()
             (define-key term-raw-map (kbd "C-t")
               (lookup-key (current-global-map) (kbd "C-t")))
             (define-key term-raw-map (kbd "M-x")
               (lookup-key (current-global-map) (kbd "M-x")))
             ))
