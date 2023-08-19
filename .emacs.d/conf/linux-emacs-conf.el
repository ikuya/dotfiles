;; ========== Emacs(with window-system)用のConfig ==========

;; ---------- PATH ----------
(setq exec-path (append exec-path '("/usr/local/bin")))

; C-zを抑制
(global-unset-key (kbd "C-z"))

;; ---------- FRAME and WINDOW ----------
;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; テーマ
;(load-theme 'tsdh-dark t)
(when window-system
  (load-theme 'wombat t))

; 起動時のウィンドウサイズ
(if window-system (progn
                    (setq initial-frame-alist '((width . 80)
                                                (height . 47)
                                                (top . 0)
                                                (left . 450)
                                                ))))

;; 透明度を変更するコマンド M-x set-alpha
;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(when window-system
  (defun set-alpha (alpha-num)
    "set frame parameter 'alpha"
    (interactive "nAlpha: ")
    (set-frame-parameter nil 'alpha (cons alpha-num '(80))))
  (set-alpha '80)
  )

;; スクロールバー非表示
(set-scroll-bar-mode nil)
;; ツールバー非表示
(tool-bar-mode -1)

;; 現在行をハイライト
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background "#000000" t))               ;背景がdarkのときの背景色
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))  ;背景がlightの時の背景色
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;;; 対応する括弧のハイライト
;; paren-mode: 対応する括弧を強調して表示する
(setq show-paren-delay 0)    ; 表示までの秒数(初期値: 0.125)
(show-paren-mode t)          ; 有効化
;; parenのスタイル
(setq show-paren-style 'expression)    ;カッコもカッコ内も強調
;(setq show-paren-style 'parenthesis)  ;カッコのみ強調
;(setq show-paren-style 'mixed)        ;対応するカッコが画面外のときはカッコ内も強調
; 背景色変更
;(set-face-background 'show-paren-match-face nil)
;; faceを変更する
(set-face-attribute 'show-paren-match nil
                    :background 'unspecified
                    :underline "turquoise")

;;; ---------- FONTS ----------
(when window-system
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "Cica-11"))
  (add-to-list 'default-frame-alist '(font . "Cica-11"))
  )

;; ----------  ElScreen ----------
;; ElScreenのプレフィックス(default: C-z)
(setq elscreen-prefix-key (kbd "C-t"))
(elscreen-start)
;(when (require 'elscreen nil t)
;  ; C-z C-zをタイプした場合にデフォルトのC-zを利用する
;  (if window-system
;      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
;    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; ----- terminal-emulator -----
;; C-tをPrefix-keyとする
(add-hook 'term-mode-hook
          '(lambda()
             (define-key term-raw-map (kbd "C-t")
               (lookup-key (current-global-map) (kbd "C-t")))
             (define-key term-raw-map (kbd "M-x")
               (lookup-key (current-global-map) (kbd "M-x")))
             ))

;; Mozc 設定
(when (require 'mozc nil t)
  (set-language-environment 'Japanese)
  (setq default-input-method 'japanese-mozc)
  (setq mozc-candidate-style 'echo-area))

;; set windows key to meta
(setq x-super-keysym 'meta)
