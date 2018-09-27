;; ========== Cocoa Emacs用のConfig ==========

;; ---------- PATH ----------
(setq exec-path (append exec-path '("/opt/local/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; ---------- KEYBOARD ----------
;; CommandとOptionを入れ替え
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

; C-zを抑制
(global-unset-key (kbd "C-z"))

;; ---------- FRAME and WINDOW ----------
;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 行番号
;; バッファ中の行番号表示
(global-linum-mode t)
;; 行番号のフォーマット
;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
(set-face-attribute 'linum nil :height 0.8)
(setq linum-format "%4d")

;; テーマ
;; http://aoe-tk.hatenablog.com/entry/20130210/1360506829
(load-theme 'wombat t)

;; 透明度を変更するコマンド M-x set-alpha
;; http://qiita.com/marcy@github/items/ba0d018a03381a964f24
(defun set-alpha (alpha-num)
  "set frame parameter 'alpha"
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha (cons alpha-num '(80))))
(set-alpha '80)

;; ツールバー非表示
(tool-bar-mode -1)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

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
;; 背景色変更
;(set-face-background 'show-paren-match-face nil)
;; faceを変更する
(set-face-attribute 'show-paren-match nil
                    :background 'unspecified
                    :underline "turquoise")

;;; ---------- FONTS ----------
;; asciiフォント
(set-face-attribute 'default nil
                    :family "Ricty"
                    :height 150)
;; 日本語フォント
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))

;; ----------  ElScreen ----------
;(require 'elscreen nil t)
;; ElScreenのプレフィックス(default: C-z)
;(elscreen-set-prefix-key (kbd "C-t"))
;(when (require 'elscreen nil t)
;  ; C-z C-zをタイプした場合にデフォルトのC-zを利用する
;  (if window-system
;      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
;    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))
(setq elscreen-prefix-key (kbd "C-t"))
(elscreen-start)

;; ---------- terminal-emulator ----------
;; C-tをPrefix-keyとする
(add-hook 'term-mode-hook
          '(lambda()
             (define-key term-raw-map (kbd "C-t")
               (lookup-key (current-global-map) (kbd "C-t")))
             (define-key term-raw-map (kbd "M-x")
               (lookup-key (current-global-map) (kbd "M-x")))
             (linum-mode)
             ))
;(define-key term-raw-map (kbd "C-p") 'previous-line)
;(define-key term-raw-map (kbd "C-n") 'next-line)


;; Live preview in Markdown mode via Markd2.app
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s"
       (shell-quote-argument (buffer-file-name))))
  )
(global-set-key (kbd "C-c m") 'markdown-preview-file)

;; ---------- misc ----------
;; ミニバッファでIMEを無効化
(mac-auto-ascii-mode 1)
