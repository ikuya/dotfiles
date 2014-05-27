;;========== No Window 環境用のConfig ==========
;;対応する括弧のハイライト
;; paren-mode: 対応する括弧を強調して表示する
(setq show-paren-delay 0)	; 表示までの秒数(初期値: 0.125)
(show-paren-mode t)			; 有効化
;; parenのスタイル
(setq show-paren-style 'expression)    ;カッコもカッコ内も強調
;(setq show-paren-style 'parenthesis)  ;カッコのみ強調
;(setq show-paren-style 'mixed)        ;対応するカッコが画面外のときはカッコ内も強調
;; faceを変更する
(set-face-attribute 'show-paren-match-face nil
                    :underline "#ffff00" :weight 'bold)
(set-face-background 'show-paren-match-face nil)
(set-face-foreground 'show-paren-match-face nil)

;;---------- migemo ----------
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-regex-dictionary nil)
  (load-library "migemo")
  (migemo-init)
)

;; ----------  ElScreen ----------
(require 'elscreen nil t)
;; ElScreenのプレフィックス(default: C-z)
(elscreen-set-prefix-key (kbd "C-z"))
(when (require 'elscreen nil t)
  ; C-z C-zをタイプした場合にデフォルトのC-zを利用する
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; ----- terminal-emulator -----
;; C-zをPrefix-keyとする
;(add-hook 'term-mode-hook
;    	  '(lambda()
;    		 (define-key term-raw-map (kbd "C-z")
;    		   (lookup-key (current-global-map) (kbd "C-z")))
;             (define-key term-raw-map (kbd "M-x")
;               (lookup-key (current-global-map) (kbd "M-x")))
;             )
;          )

;; ---------- emacs-w3m ----------
;; w3mコマンドのPATHに依存しているので、ここに
(setq w3m-command "/usr/bin/w3m")
(require 'w3m-load)
; alc:[検索文字列]でalc検索 (w3m-goto-url[keybind:g])
; http://mugijiru.seesaa.net/article/205303847.html
(eval-after-load "w3m-search"
  '(progn
     (add-to-list 'w3m-search-engine-alist
                  '("alc"
                    "http://eow.alc.co.jp/%s/UTF-8/"
                    utf-8))
     (add-to-list 'w3m-uri-replace-alist
                  '("\`alc:" w3m-search-uri-replace "alc"))))
(defun w3m-goto-url-empty(url)
  ""
  (interactive (list (w3m-input-url nil "" nil nil t)))
  (w3m-goto-url url))
(add-hook 'w3m-mode-hook
          (lambda()
            (define-key w3m-mode-map (kbd "g") 'w3m-goto-url-empty)))

;; ---------- theme ----------
;(when (require 'color-theme nil t)
;  (color-theme-initialize)
;  (color-theme-dark-laptop))

;; ---------- linum-mode ----------
(require 'linum nil t)
(global-linum-mode t)
(set-face-attribute 'linum nil :foreground "red" :height 0.8)
(setq linum-format "%4d ")

;; ---------- wb-line-number ----------
;; linum-modeを無効化してからwb-line-numberを有効にする
;; (linum-modeはnw環境では不安定)
;(global-linum-mode nil)
;(require 'wb-line-number)
;(setq wb-line-number-text-width 4)
;(wb-line-number-enable)

;; ---------- term+ ----------
;(when (require 'term+ nil t)
;  (require 'term+mux)
;  (require 'xterm-256color)
;  (require 'key-intercept)
;  (require 'multi-mode-util)
;  (add-hook 'term-mode-hook
;            '(lambda()
;               (setq term-default-fg-color (face-foreground 'default))
;               (setq term-default-bg-color (face-background 'default))
;               ))
;  (define-key term+char-map (kbd "C-t") nil)
;  (define-key term+char-map (kbd "C-z") nil))

;; ---------- dic-lookup-w3m ----------
;; w3mコマンドに依存しているのでここに記述
(when (require 'dic-lookup-w3m nil t)
  (define-key global-map (kbd "C-x C-c d") 'dic-lookup-w3m)
  (define-key global-map (kbd "C-x C-c l") '(lambda()
                                              (interactive)
                                              (dic-lookup-w3m "ee-longman")))
  (define-key global-map (kbd "C-x C-c a") '(lambda()
                                              (interactive)
                                              (dic-lookup-w3m "ej-alc")))
;  (define-key global-map (kbd "C-x C-c d") '(lambda()
;                                              (interactive)
;                                              (dic-lookup-w3m "jj-yahoo")))
  (define-key global-map (kbd "C-x C-c m") '(lambda()
                                              (interactive)
                                              (dic-lookup-w3m "ee-webster")))
;   (define-key global-map (kbd "C-x C-c t") '(lambda()
;                                              (interactive)
;                                              (dic-lookup-w3m "thesaurus-webster")))
)
