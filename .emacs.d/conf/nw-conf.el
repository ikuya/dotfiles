;;========== No Window 環境用のConfig ==========
;;対応する括弧のハイライト
;; paren-mode: 対応する括弧を強調して表示する
(setq show-paren-delay 0)	; 表示までの秒数(初期値: 0.125)
(show-paren-mode t)			; 有効化
;; parenのスタイル
(setq show-paren-style 'expression)	; expressionは括弧内も強調表示
;; faceを変更する
(set-face-attribute 'show-paren-match-face nil
                    :underline "#ffff00" :weight 'bold)
(set-face-background 'show-paren-match-face nil)
(set-face-foreground 'show-paren-match-face nil)

;;---------- migemo ----------
(require 'migemo)
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-regex-dictionary nil)
(load-library "migemo")
(migemo-init)

;; ----------  ElScreen ----------
(require 'elscreen nil t)
;; ElScreenのプレフィックス(default: C-z)
(when (require 'elscreen nil t)
  ; C-z C-zをタイプした場合にデフォルトのC-zを利用する
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; ----- terminal-emulator -----
;; C-zをPrefix-keyとする
(add-hook 'terminal-mode-hook
          '(lambda()
             (define-key terminal-map (kbd "C-z")
               (lookup-key (current-global-map) (kbd "C-z")))))

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

;; ---------- wb-line-number ----------
;; linum-modeを無効化してからwb-line-numberを有効にする
;; (linum-modeはnw環境では不安定)
(global-linum-mode nil)
(require 'wb-line-number)
(wb-line-number-toggle)
(setq wb-line-number-text-width 4)
