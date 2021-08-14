;;========== No Window 環境用のConfig ==========
;;対応する括弧のハイライト
;; paren-mode: 対応する括弧を強調して表示する
(setq show-paren-delay 0)    ; 表示までの秒数(初期値: 0.125)
(show-paren-mode t)          ; 有効化
;; parenのスタイル
(setq show-paren-style 'expression)    ;カッコもカッコ内も強調
;(setq show-paren-style 'parenthesis)  ;カッコのみ強調
;(setq show-paren-style 'mixed)        ;対応するカッコが画面外のときはカッコ内も強調
;; faceを変更する
(set-face-attribute 'show-paren-match nil
                    :background 'unspecified
                    :underline "turquoise")

;; ----------  ElScreen ----------
;; ElScreenのプレフィックス(default: C-z)
(setq elscreen-prefix-key (kbd "C-t"))
(elscreen-start)
(when (require 'elscreen nil t)
  ; C-z C-zをタイプした場合にデフォルトのC-zを利用する
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; ---------- linum-mode ----------
(global-linum-mode t)
(set-face-attribute 'linum nil :foreground "red" :height 0.8)
(setq linum-format "%4d ")

;; ---------- browse-url を Google Chromeに ----------
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program
      (if (file-exists-p "/usr/bin/google-chrome")
          "/usr/bin/google-chrome" "/usr/bin/chromium"))

;;; ----- kill-ringをclipbordに送る -----
;;; See. https://www.reddit.com/r/emacs/comments/9qvssh/copy_text_from_emacs_to_other_programs/e8cxlfu/
;; For Linux
;(defun text-to-clipboard (text &optional push)
;  ;; I ignore push since I dunno what it does; I suspect only older emacsen require it
; (with-temp-buffer
;  (insert text)
;  (call-process-region (point-min) (point-max) "xsel --clipboard --input")))
;(setq interprogram-cut-function 'text-to-clipboard)
;; For macOS
;(defun text-to-clipboard (text &optional push)
;  ;; I ignore push since I dunno what it does; I suspect only older emacsen require it
; (with-temp-buffer
;  (insert text)
;  (call-process-region (point-min) (point-max) "pbcopy")))
;(setq interprogram-cut-function 'text-to-clipboard)
