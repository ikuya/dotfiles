;; =========================
;;   File
;; =========================

;; kill buffer
(define-key global-map (kbd "C-x k") 'kill-buffer-and-window)

;; ---------- uniquify ----------
;; ミニバッファなどで同一ファイル名を区別する
;; uniquify is a Built-in el
; http://d.hatena.ne.jp/wadap/20120415/1334468285
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
