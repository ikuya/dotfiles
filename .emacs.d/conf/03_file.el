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


;; 開いているファイルのリネーム
(defun rename-current-buffer-file (new-name)
  "Rename current buffer and file it is visiting to NEW-NAME."
  (interactive "FNew name: ")
  (let ((old-name (buffer-file-name)))
    (if (not old-name)
        (message "Buffer is not visiting a file!")
      (when (get-buffer new-name)
        (kill-buffer new-name))
      (rename-file old-name new-name 1)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)
      (message "Renamed '%s' to '%s'" old-name new-name))))

(define-key global-map (kbd "C-x C-w") 'rename-current-buffer-file)
