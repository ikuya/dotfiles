;; ========== INPUT ==========
;; タブ幅
(setq-default tab-width 4)          ; タブ幅
(setq-default indent-tabs-mode nil) ; tabではなく空白文字を使う
;; cua-mode (矩形編集)の設定 C-RET
(cua-mode t)
(setq cua-enable-cua-keys nil)      ;CUAキーバインドを無効にする
;; バッファの最終行でnext-lineしても新しい行を作らない
(setq next-line-add-newlines nil)
;; Automatic character pairing (e.g. parenthesis)
(electric-pair-mode t)

;; M-f の改良. 「次の単語の直前のスペース」ではなく, 「次の単語の先頭」に移動する
(defun forward-word-to-beginning (&optional n)
  "Move point forward n words and place cursor at the beginning."
  (interactive "p")
  (let (myword)
    (setq myword
      (if (and transient-mark-mode mark-active)
        (buffer-substring-no-properties (region-beginning) (region-end))
        (thing-at-point 'symbol)))
    (if (not (eq myword nil))
      (forward-word n))
    (forward-word n)
    (backward-word n)))
(global-set-key (kbd "M-f") 'forward-word-to-beginning)
