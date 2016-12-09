;; ========== INPUT ==========
;; タブ幅
(setq-default tab-width 4)          ; タブ幅
(setq-default indent-tabs-mode nil) ; tabではなく空白文字を使う
;; cua-mode (矩形編集)の設定 C-RET
(cua-mode t)
(setq cua-enable-cua-keys nil)      ;CUAキーバインドを無効にする
;; バッファの最終行でnext-lineしても新しい行を作らない
(setq next-line-add-newlines nil)
;; Automatic character pairing (e.g. parenthesis)
(electric-pair-mode t)

;; M-f の改良. 「次の単語の直前のスペース」ではなく, 「次の単語の先頭」に移動する
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


;; ucs-normalize-NFC-region で濁点分離を直す
;; M-x ucs-normalize-NFC-buffer または "C-x RET u" で、
;; バッファ全体の濁点分離を直します。
;; 参考：
;; http://d.hatena.ne.jp/nakamura001/20120529/1338305696 
;; http://www.sakito.com/2010/05/mac-os-x-normalization.html
(require 'ucs-normalize)
(prefer-coding-system 'utf-8-hfs)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

(defun ucs-normalize-NFC-buffer ()
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max))
  )

(global-set-key (kbd "C-x RET u") 'ucs-normalize-NFC-buffer)
