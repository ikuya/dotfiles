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
