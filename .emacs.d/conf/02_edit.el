;; =========================
;;   Edit
;; =========================

;; タブ幅
(setq-default tab-width 4)
;; tabではなく空白文字を使う
(setq-default indent-tabs-mode nil)
;; cua-mode (矩形編集)の設定 C-RET
(cua-mode t)
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする
;; バッファの最終行でnext-lineしても新しい行を作らない
(setq next-line-add-newlines nil)
;; Automatic character pairing (e.g. parenthesis)
(electric-pair-mode t)

;; デフォルト文字コード
(prefer-coding-system 'utf-8-auto)

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

;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (define-key global-map (kbd "C-/") 'undo-tree-undo)
  (define-key global-map (kbd "C-c u") 'undo-tree-undo)
  (define-key global-map (kbd "C-;") 'undo-tree-redo)
  (define-key global-map (kbd "C-c r") 'undo-tree-redo)
  (define-key global-map (kbd "M--") 'undo-tree-redo))


;; ---------- 検索 ----------
;; カーソル位置の単語でi-search (このコマンドを実行後C-wで単語を検索語に追加)
; http://d.hatena.ne.jp/suztomo/20081123/1227466198
(defun isearch-with-word-under-cursor()
  "Move point to head of the word under the cursor, then begin i-search"
  (interactive)
  (command-execute 'backward-word)
  (command-execute 'isearch-forward))
(define-key global-map (kbd "C-x C-c C-s") 'isearch-with-word-under-cursor)
;; 日本語用追加設定(一文字ずつ検索語に追加)
; http://www.bookshelf.jp/soft/meadow_49.html#SEC714
(defun isearch-with-word-under-cursor-yank-char()
  "Add the next character from buffer into search string"
  (interactive)
  (isearch-yank-string
   (save-excursion
     (and (not isearch-forward) isearch-other-end
          (goto-char isearch-other-end))
     (buffer-substring (point) (1+ (point))))))
(define-key isearch-mode-map (kbd "C-f") 'isearch-with-word-under-cursor-yank-char)
;; 日本語用追加設定(一文字ずつ検索語から削除)
; http://www.bookshelf.jp/soft/meadow_49.html#SEC715
(defun isearch-with-word-under-cursor-kill-char()
  "Delete the previous character from search string"
  (interactive)
  (setq isearch-string
        (if (= (length isearch-string) 1)
            isearch-string
          (substring isearch-string 0 (- (length isearch-string) 1)))
        isearch-message isearch-string
        isearch-yank-flag t)
  (isearch-search-and-update))
(define-key isearch-mode-map (kbd "C-b") 'isearch-with-word-under-cursor-kill-char)
;; カーソル位置の単語をコピー
; http://ynomura.dip.jp/archives/2010/07/emacs.html
(defun kill-ring-save-current-word()
  "Save current word to kill ring"
  (interactive)
  (save-excursion
    (forward-char)
    (backward-sexp)
    (let ((pos (point)))
      (forward-sexp)
      (kill-ring-save pos (point)))
    ))
(define-key global-map (kbd "C-x C-c C-w") 'kill-ring-save-current-word)


;; -------------------------
;;   keybind
;; -------------------------
;; Backspace
(keyboard-translate ?\C-h ?\C-?) ; ?\C-?はDELのシーケンス
;; 改行+インデント
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; 行全体を(改行文字も含めて)kill
(define-key global-map (kbd "C-x C-c C-k") 'kill-whole-line)
;; カーソルの後ろの連続するスペースを削除
; http://d.hatena.ne.jp/syohex/20111017/1318857029
(defun kill-following-whitespaces-and-tabs()
  (interactive)
  (let ((orig-point (point)))
    (save-excursion
      (skip-chars-forward " \t")
      (delete-region orig-point (point)))))
(define-key global-map (kbd "C-x C-c k") 'kill-following-whitespaces-and-tabs)

;; 矩形選択 cua-set-rectangle-mark
(define-key global-map (kbd "C-x C-c c") 'cua-set-rectangle-mark)

;; -------------------------
;;   macro
;; -------------------------
;; カーソル行の直下に空白行を挿入してインデント
(fset 'open-line-with-indent
   "\C-e\C-m")
(define-key global-map (kbd "C-x C-c C-m") 'open-line-with-indent)
;; カーソル行の直上に空白行を挿入してインデント
(fset 'open-previous-line-with-indent
   "\C-p\C-e\C-m")
(define-key global-map (kbd "C-x C-c C-o") 'open-previous-line-with-indent)
;; カーソル行をコピー
(fset 'copy-whole-line
      "\C-x\C-c\C-k\C-y")
(define-key global-map (kbd "C-x C-c M-w") 'copy-whole-line)
;; カーソル行と直下の行を連結
(fset 'join-lines-without-whitespace
   [?\C-n ?\C-a backspace])
(define-key global-map (kbd "C-x C-c j") 'join-lines-without-whitespace)
;; ハイフン25個
(fset 'line-hyphen
      "\C-u25-")
(global-set-key (kbd "C-x C-c h") 'line-hyphen)
;; イコール25個
(fset 'line-equal
   "\C-u25=")
(global-set-key (kbd "C-x C-c e") 'line-equal)
;; 破線
(fset 'line-dashed
      "- - - - - - - - - - - - - - ")
(global-set-key (kbd "C-x C-c d") 'line-dashed)
