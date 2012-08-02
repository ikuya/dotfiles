;; ========== KEYBIND ==========

;; Backspace
(keyboard-translate ?\C-h ?\C-?) ; ?\C-?はDELのシーケンス
;; Help
(define-key global-map (kbd "C-x ?") 'help-command)
(define-key global-map (kbd "C-x /") 'help-command)
;; ウィンドウの切り替え([C-x o]と同じ)
(define-key global-map (kbd "C-x C-o") 'other-window)
;; 改行+インデント
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; 半ページスクロール
; http://archive.linux.or.jp/JF/JFdocs/mouse-wheel-scroll-12.html
(defun scroll-down-half-a-page()
  "Scroll-down-half-a-page"
  (interactive)
  (scroll-down (/ (window-height) 2)))
(define-key global-map (kbd "C-x C-c M-v") 'scroll-down-half-a-page)
(defun scroll-up-half-a-page()
  "Scroll up half a page"
  (interactive)
  (scroll-up (/ (window-height) 2)))
(define-key global-map (kbd "C-x C-c C-v") 'scroll-up-half-a-page)
;; 他のウィンドウをスクロール(C-M-v, C-S-M-vの代替)
(define-key global-map (kbd "C-x C-c C-n") 'scroll-other-window)
(define-key global-map (kbd "C-x C-c C-p") 'scroll-other-window-down)
;; anything起動
;(define-key global-map (kbd "C-c ;") 'anything)
;; anything-show-kill-ring
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)
;; anythin-for-files
(define-key global-map (kbd "C-x C-x") 'anything-for-files)
;; 折り返し表示のトグル
(define-key global-map (kbd "C-x C-c l") 'toggle-truncate-lines)
;; undo/redo
(define-key global-map (kbd "C-x C-c /") 'undo)
(define-key global-map (kbd "C-x C-c '") 'redo)
;; undo-tree-visualize
(define-key global-map (kbd "C-x C-c .") 'undo-tree-visualize)
;; 行全体を(改行文字も含めて)kill
(define-key global-map (kbd "C-x C-c C-k") 'kill-whole-line)
;; カーソルを移動させずに画面を一行ずつスクロール
; Emacs24では'scroll-up-line 'scroll-down-line というコマンドがあるらしい
(define-key global-map (kbd "M-n") (lambda() (interactive) (scroll-up 1)))
(define-key global-map (kbd "M-p") (lambda() (interactive) (scroll-down 1)))

;; M-fで次の単語の先頭に移動
(define-key global-map (kbd "M-f") (lambda() (interactive) (forward-word)(forward-char)))
;; カーソルの後ろの連続するスペースを削除
; http://d.hatena.ne.jp/syohex/20111017/1318857029
(defun kill-following-whitespaces-and-tabs()
  (interactive)
  (let ((orig-point (point)))
    (save-excursion
      (skip-chars-forward " \t")
      (delete-region orig-point (point)))))
(define-key global-map (kbd "C-x C-c k") 'kill-following-whitespaces-and-tabs)
;; cua-set-rectangle-mark
(define-key global-map (kbd "C-x C-c c") 'cua-set-rectangle-mark)

;; 終了
(define-key global-map (kbd "C-x C-c C-c") 'save-buffers-kill-terminal)

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

;; ========== KEYBOARD MACRO ==========
(fset 'open-line-with-indent
   "\C-e\C-m")
(define-key global-map (kbd "C-x C-c C-m") 'open-line-with-indent)
(fset 'open-previous-line-with-indent
   "\C-p\C-e\C-m")
(define-key global-map (kbd "C-x C-c C-o") 'open-previous-line-with-indent)
(fset 'copy-line
   "\C-a\C-@\C-e\367")
(define-key global-map (kbd "C-x C-c M-w") 'copy-line)
(fset 'join-lines
   "\C-n\C-a\C-?\240")
(define-key global-map (kbd "C-x C-c C-j") 'join-lines)

;; カーソル行をwindow上端に移動
(fset 'move-current-line-to-window-top
   "\C-u0\C-l")
(define-key global-map (kbd "C-x C-c t") 'move-current-line-to-window-top)
;; カーソル行をwindow下端に移動
(fset 'move-current-line-to-window-bottom
   "\C-u-1\C-l")
(define-key global-map (kbd "C-x C-c b") 'move-current-line-to-window-bottom)

;; ========== UNSET KEYBINDS ==========
;(global-unset-key "\C-t")
(global-unset-key (kbd "C-x C-p"))
