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
(define-key global-map (kbd "C-c M-v") 'scroll-down-half-a-page)
(defun scroll-up-half-a-page()
  "Scroll up half a page"
  (interactive)
  (scroll-up (/ (window-height) 2)))
(define-key global-map (kbd "C-c C-v") 'scroll-up-half-a-page)
;; 他のウィンドウをスクロール(C-M-v, C-S-M-vの代替)
(define-key global-map (kbd "C-c C-n") 'scroll-other-window)
(define-key global-map (kbd "C-c C-p") 'scroll-other-window-down)
;; anything起動
(define-key global-map (kbd "C-c ;") 'anything)
;; anything-show-kill-ring
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)
;; anythin-for-files
(define-key global-map (kbd "C-x C-x") 'anything-for-files)
;; 折り返し表示のトグル
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;; undo/redo
(define-key global-map (kbd "C-c /") 'undo)
(define-key global-map (kbd "C-c '") 'redo)
;; undo-tree-visualize
(define-key global-map (kbd "C-c .") 'undo-tree-visualize)
;; 行全体を(改行文字も含めて)kill
(define-key global-map (kbd "C-c C-k") 'kill-whole-line)
;; カーソルを移動させずに画面を一行ずつスクロール
; Emacs24では'scroll-up-line 'scroll-down-line というコマンドがあるらしい
(define-key global-map (kbd "M-n") (lambda() (interactive) (scroll-up 1)))
(define-key global-map (kbd "M-p") (lambda() (interactive) (scroll-down 1)))

;; cua-set-rectangle-mark
(define-key global-map (kbd "C-c c") 'cua-set-rectangle-mark)

;; ========== KEYBOARD MACRO ==========
(fset 'open-line-with-indent
   "\C-e\C-m")
(define-key global-map (kbd "C-c C-m") 'open-line-with-indent)
(fset 'open-previous-line-with-indent
   "\C-p\C-e\C-m")
(define-key global-map (kbd "C-c C-o") 'open-previous-line-with-indent)
(fset 'copy-line
   "\C-a\C-@\C-e\367")
(define-key global-map (kbd "C-c M-w") 'copy-line)
