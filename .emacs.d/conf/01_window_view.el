;; =========================
;;   Window & View
;; =========================

;; color theme
(when (require 'color-theme nil t)
  (color-theme-initialize))

;; 一行ずつスクロール
(setq scroll-conservatively 1)
;; 一画面分スクロールしたときに新しい画面内に残る行数
(setq next-screen-context-lines 1)

;; ウインドウのリサイズ(interactive)
; http://d.hatena.ne.jp/mooz/20100119/p1
(defun window-resizer()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%dy]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))
(define-key global-map (kbd "C-x C-c w") 'window-resizer)

;; 行番号を表示
(require 'linum)
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
     (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)
(global-linum-mode t)

;; 列番号
(setq column-number-mode t)

;; ファイルサイズ
(size-indication-mode 0)

;; 日付と時間
(setq display-time-string-forms
      '((format "%s %s %s %s:%s"
                dayname day monthname 24-hours minutes
                )))
(display-time-mode t)

;; 画面3分割
;; http://d.hatena.ne.jp/yascentur/20110621/1308585547
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(define-key global-map (kbd "C-x C-c 3") '(lambda ()
                           (interactive)
                           (split-window-vertically-n 3)))
(define-key global-map (kbd "C-x C-c 4") '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))

;; ウィンドウの移動
;; 反対側のウィンドウに移動できるように
(setq windmove-wrap-around t)
(define-key global-map (kbd "C-M-k") 'windmove-up)
(define-key global-map (kbd "C-M-j") 'windmove-down)
(define-key global-map (kbd "C-M-h") 'windmove-left)
(define-key global-map (kbd "C-M-l") 'windmove-right)

;; 分割したウィンドウのバッファを入れ替え
;; http://www.bookshelf.jp/soft/meadow_30.html#SEC400
(defun swap-screen()
  "Swap two screen, leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen, with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)
(global-set-key (kbd "C-x C-c r") 'swap-screen-with-cursor)

;; Whitespace mode
(require 'whitespace)
(setq whitespace-style '(face
                         tabs
                         spaces
                         trailing
                         lines
                         space-before-tab
                         newline
                         indentation::space
                         empty
                         space-after-tab
;                         space-mark
                         tab-mark
;                         newline-mark
                         ))
;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")
;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

;; メニューバーを表示しない
(menu-bar-mode -1)

;; -------------------------
;;   keybind
;; -------------------------

;; Help
(define-key global-map (kbd "C-x ?") 'help-command)
(define-key global-map (kbd "C-x /") 'help-command)

;; List of Keybindings
(define-key global-map (kbd "C-x C-c /") 'describe-bindings)

;; ウィンドウの切り替え([C-x o]と同じ)
(define-key global-map (kbd "C-x C-o") 'other-window)
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
;; 折り返し表示のトグル
(define-key global-map (kbd "C-x C-c C-l") 'toggle-truncate-lines)
;; undo-tree-visualize
(define-key global-map (kbd "C-x C-c .") 'undo-tree-visualize)
;; カーソルを移動させずに画面を一行ずつスクロール
(define-key global-map (kbd "M-n") 'scroll-up-line)
(define-key global-map (kbd "M-p") 'scroll-down-line)

;; -------------------------
;;   macro
;; -------------------------

;; カーソル行をwindow上端に移動
(fset 'move-current-line-to-window-top
   "\C-u0\C-l")
(define-key global-map (kbd "C-x C-c t") 'move-current-line-to-window-top)
;; カーソル行をwindow下端に移動
(fset 'move-current-line-to-window-bottom
   "\C-u-1\C-l")
(define-key global-map (kbd "C-x C-c b") 'move-current-line-to-window-bottom)
;; 再描画
(global-set-key (kbd "C-:") 'redraw-display)
