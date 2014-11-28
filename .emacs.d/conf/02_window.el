;; ========== WINDOW ==========

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
              ((= c ?k)
               (enlarge-window dy))
              ((= c ?j)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))
(define-key global-map (kbd "C-x C-c w") 'window-resizer)

;; 入力中のマウスカーソルが邪魔
;; banish: 右上隅に移動; exile: 右上隅に移動(しばらくすると元に戻る
;; jump: ランダムに移動; animate: ランダムに移動(アニメーション) none:移動しない
(if (display-mouse-p) (mouse-avoidance-mode 'banish))

;; ========== MODE LINE ==========
;; 行番号
(setq line-number-mode t)
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

;;; モード名
;; Eldocは表示しない
(setq eldoc-minor-mode-string "")
;; Undo-Treeは表示しない
(setq undo-tree-mode-lighter "")

;; バッテリー残量
(display-battery-mode 0)

;; モードラインの表示を詰める
; http://homepage1.nifty.com/blankspace/emacs/mode-line.html
(setq-default mode-line-format
              '("-"
                mode-line-mule-info
                mode-line-modified
                mode-line-frame-identification
                mode-line-buffer-identification
                " %[("
                mode-name
                mode-line-process
                minor-mode-alist
                "%n" ")%]--"
                ;(which-func-mode ("" which-func-format "-"))
                global-mode-string
                " "
                (line-number-mode "L%l-")
                (column-number-mode "C%c-")
                (-3 . "%p")
                vc-mode
                "-%-"
              ))

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

;; smooth-scroll.el
(require 'smooth-scroll nil t)
(smooth-scroll-mode 0)
(define-key global-map (kbd "C-x C-c s") 'smooth-scroll-mode)

;; scratch
; scratchの初期メッセージを消す
(setq initial-scratch-message "")

;; Whitespace mode
;; 空白, タブ文字を可視化する
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)

(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)
