;; ========== WINDOW ==========
;;; ウィンドウに行番号を表示する
(global-linum-mode t)

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
      '((format "%s %s/%s %s:%s"
                dayname day month 24-hours minutes
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
                "%n" ")%]"
                vc-mode
                " - "
                global-mode-string
                " "
                (which-func-mode ("" which-func-format "-"))
                (line-number-mode "L%l-")
                (column-number-mode "C%c-")
                (-3 . "%p")
                "-%-"
              ))
