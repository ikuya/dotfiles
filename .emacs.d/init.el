;;;; init.el

;;;; 全体設定等
;; ~/.emacs.d/elisp ディレクトリをload pathに追加. ただしadd-to-load-path関数を定義した場合は不要
;(add-to-list 'load-path "~/.emacs.d/elisp")
; 上記のadd-to-list関数ではサブディレクトリを自動的に追加してくれないので、以下に
; add-to-load-path関数を定義する
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")
;; init-loader.elを使用
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf") ; 各設定ファイルがあるディレクトリ
;; バックアップファイルを作成しない [t/nil] default:t
;(setq make-backup-files nil)
;; オートセーブファイルを作らない [t/nil] default:t
;(setq auto-save-default nil)
;; バックアップファイルとオートセーブファイルを.emacs.d/backupに作成
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backup/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;;;; color-theme
(when (require 'color-theme nil t)
  (color-theme-initialize))

;;;; 入力
;; タブ幅
(setq-default tab-width  4)         ; タブ幅
(setq-default indent-tabs-mode nil) ; tabではなく空白文字を使う

;;;; ウィンドウ
;;; ウィンドウに行番号を表示する
(global-linum-mode t)
;;; 現在行をハイライト
(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
     (:background "Purple" t))
    ;; 背景がlightならば背景色を緑に
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)
;;; 対応する括弧のハイライト
;; paren-mode: 対応する括弧を強調して表示する
(setq show-paren-delay 0)	; 表示までの秒数(初期値: 0.125)
(show-paren-mode t)			; 有効化
;; parenのスタイル
(setq show-paren-style 'expression)	; expressionは括弧内も強調表示
;; faceを変更する
(set-face-attribute 'show-paren-match-face nil
                    :background nil :foreground nil
                    :underline "#ffff00" :weight 'extra-bold)
(set-face-background 'show-paren-match-face nil)		; 背景色変更
;(set-face-underline-p 'show-paren-match-face "yellow")	; 下線

;;;; KEYBIND
;;; Key remap
;; C-hをBackspaceに
(keyboard-translate ?\C-h ?\C-?) ; ?\C-?はDELのシーケンス
;; C-x ?: Help
(define-key global-map (kbd "C-x ?") 'help-command)
;; ウィンドウの切り替え([C-x o]と同じ)
(define-key global-map (kbd "C-x C-o") 'other-window)
;; 改行+インデント
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; 半ページ下へ
(define-key global-map (kbd "C-S-v") 'View-scroll-half-page-forward)
;; 半ページ上へ
(define-key global-map (kbd "C-M-S-v") 'View-scroll-half-page-backward)
;;; C-c
;; 折り返し表示のトグル
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;;;; ファイル名の文字コード
;; Mac OS X
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;; Windows
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;;;; モードライン [t/0]
;; 行番号
(setq line-number-mode t)
;; 列番号
(setq column-number-mode t)
;; ファイルサイズ
(size-indication-mode 0)
;; 時計
(setq display-time-day-and-date t)	; 曜日/月/日
(setq display-time-24hr-format t)	; 24h
(display-time-mode t)
;; バッテリー残量
(display-battery-mode 0)

