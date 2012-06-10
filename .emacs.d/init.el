;; init.el

;; ~/.emacs.d/elisp ディレクトリをload pathに追加
;; ただし、add-to-load-path関数を定義した場合は不要
(add-to-list 'load-path "~/.emacs.d/elisp")
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

;; タブ幅
(setq-default indent-tabs-mode nil) ; tabではなく空白文字を使う
(setq-default tab-width  4)         ; タブ幅

;; ウィンドウに行番号を表示する
(global-linum-mode t)
;; モードラインに行番号,列番号を表示
(setq line-number-mode t)
(setq column-number-mode t)

;; init-loader.elを使用
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf") ; 各設定ファイルがあるディレクトリ

;;;; KEYBIND
;;; Key remap
;; C-h: Backspace
(keyboard-translate ?\C-h ?\C-?) ; ?\C-?はDELのシーケンス
;; C-x ?: Help
(define-key global-map (kbd "C-x ?") 'help-command)
;; C-x C-o: ウィンドウの切り替え(C-x o)
(define-key global-map (kbd "C-x C-o") 'other-window)
;; C-m: 改行+インデント
(define-key global-map (kbd "C-m") 'newline-and-indent)
;;; C-c
;; C-c l: 折り返しトグル
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
