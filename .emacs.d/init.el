;; ========== LOAD PATH ==========
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
; 各設定ファイルがあるディレクトリ(default: ~/.emacs.d/inits)
(init-loader-load "~/.emacs.d/conf")

;; ========== ELPA ==========
;;; package (emacs24では削除 - .emacs.d/elispのpackage.el[c]も削除)
;;; ELPAの利用
;; http://bit.ly/pkg-el23
(when (require 'package nil t)
  ;; パッケージリポジトリにmarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize)) ; .emacs.d/elpaにインストールされたパッケージを読み込む
