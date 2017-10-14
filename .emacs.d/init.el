(package-initialize)

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "conf")


;; ========== Cask ==========
(require 'cask "~/.emacs.d/.cask/cask.el")
(cask-initialize)

;; ========== Conf 読み込み ==========
;; init-loader.elを使用
(require 'init-loader)
; 各設定ファイルがあるディレクトリ(default: ~/.emacs.d/inits)
(init-loader-load "~/.emacs.d/conf")
