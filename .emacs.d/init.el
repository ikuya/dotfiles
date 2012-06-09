;; For Cocoa Emacs: CommandとOptionを入れ替え
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; タブ幅
(setq-default indent-tabs-mode nil) ; tabではなく空白文字を使う
(setq-default tab-width  4)         ; タブ幅

;; 行番号を表示する(elisp/wb-line-numner.elを読み込む)
(require 'wb-line-number)
(wb-line-number-toggle)                 ; 起動時に行番号表示

;; ~/.emacs.d/elisp ディレクトリをload pathに追加
;; ただし、add-to-load-path関数を定義した場合は不要
(add-to-list 'load-path "~/.emacs.d/elisp")
;; 上記のadd-to-list関数ではサブディレクトリを自動的に追加してくれないので、以下に
;; add-to-load-path関数を定義する
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