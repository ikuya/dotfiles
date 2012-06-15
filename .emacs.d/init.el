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

;; バックアップファイルを作成しない [t/nil] default:t
;(setq make-backup-files nil)
;; オートセーブファイルを作らない [t/nil] default:t
;(setq auto-save-default nil)
;; バックアップファイルとオートセーブファイルを.emacs.d/backupに作成
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backup/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))
;; auto-install
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/") ; install dir
  (auto-install-update-emacswiki-package-name t)   ; emacswikiに登録されている名前を取得
  ;(setq url-proxy-services '(("http" . "SERVERNAME:PORT"))) ; Proxy
  (auto-install-compatibility-setup))

;;; ファイル名の文字コード
;; Mac OS X
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;; Windows
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;;; C-x C-c をタイプした時に本当に終了するのか確認する
; http://blog.livedoor.jp/techblog/archives/64599359.html
(defadvice save-buffers-kill-emacs
  (before safe-save-buffers-kill-emacs activate)
  "safe-save-buffers-kill-emacs"
  (unless (y-or-n-p "Exit Emacs?")
    (keyboard-quit)))

