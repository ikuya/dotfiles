;; ========== GENERAL CONFIG ==========
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

;; ---------- ファイル名の文字コード ----------
;; Mac OS X
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;; Windows
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; ---------- C-x C-c をタイプした時に本当に終了するのか確認する ----------
; http://blog.livedoor.jp/techblog/archives/64599359.html
(defadvice save-buffers-kill-emacs
  (before safe-save-buffers-kill-emacs activate)
  "safe-save-buffers-kill-emacs"
  (unless (y-or-n-p "Exit Emacs?")
    (keyboard-quit)))

;; ---------- C-x C-cをunbind (save-buffers-kill-terminalは01_keybind.elでbind
(global-unset-key (kbd "C-x C-c"))

;; Find fileのデフォルトパス
(setq default-directory "~/")
