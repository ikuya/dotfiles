;; =========================
;;   General settings
;; =========================

;; 日本語環境を指定
(set-language-environment "Japanese")

;; バックアップファイルとオートセーブファイルを.emacs.d/backupに作成
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backup/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;; Find fileのデフォルトパス
(setq default-directory "~/")
;; command lineのデフォルトパス
(setq command-line-default-directory "~/")

;; ファイルが #! から始まる場合、+xを付けて保存n
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; -------------------------
;;   unset keybind
;; -------------------------

(global-unset-key (kbd "C-x C-p"))
;; C-x C-cをunbind (save-buffers-kill-terminalは01_keybind.elでbind
(global-unset-key (kbd "C-x C-c"))

;; Emacsの終了
(define-key global-map (kbd "C-x C-c C-c") 'save-buffers-kill-terminal)
;; Emacs終了時に本当に終了するのか確認する
; http://blog.livedoor.jp/techblog/archives/64599359.html
(defadvice save-buffers-kill-emacs
  (before safe-save-buffers-kill-emacs activate)
  "safe-save-buffers-kill-emacs"
  (unless (y-or-n-p "Exit Emacs?")
    (keyboard-quit)))

;; TRAMPでバックアップファイルを作成しない
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; ファイルを管理者権限で開き直す関数
;; cf. http://qiita.com/k_ui/items/d9e03ea9523036970519
(defun reopen-with-sudo ()
  "Reopen current buffer-file with sudo using tramp."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (find-alternate-file (concat "/sudo::" file-name))
      (error "Cannot get a file name"))))

;; Bell無効化
(setq ring-bell-function 'ignore)
