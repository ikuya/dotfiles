;; =========================
;;   Modules
;; =========================

;; ---------- howm ----------
(setq howm-directory (concat user-emacs-directory "howm"))
(setq howm-menu-lang 'ja)
(when (require 'howm nil t)
  ; C-c , , でhowm-menu起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))
;; メモを保存と同値に閉じる
(defun howm-save-buffer-and-kill()
  "Save howm note and kill immediately."
  (interactive)
  (when (and (buffer-file-name)
             (string-match "\\.howm" (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
;; メニューに表示する最近のメモ件数
(setq howm-menu-recent-num 30)

;; ---------- Egg (Git Frontend) ----------
(when (executable-find "git")
  (require 'egg nil t))

;; ---------- IRC (rcirc.el) ----------
;; irc-freenode.net サーバの emacs-lisp-ja チャンネルと emacs-ja チャンネルに入る
(setq rcirc-server-alist
      '(("irc.freenode.net"
         :channels ("#emacs-lisp-ja" "#emacs-ja"))))
(setq rcirc-log-flag nil) ;ログを保存 (t/nil)

;; ---------- ffap.el ----------
;; C-x C-fで、カーソル位置のファイル・URLをMini-bufferに表示
(ffap-bindings)

;; ---------- json-reformat ----------
;; http://gongo.hatenablog.com/entry/2012/02/10/222051
(require 'json-reformat nil t)

;; ---------- helm ----------
;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
(when (require 'helm nil t)
  (helm-mode t)

  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-x") 'helm-multi-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x C-b") 'helm-buffers-list)
;  (when (require 'helm-swoop nil t)
;    (define-key global-map (kbd "C-M-o") 'helm-multi-swoop-all)
;    (define-key global-map (kbd "M-O") 'helm-swoop)
;    )

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))

;; helm-descbinds
(when (require 'helm-descbinds nil t)
  (helm-descbinds-mode)
  ;; prior to emacs24
  ;(helm-descbinds-mode 1)
  )

;; ---------- gnu global ----------
(when (require 'helm-gtags nil t)
  (setq helm-gtags-mode-hook
       '(lambda()
          (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
          (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
          (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
          (define-key helm-gtags-mode-map (kbd "M-h") 'helm-gtags-pop-stack))))

;; ---------- diminish ----------
;; 指定したマイナーモードを表示しない
(when (require 'diminish nil t)
  (diminish 'helm-mode)
  (diminish 'undo-tree-mode)
  )

;; ---------- scratch-log ----------
;; https://github.com/mori-dev/scratch-log
(require 'scratch-log nil t)
; nil なら emacs 起動時に，最後に終了したときの スクラッチバッファの内容を復元しない。初期値は t
; (setq sl-restore-scratch-p nil)
; nil なら スクラッチバッファを削除できるままにする。初期値は t
; (setq sl-prohibit-kill-scratch-buffer-p nil)


;; ---------- exec-path-from-shell ----------
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
