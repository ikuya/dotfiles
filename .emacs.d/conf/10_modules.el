;; =========================
;;   Module Settings (.emacs.d/elisp)
;; =========================

;; ---------- emacs-async ----------
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; ---------- color-moccur ----------
(when (require 'color-moccur nil t)
  ; occur-by-moccur を Keybind
  ;(define-key global-map (kbd "M-o") 'occur-by-moccur)
  ; moccur(multi buffer search)
  (define-key global-map (kbd "M-o") 'moccur)
  ; スペース区切りでAND検索
  (setq moccur-split-word t)
  ; ディレクトリ検索時に除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
;  ; Migemoを利用できる環境であれば、Migemoを使う
;  (when (and (executable-find "cmigemo")
;             (require 'migemo nil t))
;    (setq moccur-use-migemo t))
  )
;; moccur-editを利用可能にする
(require 'moccur-edit nil t)

;; ---------- undo-tree ----------
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "C-/") 'undo-tree-undo)
  (global-set-key (kbd "C-_") 'undo-tree-undo)
  (global-set-key (kbd "C-'") 'undo-tree-redo)
  (global-set-key (kbd "C-]") 'undo-tree-redo)
  )

;; ---------- Auto Complete Mode ----------
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict")
  (define-key ac-mode-map (kbd "C-S-n") 'auto-complete)
  (ac-config-default)
  ;; ac-disable-facesの初期値は
  ;; (font-lock-comment-face font-lock-string-face font-lock-doc-face)
  ;; font-lock-comment-faceがあるとコメントの中で,
  ;; font-lock-string-faceがあるとクオートで囲まれた部分"..."で
  ;; auto-completeが反応しなくなり、セレクタを補完できないので次のように
  (setq ac-disable-faces '(font-lock-doc-face))
  ;; yasnippet絡みのエラーが発生するので、その対処
  ;; http://www.kurup.org/blog/2012/10/15/emacs-autocomplete-stumbles-on-yasnippet/
  (delq 'ac-source-yasnippet ac-sources)
  )


;; ---------- howm ----------
(setq howm-directory (concat user-emacs-directory "howm"))
;(setq howm-menu-lang 'ja)
(when (require 'howm-mode nil t)
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
;; howm-save-buffer-and-kill
(define-key howm-mode-map (kbd "C-c , k") 'howm-save-buffer-and-kill)
;; メニューに表示する最近のメモ件数
(setq howm-menu-recent-num 30)

;; ---------- Egg (Git Frontend) ----------
(when (executable-find "git")
  (require 'egg nil t))

;; ---------- aplaca ----------
(require 'alpaca)

;; ---------- twittering-mode ----------
;(add-to-list 'load-path "~/.emacs.d/elisp/twittering-mode")
(when (require 'twittering-mode nil t)
  ;; NEED GnuPG and (EasyPG or alpacs.el)
  ;; and append exec-path GnuPG path
  (setq twittering-auth-method 'oauth)
  (setq twittering-use-master-password t)
  (setq twittering-initial-timeline-spec-string
        '("usobuku/c"
          "usobuku/infosec"
          "laparisa/security"
          ;":mentions"
          "usobuku/news"
          ":home")))

(defun twittering-mode-hooks()
  ;(setq twittering-timer-interval 90)
  (setq truncate-partial-width-windows nil)
  (follow-mode t)
  (setq mode-name "twmode")
  ;; API残数をmodelineに表示
  (setq twittering-display-remaining t)
  (setq twittering-display-connection-method nil)
  ;; 表示形式
  (setq twittering-status-format "%i %S(%s)%p, %@:
%FOLD{ %T // from %f%L%r%R}
"))

(add-hook 'twittering-mode-hook
          'twittering-mode-hooks)

(add-hook 'twittering-new-tweets-hook
          '(lambda()
             (define-key twittering-mode-map (kbd "F") 'twittering-favorite)
             (define-key twittering-mode-map (kbd "R") 'twittering-native-retweet)
             (define-key twittering-mode-map (kbd "C-c h") 'twittering-home-timeline)
             ))

(setq twittering-allow-insecure-server-cert t)

;; ---------- IRC (rcirc.el) ----------
;; irc-freenode.net サーバの emacs-lisp-ja チャンネルと emacs-ja チャンネルに入る
(setq rcirc-server-alist
      '(("irc.freenode.net"
         :channels ("#emacs-lisp-ja" "#emacs-ja"))))
(setq rcirc-log-flag nil) ;ログを保存 (t/nil)

;; ---------- navi2ch ----------
(when (require 'navi2ch nil t)
  (setq navi2ch-article-exist-message-range '(1 . 1000)) ;既存スレ
  (setq navi2ch-article-new-message-range '(1000 . 1))   ;新スレ
  (setq navi2ch-board-insert-subject-with-diff t)        ;レス増加数表示
  (setq navi2ch-board-insert-subject-with-unread t)      ;未読数表示
  (setq navi2ch-list-init-open-category t)
  (setq navi2ch-board-expire-date nil)
  (setq navi2ch-history-max-line nil))

;; ---------- ffap.el ----------
;; C-x C-fで、カーソル位置のファイル・URLをMini-bufferに表示
(ffap-bindings)

;; ---------- iswitchb.el ----------
;; C-x b で部分一致を有効に
(iswitchb-mode 1)
;; バッファ読み取り関数をiswitchbにする
(setq read-buffer-function 'iswitchb-read-buffer)
;; 部分文字列の代わりに正規表現を使う場合は t を設定
(setq iswitchb-regexp nil)
;; 新しいバッファ作成時にいちいち聞いてこない
(setq iswitchb-prompt-newbuffer nil)

;; ---------- bookmark.el ----------
;; Bookmarkを変更したらすぐに保存する
(setq bookmark-save-flag 1)
;; 最近使ったBookmarkをリストの先頭に移動
(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-list-top()
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (cons latest (delq latest bookmark-alist))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-list-top))

;; ---------- text-adjust.el ----------
;; 日本語textの整形
(require 'text-adjust nil t)
;; alias
(defalias 'kutouten 'text-adjust-kutouten)
(defalias 'kutouten-buffer 'text-adjust-kutouten-buffer)
(defalias 'hankaku-zenkaku-space 'text-adjust-space)
(defalias 'hankaku-zenkaku-space-buffer 'text-adjust-space-buffer)

;; ---------- gnu global ----------
(when (require 'helm-gtags nil t)
  (setq helm-gtags-mode-hook
       '(lambda()
          (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
          (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
          (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
          (define-key helm-gtags-mode-map (kbd "M-h") 'helm-gtags-pop-stack))))

;; ---------- popwin.el ----------
(require 'popwin nil t)
(popwin-mode t)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:special-display-config '(("*Help*")
                                      ("*compilatoin*" :noselect t)
                                      ("helm" :regexp t :height 0.4)
                                      ))

;; ---------- json-reformat ----------
;; http://gongo.hatenablog.com/entry/2012/02/10/222051
(require 'json-reformat nil t)

;; ---------- helm ----------
;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
(when (require 'helm-config nil t)
  (helm-mode t)

  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-x") 'helm-multi-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x C-b") 'helm-buffers-list)
  (when (require 'helm-swoop nil t)
    (define-key global-map (kbd "C-M-o") 'helm-multi-swoop-all)
    (define-key global-map (kbd "M-O") 'helm-swoop)
    )

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


;; ---------- diminish ----------
;; 指定したマイナーモードを表示しない
(when (require 'diminish nil t)
  (diminish 'helm-mode)
  (diminish 'undo-tree-mode)
  )

;; ---------- scratch-log ----------
;; https://github.com/mori-dev/scratch-log
(require 'scratch-log nil t)
;; (setq sl-scratch-log-file "~/.emacs.d/.scratch-log")
;; (setq sl-prev-scratch-string-file "~/.emacs.d/.scratch-log-prev")

;; nil なら emacs 起動時に，最後に終了したときの スクラッチバッファの内容を復元しない。初期値は t です。
;; (setq sl-restore-scratch-p nil)
;; nil なら スクラッチバッファを削除できるままにする。初期値は t です。
;; (setq sl-prohibit-kill-scratch-buffer-p nil)

