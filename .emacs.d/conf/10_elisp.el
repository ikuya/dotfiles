;; ========== Elisp Config Misc (.emacs.d/elisp) ==========

;; ---------- color-theme ----------
(when (require 'color-theme nil t)
  (color-theme-initialize))

;; ---------- redo+ ----------
;; http://www.emacswiki.org/emacs/download/redo+.el
;; NOTICE: 24.3では読み込み時にエラーとなる。
;(when (require 'redo+ nil t)
;  ;; C-' にredoを割り当て
;  (global-set-key (kbd "C-'") 'redo))

;; ---------- color-moccur ----------
(when (require 'color-moccur nil t)
  ; occur-by-moccur を Keybind
  ;(define-key global-map (kbd "M-o") 'occur-by-moccur)
  ; moccur(multi buffer search)
  (define-key global-map (kbd "M-o") 'moccur)
  ; スペース区切りでAND検索
  (setq moccur-split-word t)
  ; ディレクトリ検索時に除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  ; Migemoを利用できる環境であれば、Migemoを使う
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (setq moccur-use-migemo t)))
;; moccur-editを利用可能にする
(require 'moccur-edit nil t)

;; ---------- undo-tree ----------
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "C-'") 'undo-tree-redo)
  (global-set-key (kbd "C-/") 'undo-tree-undo))

;; ---------- Auto Complete Mode ----------
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict")
  (define-key ac-mode-map (kbd "C-S-n") 'auto-complete)
  (ac-config-default)
  ;; ac-disable-facesの初期値は
  ;; (font-lock-comment-face font-lock-string-face font-lock-doc-face)
  ;; font-lock-comment-faceがあるとコメントの中で,
  ;; font-lock-string-faceがあるとクオートで囲まれた部分"..."で
  ;; auto-completeが反応しなくなり、セレクタを補完できないので次のように
  (setq ac-disable-faces '(font-lock-doc-face))
  ;; yasnippet絡みのエラーが発生するので、その対処
  ;; http://www.kurup.org/blog/2012/10/15/emacs-autocomplete-stumbles-on-yasnippet/
  (delq 'ac-source-yasnippet ac-sources)
  )


;; ---------- howm ----------
(setq howm-directory (concat user-emacs-directory "howm"))
;(setq howm-menu-lang 'ja)
(when (require 'howm-mode nil t)
  ; C-c , , でhowm-menu起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))
;; メモを保存と同値に閉じる
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

;; ---------- multi-term ----------
;(when (require 'multi-term nil t)
;  ;; 使用するshell
;  (setq multi-term-program "/bin/zsh"))

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
  (setq twittering-use-master-password t)
  (setq twittering-initial-timeline-spec-string
        '("usobuku/c"
          "usobuku/f"
          "masafuminegishi/security-jp"
          ;":mentions"
          "usobuku/news"
          ":home")))

(defun twittering-mode-hooks()
  (setq twittering-timer-interval 60)
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

;; ---------- IRC (rcirc.el) ----------
;; irc-freenode.net サーバの emacs-lisp-ja チャンネルと emacs-ja チャンネルに入る
(setq rcirc-server-alist
      '(("irc.freenode.net"
         :channels ("#emacs-lisp-ja" "#emacs-ja"))))
(setq rcirc-log-flag nil) ;ログを保存 (t/nil)

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
;; C-x C-fで、カーソル位置のファイル・URLをMini-bufferに表示
(ffap-bindings)

;; ---------- iswitchb.el ----------
;; C-x b で部分一致を有効に
(iswitchb-mode 1)
;; バッファ読み取り関数をiswitchbにする
(setq read-buffer-function 'iswitchb-read-buffer)
;; 部分文字列の代わりに正規表現を使う場合は t を設定
(setq iswitchb-regexp nil)
;; 新しいバッファ作成時にいちいち聞いてこない
(setq iswitchb-prompt-newbuffer nil)

;; ---------- bookmark.el ----------
;; Bookmarkを変更したらすぐに保存する
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

;; ---------- yasnippet ----------
(require 'yasnippet)
(add-to-list 'yas/root-directory "~/.emacs.d/elisp/yasnippet-snippets")
(yas/initialize)

;; ---------- GNU GLOBAL ----------
(when (require 'gtags nil t)
  (setq gtags-mode-hook
       '(lambda()
          (define-key gtags-mode-map (kbd "M-t") 'gtags-find-tag)
          (define-key gtags-mode-map (kbd "M-r") 'gtags-find-rtag)
          (define-key gtags-mode-map (kbd "M-s") 'gtags-find-symbol)
          (define-key gtags-mode-map (kbd "M-h") 'gtags-pop-stack))))

;; ---------- uniquify ----------
;; uniquify is a Built-in el
; http://d.hatena.ne.jp/wadap/20120415/1334468285
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ---------- popwin.el ----------
(require 'popwin nil t)
(popwin-mode t)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:special-display-config '(("*compilatoin*" :noselect t)
                                        ("helm" :regexp t :height 0.4)
                                        ))

;; ---------- ispell ----------
(require 'ispell nil t)
(setq-default ispell-program-name "aspell")

;; ;; ---------- elpa ----------
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (package-initialize)

;; ---------- json-reformat ----------
;; http://gongo.hatenablog.com/entry/2012/02/10/222051
(require 'json-reformat nil t)

;; ---------- web-mode ----------
;; 公式: http://web-mode.org/
(when (require 'web-mode nil t)
  (setq auto-mode-alist
        (append '(
                  ("\\.\\(html\\|xhtml\\|tpl\\|ejs\\)\\'" . web-mode)
                  )
                auto-mode-alist))
  (defun web-mode-hook()
    "Hooks for Web mode"
    (setq web-mode-markup-indent-offset 4) ;; html indent
    (setq web-mode-css-indent-offset 4)    ;; css indent
    (setq web-mode-code-indent-offset 4)   ;; script indent
    (setq web-mode-enable-auto-pairing t)
    ))
(add-hook 'web-mode-hook
          '(lambda()
             (define-key web-mode-map (kbd "C-c m") 'web-mode)))

;; ---------- markdown-mode ----------
;; 公式: http://jblevins.org/projects/markdown-mode/
;; プレビューするためのスクリプト: http://daringfireball.net/projects/markdown/
;;    -> .plスクリプトをpathの通っている場所に "markdown" という名で配置
(autoload 'markdown-mode
  "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
; プレビューでマルチバイト文字を扱うための設定
; cf. http://blog.uskanda.com/2012/02/09/emacs-markdown-mode-preview-ja/
(setq markdown-command-needs-filename t)
; M-n, M-p を上書きしないように、定義を無効化
(setq markdown-mode-hook
      '(lambda()
         (define-key markdown-mode-map (kbd "M-n") nil)
         (define-key markdown-mode-map (kbd "M-p") nil)
         (set (make-local-variable 'whitespace-action) nil)
         ))
; Boldは "__" で挟む(default "**")
(setq markdown-bold-underscore t)
; Italicは "_" で挟む(default "*")
(setq markdown-italic-underscore t)

;; ---------- helm ----------
;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
(when (require 'helm-config nil t)
  (helm-mode t)

  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-x") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x C-b") 'helm-buffers-list)
  (when (require 'helm-swoop nil t)
    (define-key global-map (kbd "C-M-o") 'helm-multi-swoop-all)
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
