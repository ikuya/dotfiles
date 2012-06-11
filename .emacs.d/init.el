;;;; init.el

;;;; 全体設定等
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
(init-loader-load "~/.emacs.d/conf") ; 各設定ファイルがあるディレクトリ
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

;------------------------------

;;;; Elisp読み込み設定
;;; color-theme
(when (require 'color-theme nil t)
  (color-theme-initialize))

;;; redo+
;; http://www.emacswiki.org/emacs/download/redo+.el
(when (require 'redo+ nil t)
  ;; C-' にredoを割り当て
  (global-set-key (kbd "C-'") 'redo))

;;; package (emacs24では削除 - .emacs.d/elispのpackage.el[c]も削除)
;; http://bit.ly/pkg-el23
(when (require 'package nil t)
  ;; パッケージリポジトリにmarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize)) ; .emacs.d/elpaにインストールされたパッケージを読み込む

;;; color-moccur
(when (require 'color-moccur nil t)
  ; M-oにoccur-by-moccurを割り当て
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
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

;;; wgrep (ELPAでインストールしたが、wgrep-startup.elに設定を有効化する記述がないためここで有効化)
(require 'wgrep nil t)

;;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;; ElScreen
(require 'elscreen nil t)
;; ElScreenのプレフィックス(default: C-z)
(setq elscreen-prefix-key (kbd "C-t"))
(when (require 'elscreen nil t)
  ; C-z C-zをタイプした場合にデフォルトのC-zを利用する
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;;; Anything
;; (auto-intsall-batch "anything")
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3              ;候補を表示するまでの時間(default:0.5)
   anything-input-idle-delay 0.2        ;タイプして再描画するまでの時間(default:0.1)
   anything-candidate-number-limit 100  ;候補の最大表示件数(default:50)
   anything-quick-update t              ;候補が多いときに体感速度を速く
   anything-enable-shortcuts 'alphabet  ;候補選択ショートカットをアルファベットに
   )

  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))  ;root権限でアクションを実行するときにコマンド(default:su)

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150)) ;lispシンボルの補完候補の再検索時間

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install)))

;; anything-c-moccurの設定(要 color-moccur.el)
 (when (require 'anything-c-moccur nil t)
   (setq
    anything-c-moccur-anything-idle-delay 0.1
    lanything-c-moccur-highligt-info-line-flag t ;バッファの情報をハイライト
    anything-c-moccur-enable-auto-look-flag t    ;現在選択中の候補の位置を他のwindowに表示
    anything-c-moccur-enable-initial-pattern 0)  ;起動時ポイントの位置の単語を初期パターンにする
 ;; C-M-oにanything-c-moccur-occur-by-moccurを割り当て
 (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))

;;; Auto Complete Mode
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "C-S-n") 'auto-complete)
  (ac-config-default))

;------------------------------

;;;; 入力
;; タブ幅
(setq-default tab-width  4)         ; タブ幅
(setq-default indent-tabs-mode nil) ; tabではなく空白文字を使う
;; cua-mode (矩形編集)の設定 C-RET
(cua-mode t)
(setq cua-enable-cua-keys nil)	;CUAキーバインドを無効にする

;------------------------------

;;;; ウィンドウ
;;; ウィンドウに行番号を表示する
(global-linum-mode t)
;;; 現在行をハイライト
(defface my-hl-line-face
  '((((class color) (background dark))  ; 背景がdarkのときの背景色
     (:background "Purple" t))
    (((class color) (background light)) ; 背景がlightの時の背景色
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)
;;; 対応する括弧のハイライト
;; paren-mode: 対応する括弧を強調して表示する
(setq show-paren-delay 0)	; 表示までの秒数(初期値: 0.125)
(show-paren-mode t)			; 有効化
;; parenのスタイル
(setq show-paren-style 'expression)	; expressionは括弧内も強調表示
;; faceを変更する
(set-face-attribute 'show-paren-match-face nil
                    :background nil :foreground nil
                    :underline "#ffff00" :weight 'extra-bold)
(set-face-background 'show-paren-match-face nil)		; 背景色変更

;------------------------------

;;;; KEYBIND
;;; Key remap
;; C-hをBackspaceに
(keyboard-translate ?\C-h ?\C-?) ; ?\C-?はDELのシーケンス
;; Help
(define-key global-map (kbd "C-x ?") 'help-command)
(define-key global-map (kbd "C-x /") 'help-command)
;; ウィンドウの切り替え([C-x o]と同じ)
(define-key global-map (kbd "C-x C-o") 'other-window)
;; 改行+インデント
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; 半ページ下へ
(define-key global-map (kbd "C-S-v") 'View-scroll-half-page-forward)
;; 半ページ上へ
(define-key global-map (kbd "C-M-S-v") 'View-scroll-half-page-backward)
;; anything起動
(define-key global-map (kbd "C-;") 'anything)
;; M-yにanything-show-kill-ringを割り当て
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)
;; C-x C-xにanythin-for-filesを割り当て
(define-key global-map (kbd "C-x C-x") 'anything-for-files)

;;; C-c
;; 折り返し表示のトグル
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;; undo-tree-visualize
(define-key global-map (kbd "C-c C-/") 'undo-tree-visualize)

;------------------------------

;;;; ファイル名の文字コード
;; Mac OS X
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;; Windows
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;------------------------------

;;;; Mode line
;; 行番号
(setq line-number-mode t)
;; 列番号
(setq column-number-mode t)
;; ファイルサイズ
(size-indication-mode 0)
;; 日付と時間
(setq display-time-string-forms
      '((format "%s %s/%s %s:%s"
                dayname day month 24-hours minutes
                )))
(display-time-mode t)
;;; モード名を短く
;; Eldocは表示しない
(setq eldoc-minor-mode-string "")
;; Undo-Treeは表示しない
(setq undo-tree-mode-lighter "")
      
;; バッテリー残量
(display-battery-mode 0)

;------------------------------

;;;; hook
;;; ファイルが #! から始まる場合、+xを付けて保存n
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;;; Emacs Lispモード用のhook
;; emacs-lisp-mode-hook用の関数を定義
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (setq mode-name "Elisp")            ;モード名
    (turn-on-eldoc-mode)))
;; elacs-lisp-modeのhookをセット
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;------------------------------

;;;; 分割したウィンドウのバッファを入れ替え
;; http://www.bookshelf.jp/soft/meadow_30.html#SEC400
(defun swap-screen()
  "Swap two screen, leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen, with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)
(global-set-key (kbd "C-c r") 'swap-screen-with-cursor)
