;========== init.el ==========

;; ---------- GENERAL CONFIG ----------
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

;; ---------- Elisp Config (.emacs.d/elisp) ----------
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
;(setq elscreen-prefix-key (kbd "C-t"))
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
    anything-c-moccur-enable-initial-pattern nil)  ;起動時ポイントの位置の単語を初期パターンにする
 ;; C-M-oにanything-c-moccur-occur-by-moccurを割り当て
 (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))

;;; Auto Complete Mode
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "C-S-n") 'auto-complete)
  (ac-config-default))

;; ---------- INPUT ----------
;; タブ幅
(setq-default tab-width 4)          ; タブ幅
(setq-default indent-tabs-mode nil) ; tabではなく空白文字を使う
;; cua-mode (矩形編集)の設定 C-RET
(cua-mode t)
(setq cua-enable-cua-keys nil)      ;CUAキーバインドを無効にする
;; バッファの最終行でnext-lineしても新しい行を作らない
(setq next-line-add-newlines nil)

;; ---------- WINDOW ----------
;;; ウィンドウに行番号を表示する
(global-linum-mode t)
;;; 現在行をハイライト
;(defface my-hl-line-face
;	'((((class color) (background dark))  ; 背景がdarkのときの背景色
;	   (:background "Purple" t))
;	  (((class color) (background light)) ; 背景がlightの時の背景色
;	   (:background "LightGoldenrodYellow" t))
;	  (t (:bold t)))
;	"hl-line's my face")
;(setq hl-line-face 'my-hl-line-face)
;(global-hl-line-mode t)
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

;; 一行ずつスクロール
(setq scroll-conservatively 1)
;; 一画面分スクロールしたときに新しい画面内に残る行数
(setq next-screen-context-lines 1)

;; ウインドウのリサイズ(interactive)
; http://d.hatena.ne.jp/mooz/20100119/p1
(defun window-resizer()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1 -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1 -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%dy]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?k)
               (enlarge-window dy))
              ((= c ?j)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))
(define-key global-map (kbd "C-c w") 'window-resizer)

;; ---------- KEYBIND ----------
;;; Key remap
;; Backspace
(keyboard-translate ?\C-h ?\C-?) ; ?\C-?はDELのシーケンス
;; Help
(define-key global-map (kbd "C-x ?") 'help-command)
(define-key global-map (kbd "C-x /") 'help-command)
;; ウィンドウの切り替え([C-x o]と同じ)
(define-key global-map (kbd "C-x C-o") 'other-window)
;; 改行+インデント
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; 半ページスクロール
; http://archive.linux.or.jp/JF/JFdocs/mouse-wheel-scroll-12.html
(defun scroll-down-half-a-page()
  "Scroll-down-half-a-page"
  (interactive)
  (scroll-down (/ (window-height) 2)))
(define-key global-map (kbd "C-c M-v") 'scroll-down-half-a-page)
(defun scroll-up-half-a-page()
  "Scroll up half a page"
  (interactive)
  (scroll-up (/ (window-height) 2)))
(define-key global-map (kbd "C-c C-v") 'scroll-up-half-a-page)
;; 他のウィンドウをスクロール(C-M-v, C-S-M-vの代替)
(define-key global-map (kbd "C-c C-n") 'scroll-other-window)
(define-key global-map (kbd "C-c C-p") 'scroll-other-window-down)
;; anything起動
(define-key global-map (kbd "C-c ;") 'anything)
;; anything-show-kill-ring
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)
;; anythin-for-files
(define-key global-map (kbd "C-x C-x") 'anything-for-files)
;; 折り返し表示のトグル
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
;; undo/redo
(define-key global-map (kbd "C-c /") 'undo)
(define-key global-map (kbd "C-c '") 'redo)
;; undo-tree-visualize
(define-key global-map (kbd "C-c .") 'undo-tree-visualize)
;; 行全体を(改行文字も含めて)kill
(define-key global-map (kbd "C-c C-k") 'kill-whole-line)
;; カーソルを移動させずに画面を一行ずつスクロール
; Emacs24では'scroll-up-line 'scroll-down-line というコマンドがあるらしい
(define-key global-map (kbd "M-n") (lambda() (interactive) (scroll-up 1)))
(define-key global-map (kbd "M-p") (lambda() (interactive) (scroll-down 1)))

;; cua-set-rectangle-mark
(define-key global-map (kbd "C-c c") 'cua-set-rectangle-mark)

;;; KEYBOARD MACRO
(fset 'open-line-with-indent
   "\C-e\C-m")
(define-key global-map (kbd "C-c C-m") 'open-line-with-indent)
(fset 'open-previous-line-with-indent
   "\C-p\C-e\C-m")
(define-key global-map (kbd "C-c C-o") 'open-previous-line-with-indent)

;; ---------- MODE LINE ----------
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

;;; モード名
;; Eldocは表示しない
(setq eldoc-minor-mode-string "")
;; Undo-Treeは表示しない
(setq undo-tree-mode-lighter "")
      
;; バッテリー残量
(display-battery-mode 0)

;; モードラインの表示を詰める
; http://homepage1.nifty.com/blankspace/emacs/mode-line.html
(setq-default mode-line-format
              '("-"
                mode-line-mule-info
                mode-line-modified
                mode-line-frame-identification
                mode-line-buffer-identification
                " %[("
                mode-name
                mode-line-process
                minor-mode-alist
                "%n" ")%]-"
                global-mode-string
                " "
                (which-func-mode ("" which-func-format "-"))
                (line-number-mode "L%l-")
                (column-number-mode "C%c-")
                (-3 . "%p")
                "-%-"
              ))

;; ---------- HOOK ----------
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

;; ---------- MISC ----------
;;; 分割したウィンドウのバッファを入れ替え
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
