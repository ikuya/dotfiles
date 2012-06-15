;; ========== Elisp Config (.emacs.d/elisp) ==========
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

;;; howm
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
;; C-c C-cでhowm-save-buffer-and-kill
(define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)
