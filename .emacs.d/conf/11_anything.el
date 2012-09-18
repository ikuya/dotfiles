;; ========== Anything config (.emacs.d/elisp/anything*) ==========

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

;; ---------- ドキュメント横断検索 ----------
;; anything-for-documentを定義
(setq anything-for-document-sources
      (list anything-c-source-man-pages
            anything-c-source-info-cl
            anything-c-source-info-pages
            anything-c-source-info-elisp
            anything-c-source-apropos-emacs-commands
            anything-c-source-apropos-emacs-functions
            anything-c-source-apropos-emacs-variables))
(defun anything-for-document()
  "Preconfigured 'anything' for anything-for-document."
  (interactive)
  (anything anything-for-document-sources
            (thing-at-point 'symbol) nil nil nil
            "*anything for document*"))
(define-key global-map (kbd "C-x C-c d") 'anything-for-document)

;; ---------- ファイル検索 ----------
;; anything-filelist+
(require 'anything-startup)
(global-set-key (kbd "C-x C-c x") 'anything-filelist+)
(setq anything-c-filelist-file-name "/tmp/all.filelist")
(setq anything-grep-candidates-fast-directory-regexp "^/tmp")
