;; ========== Elisp Config Misc (.emacs.d/elisp) ==========

;; ---------- color-theme ----------
(when (require 'color-theme nil t)
  (color-theme-initialize))

;; ---------- redo+ ----------
;; http://www.emacswiki.org/emacs/download/redo+.el
(when (require 'redo+ nil t)
  ;; C-' にredoを割り当て
  (global-set-key (kbd "C-'") 'redo))

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

;;---------- wgrep ----------
;;(ELPAでインストールしたが、wgrep-startup.elに設定を有効化する記述がないためここで有効化)
(require 'wgrep nil t)

;; ---------- undo-tree ----------
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; ----------  ElScreen ----------
(require 'elscreen nil t)
;; ElScreenのプレフィックス(default: C-z)
;(setq elscreen-prefix-key (kbd "C-t"))
(when (require 'elscreen nil t)
  ; C-z C-zをタイプした場合にデフォルトのC-zを利用する
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; ---------- Auto Complete Mode ----------
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "C-S-n") 'auto-complete)
  (ac-config-default))

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
;; C-c C-cでhowm-save-buffer-and-kill
(define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)

;; ---------- multi-term ----------
(when (require 'multi-term nil t)
  ;; 使用するshell
  (setq multi-term-program "/bin/zsh"))

;; ---------- Egg (Git Frontend) ----------
(when (executable-find "git")
  (require 'egg nil t))

;; ---------- aplaca ----------
(require 'alpaca)

;; ---------- twittering-mode ----------
(add-to-list 'load-path "~/work/src/twittering-mode")
(require 'twittering-mode)
;(require 'twittering-stream)
;; NEED GnuPG and (EasyPG or alpacs.el)
;; and append exec-path GnuPG path
(setq twittering-use-master-password t)
(setq twittering-timer-interval 60)
(setq twittering-initial-timeline-spec-string
      '("usobuku/c"
        "usobuku/f"
        ":home"))
(add-hook 'twittering-new-tweets-hook
          '(lambda()
             (define-key twittering-mode-map (kbd "F") 'twittering-favorite)
             (define-key twittering-mode-map (kbd "R") 'twittering-native-retweet)
             (define-key twittering-mode-map (kbd "C-c C-h") 'twittering-home-timeline)
             ))
;; API残数をmodelineに表示
(setq twittering-display-remaining t)
(setq twittering-display-connection-method nil)
;; 表示形式
(setq twittering-status-format "%i %S(%s)%p, %@:
%FILL{ %T // from %f%L%r%R}
")
(add-hook 'twittering-mode-hook '(lambda()
                                   (setq mode-name "twmode")))
