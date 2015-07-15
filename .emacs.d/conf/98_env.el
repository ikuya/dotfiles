;; --------------------------------------------------
;; Ubuntu環境用
;; --------------------------------------------------
;; 日本語入力にMozcを使用する。
;; 事前に emacs-mozc emacs-mozc-bin をインストールしておく。
;(require 'mozc)
;(set-language-environment "Japanese")
;(setq default-input-method "japanese-mozc")
;; 変換候補をミニバッファに表示する
;(setq mozc-candidate-style 'echo-area)
;; input methodの切り替え
;(global-set-key (kbd "C-j") 'toggle-input-method)

;; --------------------------------------------------
;; mozc + ac-mozc
;; --------------------------------------------------
;;cf. Mozcをモードレス化する「ac-mozc.el」をMacで動かす | Act as Professional http://hiroki.jp/ac-mozc-on-mac
;;NOTICE: 環境変数GYP_DEFINESの設定が必要: GYP_DEFINES="mac_sdk=10.9 mac_deployment_target=10.9"
;;NOTICE: mac_sdkのバージョンは /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/ を参照すること
;;NOTICE: パスは適宜変えること
;(load "~/work/src/mozc/src/unix/emacs/mozc.el")
;(setq default-input-method "japanese-mozc")
;;(setq mozc-helper-program-name "mozc_emacs_helper")
;(setq mozc-helper-program-name "~/work/src/mozc/src/out_mac/Release/mozc_emacs_helper")
; 
;; auto-completeにmozcの選択候補を含ませる関数定義
;(when (require 'ac-mozc nil t)
;  (defun my-ac-mozc-setup ()
;    (setq ac-sources
;          '(ac-source-mozc ac-source-ascii-words-in-same-mode-buffers))
;    (set (make-local-variable 'ac-auto-show-menu) 0.2))
;  )
; 
;; auto-completeを有効にし、my-ac-mozc-setup関数を呼ぶ
;; Markdown mode
;(add-to-list 'ac-modes 'markdown-mode)
;(add-hook 'markdown-mode-hook 'my-ac-mozc-setup)
;; Twittering mode
;(add-to-list 'ac-modes 'twittering-edit-mode)
;(add-hook 'twittering-edit-mode-hook 'my-ac-mozc-setup)
;; Emacs lisp mode
;(add-to-list 'ac-modes 'emacs-lisp-mode)
;(add-hook 'emacs-lisp-mode-hook 'my-ac-mozc-setup)
;; Howm mode
;(add-to-list 'ac-modes 'howm-mode)
;(add-hook 'howm-mode-hook 'my-ac-mozc-setup)
;; Text mode
;(add-to-list 'ac-modes 'text-mode)
;(add-hook 'text-mode-hook 'my-ac-mozc-setup)

