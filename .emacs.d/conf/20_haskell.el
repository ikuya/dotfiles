;; ========== Haskell mode ==========
(require 'haskell-mode)
;; (require 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode))

;; ;; ghc-mod
;; ;; 導入:
;; ;; $ cabal update
;; ;; $ cabal install cabal-install
;; ;; $ cabal install ghc-mod
;; ;(add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin"))
;; (setq exec-path (append exec-path '("~/.cabal/bin")))
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda()
;;                                (ghc-init)))

;; ;; ghc-browse-document() の anything版
;; ; http://d.hatena.ne.jp/kitokitoki/20111217/p1
;; (require 'anything)
;; (require 'anything-config)
;; (require 'anything-match-plugin)

;; (defvar anything-c-source-ghc-mod
;;   '((name . "ghc-browse-document")
;;     (init . anything-c-source-ghc-mod)
;;     (candidates-in-buffer)
;;     (candidate-number-limit . 9999999)
;;     (action ("Open" . anything-c-source-ghc-mod-action))))

;; (defun anything-c-source-ghc-mod ()
;;   (unless (executable-find "ghc-mod")
;;     (error "ghc-mod を利用できません。ターミナルで which したり、*scratch* で exec-path を確認したりしましょう"))
;;   (let ((buffer (anything-candidate-buffer 'global)))
;;     (with-current-buffer buffer
;;       (call-process "ghc-mod" nil t t "list"))))

;; (defun anything-c-source-ghc-mod-action (candidate)
;;   (interactive "P")
;;   (let* ((pkg (ghc-resolve-package-name candidate)))
;;     (anything-aif (and pkg candidate)
;;         (ghc-display-document pkg it nil)
;;       (message "No document found"))))

;; (defun anything-ghc-browse-document ()
;;   (interactive)
;;   (anything anything-c-source-ghc-mod))

;; ;; M-x anything-ghc-browse-document() に対応するキーの割り当て
;; ;; ghc-mod の設定のあとに書いた方がよいかもしれません
;; (add-hook 'haskell-mode-hook
;;   (lambda()
;;     (define-key haskell-mode-map (kbd "C-M-d") 'anything-ghc-browse-document)))
;; ;; (追加) Mac OS XではC-M-dがOSに取られている(「辞書で調べる」)
;; ; さらに付け加えると、C-M-dはOSに取られているが、C-S-M-dは効く
;; ; ので、keybindを追加しておく
;; (add-hook 'haskell-mode-hook
;;           (lambda()
;;             (define-key haskell-mode-map (kbd "C-c d") 'anything-ghc-browse-document)))

;; ---------- Input ---------
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook
          (lambda()
            (local-set-key [backtab] 'haskell-indent-cycle)))

;; ---------- Keybind ----------
(define-key haskell-mode-map (kbd "C-c a") 'align-regexp)
