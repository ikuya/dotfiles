;; ========== Haskell mode ==========
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/elisp/haskell-mode/")

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode))

;; ---------- Input ---------
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook
          (lambda()
            (local-set-key [backtab] 'haskell-indent-cycle)))

;; ---------- Keybind ----------
(add-hook 'haskell-mode-hook
          '(lambda()
             (define-key haskell-mode-map (kbd "C-c a") 'align-regexp)))
