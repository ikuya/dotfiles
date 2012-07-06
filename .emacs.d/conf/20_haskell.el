;; ========== Haskell mode ==========
(require 'haskell-mode)
(require 'haskell-cabal)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal$" . haskell-cabal-mode))

;; ghc-mod
;(add-to-list 'exec-path (concat (getenv "HOME") "/.cabal/bin"))
;(autoload 'ghc-init "ghc" nil t)
;(add-hook 'haskell-mode-hook (lambda()
;                               (ghc-init)))
