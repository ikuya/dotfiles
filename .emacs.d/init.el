(require 'package)
  
;; HTTP
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)
  
(package-initialize)


(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "conf")

;; init-loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")
;(custom-set-variables
; ;; custom-set-variables was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(package-selected-packages
;   '(apel scratch-log twittering-mode yasnippet web-mode use-package undo-tree smex smartparens projectile prodigy php-mode pallet nyan-mode multiple-cursors markdown-mode magit json-reformat jade-mode init-loader idle-highlight-mode htmlize helm-swoop helm-gtags helm-descbinds haskell-mode flycheck expand-region exec-path-from-shell elscreen drag-stuff diminish auto-complete)))
;(custom-set-faces
; ;; custom-set-faces was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auto-package-update vterm yasnippet web-mode use-package undo-tree twittering-mode smex smartparens scratch-log projectile prodigy php-mode pallet nyan-mode multiple-cursors markdown-mode magit json-reformat jade-mode init-loader idle-highlight-mode htmlize howm helm-swoop helm-gtags helm-descbinds haskell-mode flycheck-cask expand-region exec-path-from-shell elscreen drag-stuff diminish auto-complete apel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


 ;; auto package update
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  ; 起動時に自動更新する。ただし前回実行が7日以内であれば実行されない
  ; 自動実行したくなければ下記をコメントアウトし、
  ; 手動で M-x auto-package-update-now RET する
  ;(auto-package-update-maybe)
  )

