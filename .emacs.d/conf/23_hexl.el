;; hexl-mode

;; elscreen prefix key
(add-hook 'hexl-mode-hook
          (lambda()
            (if (not window-system)
                (elscreen-set-prefix-key (kbd "C-z"))
              (elscreen-set-prefix-key (kbd "C-t")))))
