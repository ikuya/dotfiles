;; hexl-mode

;; elscreen prefix key
;; (add-hook 'hexl-mode-hook
;;           (lambda()
;;             (cond ((null window-system)
;;                    (when (require 'elscreen nil t)
;;                      ;; ElScreenのプレフィックス(default: C-z)
;;                      (elscreen-set-prefix-key (kbd "C-z"))))
;;                   (t
;;                    (when (require 'elscreen nil t)
;;                      (elscreen-set-prefix-key (kbd "C-t"))))
;;                   )))
