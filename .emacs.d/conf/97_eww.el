;========== eww ==========

; ewwで画像を表示させない
(defun eww-disable-images ()
  "ewwで画像を表示させない"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload))
; ewwで画像を表示させる
(defun eww-enable-images ()
  "ewwで画像を表示させる"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))
(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))

(provide 'mylisp-eww-image)
