;; helm-ff--in-backup-directory が nil を返す/受けるケースをガード
;; 2025.09.19 emacs 30へのバージョンアップ以降、C-x C-f で Wrong type argument: stringp, nil エラーが発生するようになった。
;; M-x toggle-debug-on-error でエラーの詳細を表示させたところ、helm-ff--in-backup-directory() 関数がnilを返しているのが原因のようだが、
;; helm関連パッケージを更新してもエラーは解消しなかった。
;; そのため、暫定的に以下のパッチをいれることとする。
;; cf. https://chatgpt.com/c/68cd4855-ba80-8327-bd25-8524102a2549
(with-eval-after-load 'helm-files
  (defun my-helm-ff--in-backup-directory ()
    (let ((bdir (ignore-errors (helm-ff--backup-directory))))
      (and (stringp bdir)
           (stringp default-directory)
           (file-equal-p bdir default-directory))))
  (advice-add 'helm-ff--in-backup-directory :override #'my-helm-ff--in-backup-directory))
