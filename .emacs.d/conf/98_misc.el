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

;; ------------------------------
;; MetaとSuperを入れ替える
;; ------------------------------
;; Mac上のVMでLinuxを動かすと、CmdがSuper, OptがMeta扱いとなるので、
;; MetaとSuperを入れ替えてCmdをmeta扱いにすると楽。
;; Windows上のVMではこの限りではないので、確認しよう。
;;(setq x-meta-keysym 'super)
;;(setq x-super-keysym 'meta)

;; 数字列を3桁毎にcommaで区切る
;; http://www.emacswiki.org/emacs/AddCommasToNumbers
(defun add-commas-to-numbers (number &optional separator)
  "Add commas to NUMBER and return it as a string.
Optional SEPARATOR is the string to use to separate groups.
It defaults to a comma."
  (let ((num (number-to-string number))
        (op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
    (setq num (concat
               (match-string 1 num) op
               (match-string 2 num))))
    num))
