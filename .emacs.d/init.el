;; ========== LOAD PATH ==========
;; ~/.emacs.d/elisp �f�B���N�g����load path�ɒǉ�. ������add-to-load-path�֐����`�����ꍇ�͕s�v
;(add-to-list 'load-path "~/.emacs.d/elisp")
; ��L��add-to-list�֐��ł̓T�u�f�B���N�g���������I�ɒǉ����Ă���Ȃ��̂ŁA�ȉ���
; add-to-load-path�֐����`����
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
;; �����̃f�B���N�g���Ƃ��̃T�u�f�B���N�g����load-path�ɒǉ�
(add-to-load-path "elisp" "conf" "public_repos")
;; init-loader.el���g�p
(require 'init-loader)
; �e�ݒ�t�@�C��������f�B���N�g��(default: ~/.emacs.d/inits)
(init-loader-load "~/.emacs.d/conf")

;; �o�b�N�A�b�v�t�@�C�����쐬���Ȃ� [t/nil] default:t
;(setq make-backup-files nil)
;; �I�[�g�Z�[�u�t�@�C�������Ȃ� [t/nil] default:t
;(setq auto-save-default nil)
;; �o�b�N�A�b�v�t�@�C���ƃI�[�g�Z�[�u�t�@�C����.emacs.d/backup�ɍ쐬
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backup/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))
;; auto-install
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/") ; install dir
  (auto-install-update-emacswiki-package-name t)   ; emacswiki�ɓo�^����Ă��閼�O���擾
  ;(setq url-proxy-services '(("http" . "SERVERNAME:PORT"))) ; Proxy
  (auto-install-compatibility-setup))

;;; �t�@�C�����̕����R�[�h
;; Mac OS X
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;; Windows
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;;; C-x C-c ���^�C�v�������ɖ{���ɏI������̂��m�F����
; http://blog.livedoor.jp/techblog/archives/64599359.html
(defadvice save-buffers-kill-emacs
  (before safe-save-buffers-kill-emacs activate)
  "safe-save-buffers-kill-emacs"
  (unless (y-or-n-p "Exit Emacs?")
    (keyboard-quit)))

