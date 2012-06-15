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

