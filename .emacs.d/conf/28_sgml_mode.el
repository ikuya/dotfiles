;; ========== SGML mode ==========
(add-to-list 'auto-mode-alist '("\\.xml\\'" . sgml-mode))

(defalias 'xml-format 'sgml-pretty-print)
