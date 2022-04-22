(use-package xml-mode
  :mode
  (("\.xml$" . nxml-mode)
   ("\.xsl$" . nxml-mode)
   ("\.xhtml$" . nxml-mode))
  :config
  (setq nxml-child-indent 4)
  (setq nxml-attribute-indent 4)
  (setq indent-tabs-mode nil)
  (setq nxml-slash-auto-complete-flag t)
  )
