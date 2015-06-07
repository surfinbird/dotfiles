(message "** init--ace **")

(use-package  ace-isearch
  :ensure t
  :config
  (global-ace-isearch-mode 1))

(use-package  ace-jump-mode
  :ensure t
  :bind (("C-ø" . ace-jump-mode))
  )

(anr78:provide)
