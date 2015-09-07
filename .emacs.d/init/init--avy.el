(message "** init--avy **")

(use-package avy
  :ensure t
  :bind (("C-ø" . avy-goto-word-or-subword-1))
  :config
  (avy-setup-default)
  )

(anr78:provide)
