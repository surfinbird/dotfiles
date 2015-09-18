(message "** init--avy **")

(use-package avy
  :ensure t
  :bind (("C-ø" . avy-goto-word-or-subword-1)
         ("C-:" . avy-goto-char-timer)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config
  (avy-setup-default)
  )

(anr78:provide)
