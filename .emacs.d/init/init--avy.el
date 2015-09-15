(message "** init--avy **")

(use-package avy
  :ensure t
  :bind (("C-ø" . avy-goto-word-or-subword-1)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :config
  (avy-setup-default)
  )

(anr78:provide)
