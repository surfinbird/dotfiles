(message "** init--which-key **")

(use-package  which-key
  :ensure t
  :init
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  )

(anr78:provide)
