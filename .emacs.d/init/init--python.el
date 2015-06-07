(message "** init--python **")

(use-package elpy
  :ensure t
  :bind (("S-<f8>" . python-compile))
  :config
  (elpy-enable)
  (use-package pylint
    :config
    :bind (("C-<f7>" . pylint))
  )
)

(anr78:provide)
