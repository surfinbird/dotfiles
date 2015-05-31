(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (global-set-key (kbd "S-<f8>") 'python-compile)
  (use-package pylint
    :config
    (global-set-key (kbd "C-<f7>") 'pylint))
  )

(anr78:provide)
