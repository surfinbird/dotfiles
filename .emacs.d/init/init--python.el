(message "** init--python **")

(use-package elpy
  :ensure t
  :bind (("S-<f8>" . python-compile))
  :init
  (with-eval-after-load 'python (elpy-enable))
  (add-hook 'elpy-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  :config
  (use-package pylint
    :config
    :bind (("C-<f7>" . pylint)))
)

(anr78:provide)
