(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p nil)

(use-package  flymake-jshint :ensure t)

(add-hook 'js2-mode-hook
     (lambda () (flymake-mode t)))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(anr78:provide)
