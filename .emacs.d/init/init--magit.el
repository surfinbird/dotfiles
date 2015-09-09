(message "** init--magit **")

(use-package  magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)

  ;; Don't bother me with flyspell keybindings
  (eval-after-load "flyspell"
   '(bind-key "C-." nil flyspell-mode-map))
  )

(anr78:provide)
