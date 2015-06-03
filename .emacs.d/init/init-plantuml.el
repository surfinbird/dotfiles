(use-package plantuml-mode
  :ensure t
  :init
  (setq org-plantuml-jar-path
        (setq plantuml-jar-path (expand-file-name "~/.emacs.d/resources/plantuml.jar")))
  )

(anr78:provide)
