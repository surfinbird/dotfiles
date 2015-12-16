(message "** init--plantuml **")

(use-package plantuml-mode
  :ensure t
  :defer t
  :init
  (setq org-plantuml-jar-path
        (setq plantuml-jar-path (expand-file-name "~/.emacs.d/resources/plantuml.jar")))
  )

(torpeanders:provide)
