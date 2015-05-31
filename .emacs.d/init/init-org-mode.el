(use-package org
  :ensure t
  :config
  (setq
  ; agenda
   org-agenda-files "~/org/agenda-files"  
   org-agenda-start-on-weekday 1
  ; clock
   org-clock-into-drawer	"CLOCK"
   org-clock-out-when-done t
   org-clock-in-switch-to-state nil
  ; todo
   org-todo-keywords
   '((sequence "TODO(t)" "INPROGRESS(i@)" "|" "DONE(f@)" "DELEGATED(d@)" "CANCELLED(c@)"))
   org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("DONE" . (:foreground "green" :wigth bold))
     ("DELEGATED" . (:foreground "yellow" :wigth bold))
     ("CANCELLED" . (:foreground "red" :wigth bold))
     )   
   )
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  (use-package org-clock)
  (use-package org-element)
  (use-package org-table)
  (use-package plantuml-mode
    :ensure t
    :init
    (setq org-plantuml-jar-path
          (setq plantuml-jar-path (expand-file-name "~/.emacs.d/resources/plantuml.jar")))
    )
  )

(anr78:provide)
