(message "** init--org-mode **")

(use-package org
  :ensure t
  :defer t
  :config
  (setq
   org-startup-folded nil
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
  )

(anr78:provide)
