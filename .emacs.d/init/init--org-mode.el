(message "** init--org-mode **")

(use-package org
  :ensure t
  :defer t
  :config
  (use-package org-plus-contrib
    :ensure t
    :defer t)
  (require 'ox-confluence)
  (setq
   org-startup-folded nil
  ; agenda
   org-agenda-files "~/org/agenda-files"  
   org-agenda-start-on-weekday 1
  ; clock
   org-clock-into-drawer	"CLOCK"
   org-clock-out-when-done t
   org-clock-in-switch-to-state nil
   ; log
   org-log-note-clock-out t
   ; time
   org-time-clocksum-use-fractional t
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

  (key-chord-define-global "OM"
                           (defhydra hydra-org (:color red :columns 3)
                             "Org Mode Movements"
                             ("n" outline-next-visible-heading "next heading")
                             ("p" outline-previous-visible-heading "prev heading")
                             ("N" org-forward-heading-same-level "next heading at same level")
                             ("P" org-backward-heading-same-level "prev heading at same level")
                             ("u" outline-up-heading "up heading")
                             ("g" org-goto "goto" :exit t))
                           )
  )

(torpeanders:provide)
