(message "** init--avy **")

(use-package avy
  :ensure t
  :bind (("C-ø" . avy-goto-word-or-subword-1)
         ("C-:" . avy-goto-char-timer)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config
  (avy-setup-default)
:config
(defhydra hydra-avy (global-map "M-g" :color blue)
  "avy-goto"
  ("c" avy-goto-char "char")
  ("C" avy-goto-char-2 "char-2")
  ("w" avy-goto-word-1 "word")
  ("s" avy-goto-subword-1 "subword")
  ("u" link-hint-open-link "open-URI")
  ("U" link-hint-copy-link "copy-URI"))
)

(torpeanders:provide)
