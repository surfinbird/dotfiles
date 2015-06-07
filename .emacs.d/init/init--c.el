(message "** init--c **")

(require 'google-c-style)
(add-hook 'c-mode-common-hook
      (lambda()
            (subword-mode)
            (google-set-c-style)
            (google-make-newline-indent)
            (setq c-basic-offset 4)))

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 8))

(defun knr-c-mode ()
  "C mode as k&r would want it."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 4))

(anr78:provide)
