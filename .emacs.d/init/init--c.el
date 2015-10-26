(message "** init--c **")

(require 'google-c-style)
(add-hook 'c-mode-common-hook
      (lambda()
            (subword-mode)
            (google-set-c-style)
            (google-make-newline-indent)
            (setq c-basic-offset 4)))

(defun linux-c-mode-offset ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (setq c-basic-offset 8))

(anr78:provide)
