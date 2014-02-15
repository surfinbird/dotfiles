;; C syntax
(setq c-default-style "k&r"
      c-basic-offset 4
      c-auto-newline nil
      c-tab-always-indent t)
(c-set-offset 'substatement-open 0)
(setq c-font-lock-extra-types (quote ("\\sw+_t" "bool" "complex" "imaginary" "FILE" "lconv" "tm" "va_list" "jmp_buf" "Lisp_Object" "TXPACKET")))

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

(provide 'init-c)
