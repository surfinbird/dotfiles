(defun python-compile ()
  "Use compile to run python programs"
  (interactive)
  (compile (concat "python " (buffer-name))))

(setq compilation-scroll-output t)
(setq compile-command "cd ~/Source && ls -la")

(defun no-backslash-today ()
  (replace-string "\\" "/" nil (point-min) (point-max)))
(add-hook 'compilation-filter-hook 'no-backslash-today)

(anr78:provide)
