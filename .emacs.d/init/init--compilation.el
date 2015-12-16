(message "** init--python **")

(defun python-compile ()
  "Use compile to run python programs"
  (interactive)
  (compile (concat "python " (buffer-name))))

(setq compilation-scroll-output t)
(setq compile-command "cd ~/Source && ls -la")

(defun no-backslash-today ()
  (replace-string "\\" "/" nil (point-min) (point-max)))
(add-hook 'compilation-filter-hook 'no-backslash-today)

;; Compilation
(global-set-key (kbd "S-<f7>")      'compile)
(global-set-key (kbd "S-<f4>")      'next-error)
(global-set-key (kbd "C-S-<f4>")    'previous-error)
(global-set-key (kbd "S-M-<f4>")    'first-error)

(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(torpeanders:provide)
