;; These two lines are just examples
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(right . left))
(setq sml/theme 'dark)
;; These two lines you really need.
(setq sml/no-confirm-load-theme t)
(sml/setup)

(provide 'init-smart-mode-line)
