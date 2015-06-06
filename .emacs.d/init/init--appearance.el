(message "init--appearance")
(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when (eq system-type 'darwin)
  (set-default-font "Source Code Pro-14:antialias=1")
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-14:antialias=1")))

;; Prevent the cursor from blinking
(blink-cursor-mode 0)

(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(anr78:provide)
