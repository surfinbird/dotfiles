(message "*** Appearance")

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Highlight current line
;(defface hl-line '((t (:background "Gray10")))
;  "Face to use for `hl-line-face'." :group 'hl-line)
;(setq hl-line-face 'hl-line)
(global-hl-line-mode 1)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Font
(set-default-font "Inconsolata-11:antialias=1")
(add-to-list 'default-frame-alist '(font . "Inconsolata-11:antialias=1"))

;; show line number
;(global-linum-mode t)

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

(provide 'appearance)
