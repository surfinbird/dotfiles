(use-package  adaptive-wrap :ensure t)
(use-package  bm :ensure t)
(use-package  browse-kill-ring :ensure t)
(use-package  dash :ensure t)
(use-package  dos :ensure t)
(use-package  epl :ensure t)
(use-package  expand-region :ensure t)

(use-package  fill-column-indicator :ensure t)
(use-package  find-file-in-project :ensure t)
(use-package  flymake-cursor :ensure t)

(use-package  jump-char :ensure t)
(use-package  nose :ensure t)
(use-package  pkg-info :ensure t)
(use-package  rainbow-mode :ensure t)
(use-package  smart-forward :ensure t)
(use-package  smooth-scrolling :ensure t)
(use-package  undo-tree :ensure t)
(use-package  visual-regexp :ensure t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; <dead-tilde> stopped working on Ubuntu 14.04, this fixes it
(require 'iso-transl)

(setq is-win (equal system-type 'windows-nt))

;; Fasd
(unless is-win (global-fasd-mode 1))

;; Visual regexp
(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

(require 'expand-region)
(require 'jump-char)
(require 'change-inner)
(require 'smart-forward)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

(require 'bm)
(setq bookmark-default-file "~/.emacs.d/bookmarks" bookmark-save-flag 1)

;; Flymake
(load-library "flymake")
(load-library "flymake-cursor")

(global-set-key (kbd "C-<f9>")      'flymake-goto-prev-error)
(global-set-key (kbd "C-<f10>")     'flymake-goto-next-error)

;; Enable company in all modes
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)


(anr78:provide)
