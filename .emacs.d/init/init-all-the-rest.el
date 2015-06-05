
;; Install extensions if they're missing

(use-package  ace-isearch :ensure t)
(use-package  ace-jump-mode :ensure t)
(use-package  adaptive-wrap :ensure t)
(use-package  bm :ensure t)
(use-package  browse-kill-ring :ensure t)
(use-package  dash :ensure t)
(use-package  dos :ensure t)
(use-package  epl :ensure t)
(use-package  expand-region :ensure t)
(use-package  fasd :ensure t)
(use-package  fill-column-indicator :ensure t)
(use-package  find-file-in-project :ensure t)
(use-package  flymake-cursor :ensure t)
(use-package  git-timemachine :ensure t)
(use-package  highlight-escape-sequences :ensure t)
(use-package  highlight-symbol :ensure t)
(use-package  hydra :ensure t)
(use-package  idomenu :ensure t)
(use-package  jump-char :ensure t)
(use-package  nose :ensure t)
(use-package  pkg-info :ensure t)
(use-package  qml-mode :ensure t)
(use-package  rainbow-mode :ensure t)
(use-package  smart-forward :ensure t)
(use-package  smooth-scrolling :ensure t)
(use-package  tern :ensure t)
(use-package  undo-tree :ensure t)
(use-package  visual-regexp :ensure t)
(use-package  wgrep :ensure t)
(use-package  tern-auto-complete :ensure t)
(use-package  s :ensure t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; <dead-tilde> stopped working on Ubuntu 14.04, this fixes it
(require 'iso-transl)

(setq is-win (equal system-type 'windows-nt))

;; Fasd
(unless is-win (global-fasd-mode 1))

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

(require 'expand-region)
(require 'jump-char)
(require 'change-inner)
(require 'wgrep)
(require 'smart-forward)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

(require 'bm)
(setq bookmark-default-file "~/.emacs.d/bookmarks" bookmark-save-flag 1)

(load-library "flymake")
(load-library "flymake-cursor")

;; ace-isearch
;(global-ace-isearch-mode 1)

;; Enable company in all modes
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)


(anr78:provide)
