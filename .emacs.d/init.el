(message "** init **")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user info
(setq user-emacs-directory "~/.emacs.d/")
(setq user-full-name "Anders Rønningen"
      user-mail-address "anders@ronningen.priv.no")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sane defaults

;; remove visual noise
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1) ;; show matching paren
(global-hl-line-mode 1) ;; highlight current line
(blink-cursor-mode 0) ;; do not blink the cursor
;; <dead-tilde> stopped working on Ubuntu 14.04, this fixes it
(require 'iso-transl)
(windmove-default-keybindings) ;; Shift+direction
;; Auto refresh buffers
(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)
;; Transparently open compressed files
(auto-compression-mode t)
;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)
;; Never insert tabs
(set-default 'indent-tabs-mode nil)
;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)
;; Easily navigate sillycased words
(global-subword-mode 1)
;; Don't break lines for me, please
(setq-default truncate-lines t)
;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)
;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; Remove text in active region if inserting text
(delete-selection-mode 1)
(setq compilation-scroll-output t)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package management
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package is mandatory
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy, swiper and counsel
(use-package counsel
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :config
  (global-set-key (kbd "C-c p") 'counsel-projectile)
  (counsel-projectile-on))

(use-package counsel-gtags
  :ensure t
  :config
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  
  (with-eval-after-load 'counsel-gtags
    (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
    (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
    (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
    (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)))

(use-package swiper
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fasd
(use-package fasd
  :ensure t
  :bind (("C-c f" . fasd-find-file))
  :config
  (global-fasd-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydra
(use-package hydra :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key-chord
(use-package key-chord
  :ensure t
  :init (key-chord-mode 1)
  :config (setq key-chord-two-keys-delay 0.075))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whick-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org
(use-package org
  :ensure t
  :pin org
  :defer t
  :config
  (use-package org-plus-contrib
    :ensure t
    :pin org
    :defer t)
  (setq org-src-fontify-natively t)
  (setq
   org-agenda-start-on-weekday 1
   ; clock
   org-clock-into-drawer  "CLOCK"
   org-clock-out-when-done t
   org-clock-in-switch-to-state nil
   ; log
   org-log-note-clock-out t
   ; time
   org-time-clocksum-use-fractional t
   org-todo-keywords
   '((sequence "TODO(t)" "INPROGRESS(i@)" "|" "DONE(f@)" "DELEGATED(d@)" "CANCELLED(c@)"))
   org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("DONE" . (:foreground "green" :weight bold))
     ("DELEGATED" . (:foreground "yellow" :weight bold))
     ("CANCELLED" . (:foreground "red" :weight bold))
     ))
  ;; Location of (most) my org files
  (setq org-directory "~/org")
  ;; Don't split line when creating new org heading with <M-return>
  (setq org-M-RET-may-split-line '((item . nil)))
  ;; Hydra
  (key-chord-define-global "OM"
                           (defhydra hydra-org (:color red :columns 3)
                             "Org Mode Movements"
                             ("n" outline-next-visible-heading "next heading")
                             ("p" outline-previous-visible-heading "prev heading")
                             ("N" org-forward-heading-same-level "next heading at same level")
                             ("P" org-backward-heading-same-level "prev heading at same level")
                             ("u" outline-up-heading "up heading")
                             ("g" org-goto "goto" :exit t))
                           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color theme
(use-package moe-theme
  :ensure t
  :config
  (moe-dark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  ;; These two lines are just examples
  (setq powerline-arrow-shape 'curve)
  (setq powerline-default-separator-dir '(right . left))
  (setq sml/theme 'dark)
  ;; These two lines you really need.
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0))

;; Keep cursor away from edges when scrolling up/down
(use-package  smooth-scrolling :ensure t)

(use-package ansi-color :ensure t)

(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda()
              (subword-mode)
              (google-set-c-style)
              (google-make-newline-indent)
              (setq c-basic-offset 4))))

(use-package sws-mode
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package jade-mode
  :ensure t
  :defer t)

(use-package nsis-mode
  :ensure t
  :defer t)

(use-package qml-mode
  :ensure t
  :defer t)

(use-package dts-mode
  :ensure t
  :defer t)

(use-package systemd
  :ensure t
  :defer t)

(use-package dos
  :ensure t
  :defer t)

(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package elpy
  :ensure t
  :init
  (with-eval-after-load 'python (elpy-enable)))

(use-package multiple-cursors
  :ensure t
  :config
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C-S-c C-e"     . mc/edit-ends-of-lines)
         ("C-S-c C-a"     . mc/edit-beginnings-of-lines)))

(use-package  dired-details
  :ensure t
  :config
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package  highlight-escape-sequences
  :ensure t
  :config
  (hes-mode)
  (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face))

(use-package  bm
  :ensure t
  :bind (("<f2>" . bm-toggle)
         ("C-<f2>" . bm-next)
         ("S-<f2>" . bm-previous)))

(use-package  smart-forward
  :ensure t
  :bind (("M-<up>" . smart-up)
         ("M-<down>" . smart-down)
         ("M-<left>" . smart-backward)
         ("M-<right>" . smart-forward)))

(use-package  highlight-symbol
  :ensure t
  :bind (("<f10>" . highlight-symbol-at-point)
         ("<f11>" . highlight-symbol-prev)
         ("<f12>" . highlight-symbol-next)
         ("<f9>" . highlight-symbol-query-replace)))

(use-package browse-kill-ring
  :ensure t)
(setq browse-kill-ring-quit-action 'save-and-restore)
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

(use-package  undo-tree
  :ensure t
  :config
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode))

(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-rule-width 2)
  (setq fci-rule-color "grey15")
  (setq fci-rule-column 100))
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; Expand region (increases selected region by semantic units)
(use-package  expand-region
  :ensure t
  :config
  (if (eq system-type 'darwin)
      (global-set-key (kbd "C-@") 'er/expand-region)
    (global-set-key (kbd "C-'") 'er/expand-region)))

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  (defhydra hydra-avy (global-map "M-g" :color blue)
    "avy-goto"
    ("c" avy-goto-char "char")
    ("w" avy-goto-word-1 "word")
    ("s" avy-goto-word-or-subword-1 "subword")
    ("u" link-hint-open-link "open-URI")
    ("U" link-hint-copy-link "copy-URI")))

(use-package yasnippet
  :ensure t
  :config
;  (add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20160129.1057/snippets")
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mac/apple specific
(when (eq system-type 'darwin)
  ;; change command to meta, and ignore option to use weird Norwegian keyboard
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (setq ns-alternate-modifier 'none))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defuns
(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

;; Edit file with sudo
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun linux-c-mode-offset ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (setq c-basic-offset 8))

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'.  
All elements of this alist are checked, meaning you can enable
multiple minor modes for the same regexp.")

(defun enable-minor-mode-based-on-extension ()
  "Check file name against `auto-minor-mode-alist' to enable minor modes.
The checking happens for all pairs in `auto-minor-mode-alist'"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook
	  'enable-minor-mode-based-on-extension)

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)	
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)))
    ;; resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;; resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun no-backslash-today ()
  (replace-string "\\" "/" nil (point-min) (point-max)))
(add-hook 'compilation-filter-hook 'no-backslash-today)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard shortcuts

;; Remap back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

(global-set-key (kbd "S-<f7>")      'compile)
(global-set-key (kbd "S-<f4>")      'next-error)
(global-set-key (kbd "C-S-<f4>")    'previous-error)
(global-set-key (kbd "S-M-<f4>")    'first-error)

(global-set-key (kbd "M-s e") 'sudo-edit)

(global-set-key [(control x) (control c)]
                (function
                 (lambda () (interactive)
                   (cond ((y-or-n-p "Quit? ")
                          (save-buffers-kill-emacs))))))

(global-set-key (kbd "<f1>")        'goto-line)
(global-set-key (kbd "M-<return>")  'toggle-fullscreen)
(global-set-key (kbd "C-<tab>")     'other-window)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(global-set-key (kbd "C-c b") 'create-scratch-buffer)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

(global-set-key [remap goto-line] 'goto-line-with-feedback)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor modes
(add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.watchr$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
(add-to-list 'magic-mode-alist '("# -*-robot-*" . robot-mode))
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
(add-to-list 'auto-mode-alist '("SConstruct$" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript$" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript.*$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bb$" . bitbake-mode))
(add-to-list 'auto-mode-alist '("\\.bbappend$" . bitbake-mode))
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
(add-to-list 'auto-mode-alist '("\\.dts$" . dts-mode))
(add-to-list 'auto-mode-alist '("\\.dtsi$" . dts-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-plus-contrib moe-theme evil fasd counsel-gtags counsel-projectile counsel use-package tern-auto-complete systemd sws-mode smooth-scrolling rainbow-mode qml-mode nsis-mode multiple-cursors magit js2-mode jade-mode imenu-anywhere hydra haskell-mode google-c-style ggtags exec-path-from-shell elpy dts-mode dos avy ample-zen-theme aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
