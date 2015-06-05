(when
    (functionp 'list-packages)          ; auto-loaded somewhere
 
  (setq
   package-enable-at-startup    t       ; load packages installed at startup
   package-load-list            '(all)  ; all of them (latest of each)
   )
 
  (eval-after-load "package"
    '(progn
       (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
       (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
       (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
       (add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))       
       ))
 
  ;; do this before any local package init
  (when user-init-file
    (message "Initializing ELPA packages...")
    (package-initialize)
 
    ;; load package list if none loaded (typical first run)
    (when (not package-archive-contents)
      (package-refresh-contents))
 
    ;; Bootstrap `use-package'
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    ))


(setq user-emacs-directory "~/.emacs.d/")

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Setup load path
(message "*** Setting load paths")
(add-to-list 'load-path site-lisp-dir)

;; Machine specific, loaded early for possible proxy setup
(cond ((file-exists-p "~/.emacs-this-pc.el")
       (load "~/.emacs-this-pc.el")))

;; make sure we have a use-package macro available at all times
(when (not (require 'use-package nil t))
  (defmacro use-package (name &rest args)
    "Just a dummy, since use-package wasn't loaded"
    (message "Warning: Setup of %s ignored due to missing use-package" name)))

;; useful function used in each init-*.el
(defun anr78:provide ()
  "Generate symbol based on filename and provide it"
  (provide (intern (file-name-sans-extension
                    (file-name-nondirectory load-file-name)))))

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

;; Where are we?
(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))

;; Load all init-*.el-files in ~/.emacs.d/init
(let (
      (init-dir (concat user-emacs-directory "init")))
  (add-to-list 'load-path init-dir)
  (message "Loading init files...")
  (mapcar
   (lambda (file)
     (let ((base (file-name-base file)))
       (require (intern base))))
   (directory-files init-dir nil "^init-.*\\.elc?$")))

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

;; Fasd
(unless is-win (global-fasd-mode 1))

;; ace-isearch
;(global-ace-isearch-mode 1)

;; Enable company in all modes
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(cursor-color "#839496")
 '(custom-safe-themes
   (quote
    ("51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "e56f1b1c1daec5dbddc50abd00fcd00f6ce4079f4a7f66052cf16d96412a09a9" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e24180589c0267df991cf54bf1a795c07d00b24169206106624bb844292807b9" default)))
 '(safe-local-variable-values
   (quote
    ((ffip-local-excludes "node_modules" "public/images")
     (ffip-local-patterns "*.js" "*.jade" "*.css" "*.html"))))
 '(vc-handled-backends (quote (SVN Bzr Git Hg))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
