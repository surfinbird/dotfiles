(setq user-emacs-directory "~/.emacs.d/")

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Setup load path
(message "*** Setting load paths")
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Machine specific, loaded early for possible proxy setup
(cond (
       (file-exists-p "~/.emacs-this-pc.el")
       (load "~/.emacs-this-pc.el")))

;; Elpa
(message "*** Setup elpa")
(load "~/.emacs.d/init/init-package.el")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; make sure we have a use-package macro available at all times
(when (not (require 'use-package nil t))
  (defmacro use-package (name &rest args)
    "Just a dummy, since use-package wasn't loaded"
    (message "Warning: Setup of %s ignored due to missing use-package" name)))

;; useful function used in each init-*.el
(defun anr78:provide ()
  "Genereate symbol based on filename and provide it"
  (provide (intern (file-name-sans-extension
                    (file-name-nondirectory load-file-name)))))

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(ace-isearch
     ace-jump-mode
     adaptive-wrap
     bm
     browse-kill-ring
     dash
     dired-details
     dos
     epl
     expand-region
     fasd
     fill-column-indicator
     find-file-in-project
     flymake-cursor
     flymake-jshint
     git-timemachine
     guide-key
     haskell-mode
     helm
     helm-projectile
     highlight-escape-sequences
     highlight-symbol
     hydra
     idomenu
     jade-mode
     js2-mode
     jump-char
     magit
     magit-topgit
     multiple-cursors
     nose
     nsis-mode
     projectile
     pkg-info
     qml-mode
     rainbow-mode
     s
     smart-forward
     smart-mode-line
     smooth-scrolling
     solarized-theme
     sws-mode
     tern
     twilight-theme
     undo-tree
     visual-regexp
     wgrep
     yasnippet)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; <dead-tilde> stopped working on Ubuntu 14.04, this fixes it
(require 'iso-transl)

;; Where are we?
(setq is-mac (equal system-type 'darwin))
(setq is-win (equal system-type 'windows-nt))

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load all init-*.el-files in ~/.emacs.d/init
(let ((force-load-messages t)
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

;; Setup key bindings
(require 'init-key-bindings)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Enable company in all modes
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Fixes from Peder to make emacs in a terminal behave better (key and colorwise)x
(eval-after-load "xterm"
  '(progn
     (define-key xterm-function-map "\e[27;4;13~" [S-M-return])
     (define-key xterm-function-map "\e[27;8;13~" [C-M-S-return])
     ))
(eval-after-load "screen"
  '(progn
     ;; override screens init to just use xterms
     (defadvice terminal-init-screen
       (around fix-terminal-init-screen first () activate)
       (terminal-init-xterm))))

(load-theme 'solarized-dark t)

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
