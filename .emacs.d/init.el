;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; Setup load path
(message "*** Setting load paths")
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "vendor"))
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

;; Fix our good looks
(require 'appearance)   

;; Settings for currently logged in user
(setq user-settings-dir
      (concat "~/users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Machine specific, loaded early since I need to setup a proxy at work
(cond (
       (file-exists-p "~/.emacs-this-pc.el")
       (load "~/.emacs-this-pc.el")))

;; El-get
(message "*** Setup el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
 
; local sources
(setq el-get-sources
      '())

; packages to install
(setq my-packages
      (append '(ace-jump-mode auto-complete bm dash dired-details elpy epl expand-region
                              org-mode flymake-cursor highlight-symbol idomenu jade-mode jump-char magit
                              multiple-cursors nose pkg-info projectile rainbow-mode s ido-vertical-mode
                              smex tern tomorrow-theme xcscope yasnippet evil evil-numbers
                              find-file-in-project perspective smooth-scrolling wgrep
                              smart-forward browse-kill-ring fill-column-indicator)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

;; Elpa
(message "*** Setup elpa")
(require 'init-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(guide-key flymake-jshint js2-mode sws-mode flx-ido ido-at-point ido-ubiquitous
               highlight-escape-sequences visual-regexp)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Set some sane defaults
(require 'sane-defaults)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

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
  (when (file-regular-p file)))

;; Setup extensions
(require 'init-auto-complete)
(require 'init-greps)
(require 'init-magit)
(require 'init-ido)
(require 'init-org-mode)
(require 'init-dired)
(require 'init-yasnippet)
(require 'init-perspective)
(require 'init-ffip)
(require 'init-evil)

;; Put any language specific setup here
(require 'init-c)
(require 'init-js)
(require 'init-python)

(require 'compilation)

(require 'mode-mappings)

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

(require 'multiple-cursors)
(require 'expand-region)
(require 'jump-char)
(require 'change-inner)
(require 'wgrep)
(require 'smart-forward)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

(projectile-global-mode)
(setq projectile-enable-caching t)

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

;; Iswitchb
(require 'iswitchb-highlight)
;(iswitchb-default-keybindings)

(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; Setup key bindings
(require 'init-key-bindings)

(when is-mac (require 'mac))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "03f28a4e25d3ce7e8826b0a67441826c744cbf47077fb5bc9ddb18afe115005f" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" default)))
 '(fci-rule-color "#282a2e")
 '(foreground-color "#839496")
 '(pylint-options "--output-format=parseable --include-ids=y -rn"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "#454545" :foreground "#ffffff"))))
 '(magit-item-highlight ((t (:inherit nil)))))

;; Conclude init by setting up specifics for the current user
(when (file-exists-p user-settings-dir)
  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
