;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine specific, loaded early for possible proxy setup
(cond ((file-exists-p "~/.emacs-this-pc.el")
       (load "~/.emacs-this-pc.el")))

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     ;; better-defaults
     c-c++
     colors
     emacs-lisp
     fasd
     git
     gtags
     haskell
;     ivy
     javascript
     ;; markdown
     org
     pandoc
     python
     ruby
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     ;; spell-checking
     syntax-checking
     ;; version-control
     windows-scripts
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(bm
                                      google-c-style
                                      ;;key-chord
                                      sws-mode
                                      haskell-mode
                                      js2-mode
                                      jade-mode
                                      nsis-mode
                                      qml-mode
                                      dts-mode
                                      systemd
                                      dos
                                      multiple-cursors)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         monokai
                         zenburn
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         leuven)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 24
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (setq user-full-name "Anders Rønningen"
        user-mail-address "anders@ronningen.priv.no")

  (setq-default dotspacemacs-configuration-layers '(
     (colors :variables colors-enable-rainbow-identifiers t)))

  (setq-default dotspacemacs-configuration-layers
                  '((c-c++ :variables c-c++-enable-clang-support t)))

  (setq-default dotspacemacs-configuration-layers
                '((auto-completion :variables
                                   auto-completion-enable-snippets-in-popup t)))

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

  (global-set-key (kbd "C-c b") 'create-scratch-buffer)

  (global-set-key (kbd "C-c f") 'fasd-find-file)

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

  (use-package sws-mode
    :ensure t
    :defer t)

  (use-package haskell-mode
    :ensure t
    :defer t)

  (use-package js2-mode
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

  ;; Auto minor modes
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
  (add-to-list 'auto-mode-alist '("\\.bb$" . python-mode))
  (add-to-list 'auto-mode-alist '("\\.bbappend$" . python-mode))
  (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))
  (add-to-list 'auto-mode-alist '("\\.dts$" . dts-mode))
  (add-to-list 'auto-mode-alist '("\\.dtsi$" . dts-mode))

  ;; Shortcuts for error navigation
  (global-set-key (kbd "S-<f4>")      'next-error)
  (global-set-key (kbd "C-S-<f4>")    'previous-error)
  (global-set-key (kbd "S-M-<f4>")    'first-error)

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
    ;(indent-buffer)
    )

  (global-set-key (kbd "C-c n") 'cleanup-buffer)
  
  (defun linux-c-mode-offset ()
    "C mode with adjusted defaults for use with the Linux kernel."
    (interactive)
    (setq c-basic-offset 8))

  (use-package google-c-style
    :ensure t
    :config
    (add-hook 'c-mode-common-hook
              (lambda()
                (subword-mode)
                (google-set-c-style)
                (google-make-newline-indent)
                (setq c-basic-offset 4))))

  (use-package bm
    :ensure t
    :bind (("<f2>" . bm-toggle)
           ("C-<f2>" . bm-next)
           ("S-<f2>" . bm-previous)))

  (use-package multiple-cursors
    :ensure t
    :config
    :bind (("C-S-c C-S-c"   . mc/edit-lines)
           ("C-S-c C-e"     . mc/edit-ends-of-lines)
           ("C-S-c C-a"     . mc/edit-beginnings-of-lines)))

  (setq
   org-agenda-start-on-weekday 1
   ; clock
   org-clock-into-drawer	"CLOCK"
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
   
  (defadvice org-archive-subtree (around my-org-archive-subtree activate)
    (let ((org-archive-location
           (if (save-excursion (org-back-to-heading)
                               (> (org-outline-level) 1))
               (concat (car (split-string org-archive-location "::"))
                       "::* "
                       (car (org-get-outline-path)))
             org-archive-location)))
      ad-do-it))
  
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  (defun no-backslash-today ()
    (replace-string "\\" "/" nil (point-min) (point-max)))
  (add-hook 'compilation-filter-hook 'no-backslash-today)

  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1)

  ;; Some things are different on mac
  (when (eq system-type 'darwin)
    ;; change command to meta, and ignore option to use weird Norwegian keyboard
    (setq mac-option-modifier 'super)
    (setq mac-command-modifier 'meta)
    (setq ns-function-modifier 'hyper)
    (setq ns-alternate-modifier 'none)
    )

  (windmove-default-keybindings)
  (global-company-mode)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(evil-want-Y-yank-to-eol t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(magit-pull-arguments nil)
 '(org-agenda-files (quote ("~/src/anr/stuff/org/todo.org")))
 '(package-selected-packages
   (quote
    (yapfify uuidgen rake py-isort org-projectile org-download livid-mode skewer-mode simple-httpd live-py-mode link-hint intero hlint-refactor helm-hoogle git-link eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff eshell-z dumb-jump company-shell company-ghci column-enforce-mode color-identifiers-mode dos xterm-color ws-butler window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe use-package toc-org systemd sws-mode spacemacs-theme spaceline smooth-scrolling smeargle shm shell-pop rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-mode rainbow-identifiers rainbow-delimiters quelpa qml-mode pyvenv pytest pyenv-mode py-yapf powershell popwin pip-requirements persp-mode paradox pandoc-mode page-break-lines ox-pandoc orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file nsis-mode neotree multi-term move-text monokai-theme magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme key-chord json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-git-grep helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate google-c-style golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger ggtags flycheck-pos-tip flycheck-haskell flx-ido fish-mode fill-column-indicator fasd fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help elisp-slime-nav dts-mode disaster define-word cython-mode company-tern company-statistics company-quickhelp company-ghc company-cabal company-c-headers company-anaconda coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format chruby bundler buffer-move bracketed-paste bm auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
