(message "** init--helm **")

(use-package helm
  :ensure t
  :commands (helm-execute-persistent-action helm-select-action)
  :bind (
         ("C-c h" . helm-command-prefix)
         ("C-c h /" . helm-find)
         ("C-c h i" . helm-semantic-or-imenu)
         ("C-c h l" . helm-locate)
         ("C-c h m" . helm-man-woman)
         ("C-c h o" . helm-occur)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-h SPC" . helm-all-mark-rings)
         )
  :init
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map)  
  :config
  (use-package helm-projectile
    :ensure t
    :config
    (setq projectile-switch-project-action 'helm-projectile)
    (helm-projectile-on)
    )

  (require 'helm-config)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  
  (helm-mode 1)
)

(use-package imenu-anywhere
  :ensure t
  :bind (("C-." . helm-imenu-anywhere))
  )

(anr78:provide)
