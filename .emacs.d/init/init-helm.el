(use-package helm
  :ensure t
  :bind (
         ("C-c h" . helm-command-prefix)
         )
  :config
  (use-package  helm-projectile
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

  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-i" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map)
  
  (helm-mode 1)
)

(anr78:provide)
