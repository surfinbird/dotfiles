(use-package  magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)
  (setq magit-last-seen-setup-instructions "1.4.0")

  (eval-after-load 'ediff
    '(progn
       (set-face-foreground 'ediff-odd-diff-B "#ffffff")
       (set-face-background 'ediff-odd-diff-B "#292521")
       (set-face-foreground 'ediff-even-diff-B "#ffffff")
       (set-face-background 'ediff-even-diff-B "#292527")

       (set-face-foreground 'ediff-odd-diff-A "#ffffff")
       (set-face-background 'ediff-odd-diff-A "#292521")
       (set-face-foreground 'ediff-even-diff-A "#ffffff")))

  (defun magit-save-and-exit-commit-mode ()
    (interactive)
    (save-buffer)
    (server-edit)
    (delete-window))

  (defun magit-exit-commit-mode ()
    (interactive)
    (kill-buffer)
    (delete-window))

  (eval-after-load "git-commit-mode"
    '(bind-key "C-c C-k" 'magit-exit-commit-mode git-commit-mode-map))

  (defun magit-just-amend ()
    (interactive)
    (save-window-excursion
      (magit-with-refresh
       (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

  (eval-after-load "magit"
    '(bind-key "C-c C-a" 'magit-just-amend git-commit-mode-map))

  (defun magit-kill-file-on-line ()
    "Show file on current magit line and prompt for deletion."
    (interactive)
    (magit-visit-item)
    (delete-current-buffer-file)
    (magit-refresh))

    (eval-after-load "magit"
      '(bind-key "C-c C-k" 'magit-kill-file-on-line magit-status-mode-map))

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (bind-key "q" 'magit-quit-session)

  ;; full screen vc-annotate
  (defun vc-annotate-quit ()
    "Restores the previous window configuration and kills the vc-annotate buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :vc-annotate-fullscreen))

  (eval-after-load "vc-annotate"
    '(progn
       (defadvice vc-annotate (around fullscreen activate)
         (window-configuration-to-register :vc-annotate-fullscreen)
         ad-do-it
         (delete-other-windows))

       (bind-key "q" 'vc-annotate-quit)))

  ;; ignore whitespace
  (defun magit-toggle-whitespace ()
    (interactive)
    (if (member "-w" magit-diff-options)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-options "-w")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "-w" magit-diff-options))
    (magit-refresh))

  (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map)

  ;; Don't bother me with flyspell keybindings
  (eval-after-load "flyspell"
   '(bind-key "C-." nil flyspell-mode-map))
  (use-package  magit-topgit :ensure t)
  )

(anr78:provide)
