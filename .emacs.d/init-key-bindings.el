;; Global key bindings

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s")       'git-grep-fullscreen)
(global-set-key (kbd "M-s S")       'rgrep-fullscreen)

;; Jump to a definition in the current file
(global-set-key (kbd "C-x C-i")     'ido-imenu)

;; Bookmarks
(global-set-key (kbd "<f2>")        'bm-toggle)
(global-set-key (kbd "C-<f2>")      'bm-next)
(global-set-key (kbd "S-<f2>")      'bm-previous)

;; Flymake
(global-set-key (kbd "C-<f9>")      'flymake-goto-prev-error)
(global-set-key (kbd "C-<f10>")     'flymake-goto-next-error)

;; vim's ci and co commands
(global-set-key (kbd "M-I")         'change-inner)
(global-set-key (kbd "M-O")         'change-outer)

;; expand region
(global-set-key (kbd "C-'")         'er/expand-region)

;; Remap back-to-indentation
(global-set-key (kbd "M-i")         'back-to-indentation)

;; Compilation
(global-set-key (kbd "S-<f7>")      'compile)
(global-set-key (kbd "C-<f7>")      'python-pylint)
(global-set-key (kbd "C-<f8>")      'python-pep8)
(global-set-key (kbd "S-<f8>")      'python-compile)
(global-set-key (kbd "S-<f4>")      'next-error)
(global-set-key (kbd "C-S-<f4>")    'previous-error)
(global-set-key (kbd "S-M-<f4>")    'first-error)

;; Quickly jump in document with ace-jump-mode
(global-set-key (kbd "C-ø")         'ace-jump-mode)

;; magit
(global-set-key (kbd "C-x g")       'magit-status)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)

;; org-mode
(global-set-key (kbd "C-c l")       'org-store-link)
(global-set-key (kbd "C-c c")       'org-capture)
(global-set-key (kbd "C-c a")       'org-agenda)
(global-set-key (kbd "C-c b")       'org-iswitchb)

;; Smex
(global-set-key (kbd "M-x")         'smex)
(global-set-key (kbd "M-X")         'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; highlight-symbol
(global-set-key (kbd "<f10>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f11>") 'highlight-symbol-prev)
(global-set-key (kbd "<f12>") 'highlight-symbol-next)
(global-set-key (kbd "<f9>") 'highlight-symbol-query-replace)

;; Evil
(global-set-key (kbd "C-*") 'evil-search-symbol-forward)
(global-set-key (kbd "C-#") 'evil-search-symbol-backward)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Misc
(global-set-key (kbd "<f1>")         'goto-line)
(global-set-key (kbd "M-<return>")  'toggle-fullscreen)
(global-set-key (kbd "C-<tab>")     'other-window)

(global-set-key [(control x) (control c)]
                (function
                 (lambda () (interactive)
                   (cond ((y-or-n-p "Quit? ")
                          (save-buffers-kill-emacs))))))


(provide 'init-key-bindings)

