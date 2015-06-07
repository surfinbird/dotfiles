;; Global key bindings

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s")       'git-grep-fullscreen)
(global-set-key (kbd "M-s S")       'rgrep-fullscreen)

;; File finding
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-x C-b") 'quick-switch-buffer)

(global-set-key (kbd "s-y") 'bury-buffer)

;; Revert without any fuss
(global-set-key (kbd "M-<escape>") (λ (revert-buffer t t)))

;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this

;; Bookmarks
(global-set-key (kbd "<f2>")        'bm-toggle)
(global-set-key (kbd "C-<f2>")      'bm-next)
(global-set-key (kbd "S-<f2>")      'bm-previous)

;; Flymake
(global-set-key (kbd "C-<f9>")      'flymake-goto-prev-error)
(global-set-key (kbd "C-<f10>")     'flymake-goto-next-error)

;; iy-go-to-char - like f in Vim
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)
(global-set-key (kbd "s-m") 'jump-char-backward)

;; vim's ci and co commands
(global-set-key (kbd "C-c i")         'change-inner)
(global-set-key (kbd "C-c o")         'change-outer)

(global-set-key (kbd "C-c C-c i") 'copy-inner)
(global-set-key (kbd "C-c C-c o") 'copy-outer)

;; Expand region (increases selected region by semantic units)
(global-set-key (when (eq system-type 'darwin) (kbd "C-@") (kbd "C-'")) 'er/expand-region)

;; Remap back-to-indentation
(global-set-key (kbd "M-i")         'back-to-indentation)


;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Replace rectangle-text with string-rectangle
(global-set-key (kbd "C-x r t") 'string-rectangle)

;; highlight-symbol
(global-set-key (kbd "<f10>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f11>") 'highlight-symbol-prev)
(global-set-key (kbd "<f12>") 'highlight-symbol-next)
(global-set-key (kbd "<f9>") 'highlight-symbol-query-replace)

;; Change next underscore with a camel case
(global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
(global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Indentation help
(global-set-key (kbd "M-j") (λ (join-line -1)))

;; Navigation bindings

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)

;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))

;; Query replace regex key binding
;(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Yank selection in isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-yank-selection)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

;; Move windows, even in org-mode
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

;; Yank and indent
(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Increase number at point (or other change based on prefix arg)
(eval-after-load 'undo-tree '(define-key undo-tree-map (kbd "C-?") nil))

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)


;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-o") 'occur)

;; Fasd
(global-set-key (kbd "C-c f") 'fasd-find-file)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; Misc
(global-set-key (kbd "<f1>")         'goto-line)
(global-set-key (kbd "M-<return>")  'toggle-fullscreen)
(global-set-key (kbd "C-<tab>")     'other-window)

(global-set-key [(control x) (control c)]
                (function
                 (lambda () (interactive)
                   (cond ((y-or-n-p "Quit? ")
                          (save-buffers-kill-emacs))))))


(anr78:provide)

