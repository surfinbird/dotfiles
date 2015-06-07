;; Global key bindings


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


(anr78:provide)

