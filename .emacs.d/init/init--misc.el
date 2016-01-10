(message "** init--misc **")

(use-package s
  :ensure t)

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

;; Windmove
(windmove-default-keybindings) ;; Shift+direction
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)

(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

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
  (indent-buffer))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

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

;; Clever newlines
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun open-line-and-indent ()
  (interactive)
  (newline-and-indent)
  (end-of-line 0)
  (indent-for-tab-command))

(global-set-key (kbd "C-o") 'open-line-and-indent)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(defun replace-region-by (fn)
  (let* ((beg (region-beginning))
         (end (region-end))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (λ (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (λ (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (λ (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (λ (replace-region-by 's-upper-camel-case)))

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "M-h") 'kill-region-or-backward-word)

(defun kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; Used by stuff in init--magit
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Edit file with sudo
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "M-s e") 'sudo-edit)

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")

;; Zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z")
                (lambda (char) (interactive "cZap up to char backwards: ") (zap-up-to-char -1 char)))

;; Fixes from Peder to make emacs in a terminal behave better (key and colorwise)
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

(use-package fasd
  :ensure t
  :bind (("C-c f" . fasd-find-file))
  :config
  (global-fasd-mode 1)
  )

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; <dead-tilde> stopped working on Ubuntu 14.04, this fixes it
(require 'iso-transl)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; Browse the kill ring
(use-package  browse-kill-ring :ensure t)
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

(global-set-key (kbd "<f1>")         'goto-line)
(global-set-key (kbd "M-<return>")  'toggle-fullscreen)
(global-set-key (kbd "C-<tab>")     'other-window)

(global-set-key [(control x) (control c)]
                (function
                 (lambda () (interactive)
                   (cond ((y-or-n-p "Quit? ")
                          (save-buffers-kill-emacs))))))

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Navigation bindings
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(use-package  smart-forward
  :ensure t
  :bind (("M-<up>" . smart-up)
         ("M-<down>" . smart-down)
         ("M-<left>" . smart-backward)
         ("M-<right>" . smart-forward))
  )

(use-package  highlight-symbol
  :ensure t
  :bind (("<f10>" . highlight-symbol-at-point)
         ("<f11>" . highlight-symbol-prev)
         ("<f12>" . highlight-symbol-next)
         ("<f9>" . highlight-symbol-query-replace))
  )

(use-package  highlight-escape-sequences
  :ensure t
  :config
  (hes-mode)
  (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face))

(use-package  bm
  :ensure t
  :bind (("<f2>" . bm-toggle)
         ("C-<f2>" . bm-next)
         ("S-<f2>" . bm-previous))
  )

;; iy-go-to-char - like f in Vim
(use-package  jump-char
  :ensure t
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)
         ("s-m" . jump-char-backward))
  )

;; Fill column indicator
(setq fill-column 80)

(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-rule-width 2)
  (setq fci-rule-color "grey15")
  (setq fci-rule-column 100)
  )
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; vim's ci and co commands
(require 'change-inner)
(global-set-key (kbd "C-c i") 'change-inner)
(global-set-key (kbd "C-c o") 'change-outer)
(global-set-key (kbd "C-c C-c i") 'copy-inner)
(global-set-key (kbd "C-c C-c o") 'copy-outer)

;; Remap back-to-indentation
(global-set-key (kbd "M-i") 'back-to-indentation)

;; Replace rectangle-text with string-rectangle
(global-set-key (kbd "C-x r t") 'string-rectangle)

;; Expand region (increases selected region by semantic units)
(use-package  expand-region :ensure t)
(require 'expand-region)
(if (eq system-type 'darwin)
    (global-set-key (kbd "C-@") 'er/expand-region)
  (global-set-key (kbd "C-'") 'er/expand-region))

;; Visual regexp
(use-package visual-regexp
  :ensure t
  :bind (("M-&" . vr/query-replace)
         ("M-/" . vr/replace))
  )

(use-package  rainbow-mode :ensure t)

(use-package  dts-mode :ensure t)

(use-package  systemd :ensure t)

;; Enable company in all modes
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)

;; Company colors more suited to a dark theme
(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(use-package  dos :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  )

(use-package hydra :ensure t)

(use-package key-chord
  :ensure t
  :init (key-chord-mode 1)
  :config (setq key-chord-two-keys-delay 0.075))

;; join line
(global-set-key (kbd "M-j") (λ (join-line -1)))

;; stuff I am not sure I need ...
;; (use-package  adaptive-wrap :ensure t)
;; (use-package  dash :ensure t)
;; (use-package  epl :ensure t)
;; (use-package  nose :ensure t)
;; (use-package  pkg-info :ensure t)


(torpeanders:provide)
