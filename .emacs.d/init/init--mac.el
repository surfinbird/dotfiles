(message "** init--mac **")

(when (eq system-type 'darwin)
  (message "init--mac")
  
  (use-package dash :ensure t)

  (set-frame-font "Source Code Pro-14:antialias=1")
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-14:antialias=1"))

  
  ;; change command to meta, and ignore option to use weird Norwegian keyboard
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (setq ns-alternate-modifier 'none)

  (setq magit-git-executable "/usr/bin/git")

  ;; Norwegian mac-keyboard alt-keys)
  ;; (define-key key-translation-map (kbd "s-8") (kbd "["))
  ;; (define-key key-translation-map (kbd "s-(") (kbd "{"))
  ;; (define-key key-translation-map (kbd "s-9") (kbd "]"))
  ;; (define-key key-translation-map (kbd "s-)") (kbd "}"))
  ;; (define-key key-translation-map (kbd "s-7") (kbd "|"))
  ;; (define-key key-translation-map (kbd "s-/") (kbd "\\"))
  ;; (define-key key-translation-map (kbd "M-s-7") (kbd "M-|"))

  ;; (global-set-key (kbd "s-u") 'universal-argument)
  ;; (global-set-key (kbd "s--") 'negative-argument)
  ;; (--dotimes 5 (global-set-key (read-kbd-macro (format "s-%d" it)) 'digit-argument))

  ;; Move to trash when deleting stuff
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")

  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)

  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))  
  )

(torpeanders:provide)
