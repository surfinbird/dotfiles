(message "** init--greps **")

(use-package s :ensure t)
(use-package wgrep :ensure t)

(defvar git-grep-switches "--extended-regexp -I -n"
  "Switches to pass to `git grep'.")

(defun git-grep-fullscreen (regexp &optional files dir confirm)
  (interactive
   (let* ((regexp (grep-read-regexp))
          (files (grep-read-files regexp))
          (files (if (string= "* .*" files) "*" files))
          (dir (read-directory-name "Base directory: "
                                    nil default-directory t))
          (confirm (equal current-prefix-arg '(4))))
     (list regexp files dir confirm)))
  (let ((command (format "cd %s && git --no-pager grep %s %s -e %S -- '%s' "
                         dir
                         git-grep-switches
                         (if (s-lowercase? regexp) " --ignore-case" "")
                         regexp
                         files))
        (grep-use-null-device nil))
    (when confirm
      (setq command (read-shell-command "Run git-grep: " command 'git-grep-history)))
    (window-configuration-to-register ?$)
    (grep command)
    (switch-to-buffer "*grep*")
    (delete-other-windows)
    (beginning-of-buffer)))

(eval-after-load "grep"
  '(progn
     ;; Don't recurse into some directories
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "vendor")
     (add-to-list 'grep-find-ignored-directories "toolchains")

     ;; Add custom keybindings
     (define-key grep-mode-map (kbd "C-x C-s") 'wgrep-save-all-buffers)

     ;; Use same keybinding as occur
     (setq wgrep-enable-key "e")))

(global-set-key (kbd "M-s s")       'git-grep-fullscreen)

(require 'dash)

(defvar grep-match-positions nil)
(make-variable-buffer-local 'grep-match-positions)

(defun grep-register-match-positions ()
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Register all positions of matches
        (while (re-search-forward "\033\\[0?1;31m\\(.*?\\)\033\\[[0-9]*m" end 1)
          (add-to-list 'grep-match-positions (set-marker (make-marker) (match-beginning 1))))))))

(eval-after-load "grep"
  '(defadvice grep-mode (after grep-register-match-positions activate)
     (add-hook 'compilation-filter-hook 'grep-register-match-positions nil t)))

(defun mc/add-cursors-to-all-matches ()
  (interactive)
  (--each grep-match-positions
    (unless (= 0 it-index)
      (mc/create-fake-cursor-at-point))
    (goto-char it))
  (mc/maybe-multiple-cursors-mode))

(eval-after-load "multiple-cursors"
  '(add-to-list mc--default-cmds-to-run-once 'mc/add-cursors-to-all-matches))

(eval-after-load "wgrep"
  '(define-key wgrep-mode-map (kbd "C-c C-æ") 'mc/add-cursors-to-all-matches))

(torpeanders:provide)
