(require 'package)
(require 'dash)

;; Add gnu and melpa to package repos
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; This allows you to specify what repository to download a particular package from
;; But it only works on Emacs 24.4, and pretest versions are given a 24.3 numbering scheme,
;; so to make things simple we'll just ignore any potential errors
(ignore-errors
(setq package-pinned-archives '((highlight-indentation . "melpa-stable"))))

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun packages-install (packages)
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(package-initialize)

(provide 'init-package)
