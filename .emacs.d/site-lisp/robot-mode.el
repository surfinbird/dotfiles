;; Robot mode
;; ==========
;;
;; A major mode for editing robot framework text files.
;; Add the following to your .emacs file
;;
;;    (load-file "path/to/robot-mode.el")
;;    (add-to-list 'auto-mode-alist '("\\.txt\\'" . robot-mode))
;;
;; Type "M-x load-file" and give the path to the .emacs file (e.g. ~/.emacs)
;; to reload the file. Now when you open a .txt file emacs automatically sets 
;; the robot-mode on for that buffer. This will also be done automatically when
;; you start emacs.
;;
;; Please report problem to http://code.google.com/p/robot-mode
;;
;; Copyright 2010 Sakari Jokinen Licensed under the Apache License, Version 2.0 (the "License"); 
;; you may not use this file except in compliance with the License. You may obtain a copy of 
;; the License at 
;;        http://www.apache.org/licenses/LICENSE-2.0 
;; Unless required by applicable law or agreed to in writing, software distributed under the 
;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, 
;; either express or implied. See the License for the specific language governing permissions 
;; and limitations under the License. 

(setq robot-mode-keywords
      '(
	;;normal comment
	("#.*" . font-lock-comment-face)
	;;Section headers
	("\\*\\*\\* [^\\*]+ \\*\\*\\*" . font-lock-keyword-face)
	;;keyword definitions
	("^[^ \n].+" . font-lock-function-name-face)
	;;Variables
	("\\(\\$\\|@\\){\\( ?[^ }$]\\)+}" 0 font-lock-variable-name-face t)
	;;tags etc
	("\\[[^\]]+\\]+" . font-lock-constant-face)
	;;comment kw
	("comment  .*" . font-lock-comment-face)
	)
      )

(defun robot-mode-kw-at-point()
  "Return the robot keyword around the current point in buffer"
  (defun extract-kw (str)
    (defun trim (str)
      (replace-regexp-in-string "\\(^\s+\\)\\|\\(\s+$\\)\\|\n$" "" str))
    (defun cut-kw (str)
      (replace-regexp-in-string "  .*$" "" str))
    (defun cut-bdd (str) 
      (replace-regexp-in-string "^\\(given\\)\\|\\(then\\)\\|\\(when\\)\\s*" "" str))
    (cut-kw (cut-bdd (trim str)))
    )
  (let ((line (thing-at-point 'line))
	(kw-end (save-excursion (re-search-forward "$\\|\\(  \\)"))) 
	(kw-start (save-excursion (re-search-backward "^\\|\\(  \\)"))))
    (extract-kw (buffer-substring kw-start kw-end))
    )
  )

(defun robot-mode-continue-find-kw()
  "Find the next matching robot kw."
  (interactive)
  (find-tag-regexp "" t)
  )

(defun robot-mode-find-first-kw()
  "Start the robot kw search."
  (defun kw-regex (line) 
    (defun match-underscores (str)
      (replace-regexp-in-string "\\(_\\| \\)" "[_ ]?" str t t))
    (defun match-infix-args (str)
      (replace-regexp-in-string "'[^']+'" "'\\($\\|@\\){[^}]+}'" str t t))
    (match-infix-args (match-underscores line))
    )
  (setq default-kw (if (and transient-mark-mode mark-active)
			  (buffer-substring-no-properties (region-beginning) (region-end))
			(robot-mode-kw-at-point)
			))
  (let ((kw (read-from-minibuffer (format "Find kw (%s): " default-kw))))
    (if (string= "" kw) (find-tag-regexp (kw-regex default-kw))
      (find-tag-regexp (kw-regex kw)) 
      )
    )
  )

(defun robot-mode-find-kw(continue)
  "Find the kw in region or in the line where the point is from TAGS.

If 'continue' is is non nil or interactively if the function is called
with a prefix argument (i.e. prefixed with C-u) then continue from the last
found kw."
  (interactive "P")
  (if continue (robot-mode-continue-find-kw)
    (robot-mode-find-first-kw)
    )
  )

(define-derived-mode robot-mode fundamental-mode
  "robot mode"
  "Major mode for editing Robot Framework text files."
  (set (make-local-variable 'font-lock-defaults) '(robot-mode-keywords))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#")
  (define-key robot-mode-map [remap find-tag] 'robot-mode-find-kw)
)
