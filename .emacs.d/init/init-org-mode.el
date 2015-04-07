(message "*** Org-mode")

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(require 'org-clock)
(require 'org-element)
(require 'org-table)

(setq
  org-support-shift-select	nil	; no shift-select, org keys are better

  ;;;; org-agenda
  ;;org-agenda-jump-prefer-future			org:2646
  org-agenda-files	"~/org/agenda-files"	; where to store the list
  org-agenda-skip-unavailable-files	t	; skip, don't remove

  ;;;; org-agenda-custom-commands
  org-agenda-custom-commands
  '(("w" "Work TODO" tags-todo "work"
     ((org-agenda-sorting-strategy '(priority-down))))
    ("W" "Work Agenda" agenda "+work"
     ((org-agenda-skip-function 
        '(org-agenda-skip-entry-if 'notregexp ":work:"))))
    )

  ;;;; org-agenda-daily
  org-agenda-skip-scheduled-if-done	t	; 
  org-agenda-skip-deadline-if-done	t	;
  org-agenda-dim-blocked-tasks		t	; semihide todos that can't be done yet
  org-agenda-start-on-weekday	1	; the only sane choice
  org-agenda-include-diary	t	; probably nice

  ;;;; org-agenda-line-format
  org-agenda-tags-column -79
  org-agenda-fontify-priorities 	; remove the annoying underline
  '((?A (:bold t)))

  ;;;; org-agenda-skip
  org-timeline-show-empty-dates nil	; drop dates without entries

  ;;;; org-agenda-sorting
  org-agenda-sorting-strategy		; select how agenda views sort
  '((agenda time-up priority-down)
    (todo category-keep priority-down)
    (tags category-keep priority-down)
    (search category-keep))
  org-sort-agenda-notime-is-late t	; undated items last

  ;;;; org-appearance
  org-level-color-stars-only		nil	; colorize whole line
  org-fontify-done-headline		t
  org-fontify-emphasized-text		t	; fontify ascii emphasis
  org-hide-emphasis-markers		nil	; .. and hide the markers
  org-src-fontify-natively	t	; fontify code-block

  ;;;; org-archive
  org-archive-location 	"%s_archive::"	; where to archive
  org-agenda-skip-archived-trees	t
  org-archive-mark-done t		; mark archived as done
  org-archive-stamp-time t		; obsolete from 
  org-archive-save-context-info
  '(time file olpath category todo itags ltags)

  ;;;; org-babel
  org-confirm-babel-evaluate				;;ob:109
  '(lambda (lang body)
     (assoc lang org-confirm-babel-evaluate-auto-yes))

  org-confirm-babel-evaluate-auto-yes
  '(sh perl sqlite)

  org-babel-load-languages				;;org:148
  '((emacs-lisp . t)
    (sh . t)
    (perl . t)
    (sqlite . t)
    (css . t)
    (ditaa . t)
    (latex . t)
    (ledger . t)
    )
  org-babel-results-keyword		"results"

  ;;;; org-clock
  org-clock-into-drawer	"CLOCK"	; always log into CLOCK
  org-clock-out-when-done t		; when its done its done
  org-clock-in-switch-to-state nil	; force a state change?

  ;;;; org-cycle
  org-cycle-emulate-tab	t	; cycle visibility only in headline
  org-cycle-separator-lines	0	; need only one
  org-cycle-open-archived-trees	nil

  ;;;; org-edit-structure
  org-adapt-indentation	t	; use indentation
  org-special-ctrl-a/e		t	; skip stars and tags on move
  org-special-ctrl-k		t	; and ignore tags on kill
  org-ctrl-k-protect-subtree	nil	; kill whole trees
  org-yank-folded-subtrees	t	; yank them folded
  org-yank-adjusted-subtrees	nil	; yank as is
  org-blank-before-new-entry		; where to add an empty line
  '((heading . auto)
    (plain-list-item . nil))
  org-enable-fixed-width-editor	t	; eneble : for fixed width
  org-src-preserve-indentation		nil	; indent!
  org-edit-src-content-indentation	2	; 2 is good
  org-edit-src-persistent-message	t	; nice help

  ;;;; org-export-general
  org-export-run-in-background nil
  org-export-html-expand t
  org-export-default-language "en"
  org-export-skip-text-before-1st-heading nil
  org-export-headline-levels 3
  org-export-with-section-numbers t
  org-export-with-toc t
  org-export-mark-todo-in-toc t
  org-export-preserve-breaks nil
  org-export-with-archived-trees 'headline
  org-export-author-info t
  org-export-time-stamp-file t
  org-export-with-timestamps t
  org-export-remove-timestamps-from-toc t
  org-export-with-tags 'not-in-toc
  org-export-with-drawers nil

  ;;;; org-export-html
  org-export-html-link-org-files-as-html t
  org-export-html-inline-images 'maybe
  org-export-html-with-timestamp nil

  ;;;; org-export-latex
  org-export-latex-date-format "%Y-%m-%d"

  ;;;; org-export-tables
  org-export-with-tables t
  org-export-highlight-first-table-line t
  org-export-table-remove-special-lines t

  ;;;; org-export-translation
  org-export-with-special-strings t
  org-export-with-emphasize t
  org-export-with-footnotes t
  org-export-with-TeX-macros t
  org-export-with-LaTeX-fragments nil
  org-export-with-fixed-width t

  ;;;; org-footnote
  org-footnote-define-inline	t	; use inline footnotes
  org-footnote-auto-adjust	t	; renumer and reorder

  ;;;; org-link
  org-link-abbrev-alist			; link shorthand
  '(("gc" . "http://coord.info/GC%s"))
  org-descriptive-links	t		; hide link if desc
  org-link-file-path-type	'adaptive	; relative in sub, abs other

  ;;;; org-link-follow
  org-tab-follows-link		nil		; dont want that
  org-return-follows-link	t		; follow links on RET
  org-link-frame-setup			; how to follow links
  '((vm   . vm-visit-folder)
    (gnus . org-gnus-no-new-news)
    (file . find-file)
    (wl   . wl))
  org-open-non-existing-files	t	; open nonexisting files

  ;;;; org-link-store
  org-link-to-org-use-id
  'create-if-interactive-and-no-custom-id
  org-context-in-file-links	t		; store context too
  org-keep-stored-link-after-insertion nil	; remove used links

  ;;;; org-plain-lists
  org-list-ending-method			'both	; indent and regexp
  org-empty-line-terminates-plain-lists	nil

  ;;;; org-priorities
  org-highest-priority		?A
  org-lowest-priority		?E
  org-default-priority		?C
  org-priority-start-cycle-with-default nil	; don't start cycle with default

  ;;;; org-progress
  org-log-done			'time	; add CLOSED on completed todos
  org-log-note-clock-out	t	; auto-add add a note
  org-log-done-with-time	t	; both date and time
  org-log-states-order-reversed t	; newest on top
  org-log-repeat		'time	; record at least time on repeats

  ;;;; org-refile
  org-directory		"~/org"		; where to put org-files
  org-default-notes-file	"~/org/notes"	; notes-file
  org-reverse-note-order	nil

  ;;;; org-startup
  org-startup-folded		nil	; show all headers
  org-startup-truncated	t	; turn off linewrap
  org-use-sub-superscripts	t	; 
  org-startup-align-all-tables	nil	; faster
  org-startup-with-inline-images nil	;
  org-insert-mode-line-in-empty-file t	; add -*- line
  org-replace-disputed-keys	t	; the S-bindings isn't any good

  ;;;; org-structure
  org-M-RET-may-split-line	nil	; dont split lines

  ;;;; org-table
  org-enable-table-editor	'optimized

  ;;;; org-table-calculation
  org-table-use-standard-references t	; B3 instead of @3$2
  org-table-copy-increment	t	; increment on copy-down

  ;;;; org-table-editing
  org-table-automatic-realign	t	; relign tables while editing
  org-table-auto-blank-field	t	; clear cell content on edit
  org-table-tab-jumps-over-hlines nil	; insert rows before hline

  ;;;; org-tags
  org-tags-column			-79	; looks better than -80
  org-auto-align-tags			t	; do alignment
  org-use-tag-inheritance		t
  org-tags-match-list-sublevels	nil

  ;;;; org-time
  org-insert-labeled-timestamps-at-point nil	; use next line
  org-time-stamp-rounding-minutes '(0 1)	; don't round minutes
  org-display-custom-times		nil	; use the default iso-dates
  org-time-clocksum-use-fractional	t
  org-deadline-warning-days		14
  org-read-date-prefer-future		t	; prefer future for incomplete
  org-read-date-display-live		t	; show interpreted date
  org-read-date-popup-calendar		t
  org-edit-timestamp-down-means-later	nil

  ;;;; org-todo
  org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(i@)" "|" "DONE(f@)" "DELEGATED(d@)" "CANCELLED(c@)"))

  org-use-fast-todo-selection	'prefix
  org-enforce-todo-dependencies	t	; don't let marking a parent done befor childs
  org-enforce-todo-checkbox-dependencies	t	; require all CBs checked
  org-log-done		'time		; time is enough for defaults
  org-log-into-drawer			t	; hide away logs
  org-track-ordered-property-with-tag	t	; a bit nicer
  org-todo-keyword-faces
  '(("TODO" . org-warning)
    ("DONE" . (:foreground "green" :wigth bold))
    ("DELEGATED" . (:foreground "yellow" :wigth bold))
    ("CANCELLED" . (:foreground "red" :wigth bold))
    )   
  ;;;org-priority-faces

  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(setq org-plantuml-jar-path
      (setq plantuml-jar-path (expand-file-name "./plantuml.jar")))


(add-to-list 'org-latex-packages-alist
             "\\hypersetup{colorlinks,breaklinks,urlcolor=blue,linkcolor=blue}"
             )

(eval-after-load "org-src"
                 '(setq org-src-lang-modes			; mapping name to mode
                        (append '(("perl" . cperl))
                                org-src-lang-modes)))

;;;; ---------------------------------

(setq

  ;;; remember
  org-remember-store-without-prompt t		; store without promt
  org-remember-templates			; templates for remember
  '(("Todo" ?t "* TODO %?\n  SCHEDULED: %t\n  %i\n  %a" "~/org/todo.org")
    ("Work" ?w "* TODO %?\t:work:\n  SCHEDULED: %t\n  %i\n" "~/org/todo.org")
    )

  )

;; since we only want to change some of the properties
(eval-after-load "org"
                 '(progn
                    (setq org-format-latex-options	; change colors for overlay
                          (plist-put
                            (plist-put org-format-latex-options :foreground "White")
                            :background "Black"))
                    (define-key org-mode-map "\C-c\C-g" 'ps:org-make-gc-header)))

(provide 'init-org-mode)
