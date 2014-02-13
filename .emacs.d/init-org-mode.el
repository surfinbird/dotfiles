(message "*** Org-mode")

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(require 'org-clock)
(require 'org-element)
(require 'org-table)

(setq
  ;;;; org
  ;;org-mode-hook					org:238
  ;;org-load-hook					org:243
  ;;org-modules					org:281
  org-support-shift-select	nil	; no shift-select, org keys are better

  ;;;; org-agenda
  ;;org-agenda-jump-prefer-future			org:2646
  org-agenda-files	"~/org/agenda-files"	; where to store the list
  ;;org-agenda-file-regexp				org:3068
  ;;org-agenda-text-search-extra-files			org:3082
  org-agenda-skip-unavailable-files	t	; skip, don't remove
  ;;org-calendar-to-agenda-key				org:3102
  ;;org-calendar-agenda-action-key			org:3109
  ;;org-calendar-insert-diary-entry-key		org:3120
  ;;org-agenda-diary-file				org:3128
  ;;org-effort-durations				org:15543
  ;;org-agenda-confirm-kill				org-agenda:84
  ;;org-agenda-compact-blocks				org-agenda:93
  ;;org-agenda-block-separator				org-agenda:100
  ;;org-agenda-add-entry-text-maxlines			org-agenda:148
  ;;org-agenda-add-entry-text-descriptive-links	org-agenda:156
  ;;org-agenda-persistent-filter			org-agenda:203
  ;;org-agenda-menu-show-matcher			org-agenda:825
  ;;org-agenda-menu-two-column				org-agenda:832
  ;;org-agenda-entry-text-maxlines			org-agenda:868
  ;;org-agenda-entry-text-exclude-regexps		org-agenda:875
  ;;org-agenda-timegrid-use-ampm			org-agenda:1006
  ;;org-agenda-columns-remove-prefix-from-item		org-agenda:1625
  ;;org-agenda-auto-exclude-function			org-agenda:1658
  ;;org-agenda-insert-diary-strategy			org-agenda:7605
  ;;org-agenda-insert-diary-extract-time		org-agenda:7612

  ;;;; org-agenda-column-view
  ;;org-agenda-columns-show-summaries			org-agenda:1615
  ;;org-agenda-columns-compute-summary-properties	org-agenda:1636
  ;;org-agenda-columns-add-appointments-to-effort-sum	org-agenda:1646

  ;;;; org-agenda-custom-commands
  org-agenda-custom-commands
  '(("w" "Work TODO" tags-todo "work"
     ((org-agenda-sorting-strategy '(priority-down))))
    ("W" "Work Agenda" agenda "+work"
     ((org-agenda-skip-function 
        '(org-agenda-skip-entry-if 'notregexp ":work:"))))
    )
  ;;org-agenda-query-register				org-agenda:468
  ;;org-stuck-projects					org-agenda:499
  ;;org-agenda-filter-effort-default-operator		org-agenda:510

  ;;;; org-agenda-daily
  ;;org-deadline-warning-days				org:2609
  org-agenda-skip-scheduled-if-done	t	; 
  ;;org-agenda-skip-scheduled-if-deadline-is-shown	org-agenda:733
  org-agenda-skip-deadline-if-done	t	;
  ;;org-agenda-skip-deadline-prewarning-if-scheduled	org-agenda:761
  ;;org-agenda-skip-timestamp-if-done			org-agenda:777
  org-agenda-dim-blocked-tasks		t	; semihide todos that can't be done yet
  ;;org-agenda-ndays					org-agenda:933
  ;;org-agenda-span					org-agenda:942
  org-agenda-start-on-weekday	1	; the only sane choice
  ;;org-agenda-show-all-dates				org-agenda:961
  ;;org-agenda-format-date				org-agenda:970
  ;;org-agenda-time-leading-zero			org-agenda:1001
  ;;org-agenda-weekend-days				org-agenda:1037
  org-agenda-include-diary	t	; probably nice
  ;;org-agenda-include-deadlines			org-agenda:1056
  ;;org-agenda-include-all-todo			org-agenda:1064
  ;;org-agenda-repeating-timestamp-show-all		org-agenda:1072
  ;;org-scheduled-past-days				org-agenda:1084
  ;;org-agenda-log-mode-items				org-agenda:1096
  ;;org-agenda-log-mode-add-notes			org-agenda:1104
  ;;org-agenda-start-with-log-mode			org-agenda:1110
  ;;org-agenda-start-with-clockreport-mode		org-agenda:1116
  ;;org-agenda-clockreport-parameter-plist		org-agenda:1127
  ;;org-agenda-deadline-faces				org-faces:655

  ;;;; org-agenda-export
  ;;org-agenda-with-colors				org-agenda:112
  ;;org-agenda-exporter-settings			org-agenda:124
  ;;org-agenda-before-write-hook			org-agenda:133
  ;;org-agenda-export-html-style			org-agenda:197

  ;;;; org-agenda-line-format
  ;;org-agenda-prefix-format				org-agenda:1405
  ;;org-agenda-todo-keyword-format			org-agenda:1415
  ;;org-agenda-timerange-leaders			org-agenda:1425
  ;;org-agenda-scheduled-leaders			org-agenda:1440
  ;;org-agenda-inactive-leader				org-agenda:1448
  ;;org-agenda-deadline-leaders			org-agenda:1459
  ;;org-agenda-remove-times-when-in-prefix		org-agenda:1477
  ;;org-agenda-remove-timeranges-from-blocks		org-agenda:1486
  ;;org-agenda-default-appointment-duration		org-agenda:1493
  ;;org-agenda-show-inherited-tags			org-agenda:1500
  ;;org-agenda-hide-tags-regexp			org-agenda:1508
  ;;org-agenda-remove-tags				org-agenda:1517
  org-agenda-tags-column -79
  org-agenda-fontify-priorities 	; remove the annoying underline
  '((?A (:bold t)))
  ;;org-agenda-day-face-function			org-agenda:1567
  ;;org-agenda-category-icon-alist			org-agenda:1599

  ;;;; org-agenda-match-view
  ;;org-agenda-tags-todo-honor-ignore-options		org-agenda:709

  ;;;; org-agenda-search-view
  ;;org-agenda-search-view-always-boolean		org-agenda:1154
  ;;org-agenda-search-view-force-full-words		org-agenda:1164

  ;;;; org-agenda-skip
  ;;org-agenda-skip-archived-trees			org:3979
  ;;org-agenda-skip-function-global			org-agenda:533
  ;;org-agenda-skip-comment-trees			org-agenda:563
  ;;org-agenda-todo-list-sublevels			org-agenda:570
  ;;org-agenda-todo-ignore-with-date			org-agenda:582
  ;;org-agenda-todo-ignore-timestamp			org-agenda:613
  ;;org-agenda-todo-ignore-scheduled			org-agenda:645
  ;;org-agenda-todo-ignore-deadlines			org-agenda:686
  ;;org-agenda-tags-todo-honor-ignore-options		org-agenda:707
  ;;org-agenda-skip-scheduled-if-done			org-agenda:718
  ;;org-agenda-skip-scheduled-if-deadline-is-shown	org-agenda:732
  ;;org-agenda-skip-deadline-if-done			org-agenda:745
  ;;org-agenda-skip-deadline-prewarning-if-scheduled	org-agenda:760
  ;;org-agenda-skip-additional-timestamps-same-entry	org-agenda:771
  ;;org-agenda-skip-timestamp-if-done			org-agenda:776
  org-timeline-show-empty-dates nil	; drop dates without entries

  ;;;; org-agenda-sorting
  org-agenda-sorting-strategy		; select how agenda views sort
  '((agenda time-up priority-down)
    (todo category-keep priority-down)
    (tags category-keep priority-down)
    (search category-keep))
  ;;org-agenda-cmp-user-defined			org-agenda:1310
  org-sort-agenda-notime-is-late t	; undated items last
  ;;org-sort-agenda-noeffort-is-high			org-agenda:1329

  ;;;; org-agenda-startup
  ;;org-finalize-agenda-hook				org-agenda:837
  ;;org-agenda-mouse-1-follows-link			org-agenda:844
  ;;org-agenda-start-with-follow-mode			org-agenda:849
  ;;org-agenda-show-outline-path			org-agenda:854
  ;;org-agenda-start-with-entry-text-mode		org-agenda:859
  ;;org-agenda-start-with-log-mode			org-agenda:1109
  ;;org-agenda-start-with-clockreport-mode		org-agenda:1115

  ;;;; org-agenda-time-grid
  ;;org-agenda-search-headline-for-time		org-agenda:1180
  ;;org-agenda-use-time-grid				org-agenda:1190
  ;;org-agenda-time-grid				org-agenda:1211
  ;;org-agenda-show-current-time-in-grid		org-agenda:1227
  ;;org-agenda-current-time-string			org-agenda:1233

  ;;;; org-agenda-todo-list
  ;;org-agenda-todo-list-sublevels			org-agenda:571
  ;;org-agenda-todo-ignore-with-date			org-agenda:583
  ;;org-agenda-todo-ignore-timestamp			org-agenda:614
  ;;org-agenda-todo-ignore-scheduled			org-agenda:646
  ;;org-agenda-todo-ignore-deadlines			org-agenda:687
  ;;org-agenda-tags-todo-honor-ignore-options		org-agenda:708
  ;;org-agenda-dim-blocked-tasks			org-agenda:796

  ;;;; org-agenda-windows
  ;;org-indirect-buffer-display			org:748
  ;;org-agenda-window-setup				org-agenda:905
  ;;org-agenda-window-frame-fractions			org-agenda:916
  ;;org-agenda-restore-windows-after-quit		org-agenda:926

  ;;;; org-appearance
  ;;org-odd-levels-only				org:943
  org-level-color-stars-only		nil	; colorize whole line
  ;;org-hide-leading-stars				org:3330
  ;;org-hidden-keywords				org:3337
  org-fontify-done-headline		t
  org-fontify-emphasized-text		t	; fontify ascii emphasis
  ;;org-fontify-whole-heading-line			org:3361
  ;;org-highlight-latex-fragments-and-specials		org:3366
  org-hide-emphasis-markers		nil	; .. and hide the markers
  ;;org-pretty-entities				org:3377
  ;;org-pretty-entities-include-sub-superscripts	org:3382
  ;;org-emphasis-regexp-components			org:3471
  ;;org-emphasis-alist					org:3498
  org-src-fontify-natively	t	; fontify code-block
  ;;org-cycle-level-faces				org-faces:706

  ;;;; org-archive
  org-archive-location 	"%s_archive::"	; where to archive
  ;;org-archive-tag					org:3969
  org-agenda-skip-archived-trees	t
  ;;org-columns-skip-archived-trees			org:3984
  ;;org-cycle-open-archived-trees			org:3993
  ;;org-sparse-tree-open-archived-trees		org:4001
  ;;org-archive-default-command			org-archive:39
  ;;org-archive-reversed-order				org-archive:47
  ;;org-archive-sibling-heading			org-archive:54
  org-archive-mark-done t		; mark archived as done
  org-archive-stamp-time t		; obsolete from 
  ;;org-archive-subtree-add-inherited-tags		org-archive:76
  org-archive-save-context-info
  '(time file olpath category todo itags ltags)
  ;;org-export-with-archived-trees			org-exp:321

  ;;;; org-attach
  ;;org-attach-directory				org-attach:55
  ;;org-attach-auto-tag				org-attach:60
  ;;org-attach-file-list-property			org-attach:70
  ;;org-attach-method					org-attach:83
  ;;org-attach-expert					org-attach:91
  ;;org-attach-allow-inheritance			org-attach:96
  ;;org-attach-store-link-p				org-attach:104

  ;;;; org-babel
  org-confirm-babel-evaluate				;;ob:109
  '(lambda (lang body)
     (assoc lang org-confirm-babel-evaluate-auto-yes))

  org-confirm-babel-evaluate-auto-yes
  '(sh perl sqlite)

  ;;org-babel-no-eval-on-ctrl-c-ctrl-c			ob:116
  ;;org-export-babel-evaluate				ob-exp:53
  ;;org-babel-lob-files				ob-lob:43
  ;;org-babel-post-tangle-hook				ob-tangle:58
  ;;org-babel-pre-tangle-hook				ob-tangle:63
  ;;org-babel-tangle-pad-newline			ob-tangle:68
  ;;org-babel-tangle-comment-format-beg		ob-tangle:82
  ;;org-babel-tangle-comment-format-end		ob-tangle:96
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

  ;;org-src-fontify-natively				org:5116
  ;;org-src-tab-acts-natively				org-src:745

  ;;;; org-babel-tangle
  ;;org-babel-tangle-lang-exts				ob-tangle:50

  ;;;; org-bbdb-anniversaries
  ;;org-bbdb-default-anniversary-format		org-bbdb:132
  ;;org-bbdb-anniversary-format-alist			org-bbdb:160
  ;;org-bbdb-anniversary-field				org-bbdb:177
  ;;org-bbdb-extract-date-fun				org-bbdb:187

  ;;;; org-beamer
  ;;org-beamer-use-parts				org-beamer:46
  ;;org-beamer-frame-level				org-beamer:55
  ;;org-beamer-frame-default-options			org-beamer:63
  ;;org-beamer-column-view-format			org-beamer:69
  ;;org-beamer-themes					org-beamer:79
  ;;org-beamer-environments-extra			org-beamer:144
  ;;org-beamer-fragile-re				org-beamer:404
  ;;org-beamer-outline-frame-title			org-beamer:513
  ;;org-beamer-outline-frame-options			org-beamer:521

  ;;;; org-clock
  org-clock-into-drawer	"CLOCK"	; always log into CLOCK
  org-clock-out-when-done t		; when its done its done
  ;;org-clock-out-remove-zero-time-clocks		org-clock:83
  org-clock-in-switch-to-state nil	; force a state change?
  ;;org-clock-out-switch-to-state			org-clock:105
  ;;org-clock-history-length				org-clock:114
  ;;org-clock-goto-may-find-recent-task		org-clock:119
  ;;org-clock-heading-function				org-clock:126
  ;;org-clock-string-limit				org-clock:131
  ;;org-clock-in-resume				org-clock:138
  ;;org-clock-persist					org-clock:152
  ;;org-clock-persist-file				org-clock:162
  ;;org-clock-persist-query-save			org-clock:167
  ;;org-clock-persist-query-resume			org-clock:172
  ;;org-clock-sound					org-clock:182
  ;;org-clock-modeline-total				org-clock:198
  ;;org-task-overrun-text				org-clock:212
  ;;org-show-notification-handler			org-clock:221
  ;;org-clocktable-defaults				org-clock:253
  ;;org-clock-idle-time				org-clock:280
  ;;org-clock-auto-clock-resolution			org-clock:287
  ;;org-clock-report-include-clocking-task		org-clock:295
  ;;org-clock-resolve-expert				org-clock:300

  ;;;; org-clocktable
  ;;org-clock-clocktable-formatter			org-clock:259
  ;;org-clock-clocktable-language-setup		org-clock:268
  ;;org-clock-clocktable-default-properties		org-clock:275

  ;;;; org-completion
  ;;org-completion-use-ido				org:3528
  ;;org-completion-use-iswitchb			org:3537
  ;;org-completion-fallback-command			org:3543
  ;;org-structure-template-alist			org:10734

  ;;;; org-cycle
  ;;org-cycle-skip-children-state-if-no-children	org:794
  ;;org-cycle-max-level				org:807
  ;;org-drawers					org:826
  ;;org-hide-block-startup				org:836
  ;;org-cycle-global-at-bob				org:849
  ;;org-cycle-level-after-item/entry-creation		org:864
  org-cycle-emulate-tab	t	; cycle visibility only in headline
  org-cycle-separator-lines	0	; need only one
  ;;org-pre-cycle-hook					org:909
  ;;org-cycle-hook					org:922
  org-cycle-open-archived-trees	nil

  ;;;; org-edit-structure
  ;;org-odd-levels-only				org:942
  org-adapt-indentation	t	; use indentation
  org-special-ctrl-a/e		t	; skip stars and tags on move
  org-special-ctrl-k		t	; and ignore tags on kill
  org-ctrl-k-protect-subtree	nil	; kill whole trees
  org-yank-folded-subtrees	t	; yank them folded
  org-yank-adjusted-subtrees	nil	; yank as is
  org-blank-before-new-entry		; where to add an empty line
  '((heading . auto)
    (plain-list-item . nil))
  ;;org-insert-heading-hook				org:1108
  org-enable-fixed-width-editor	t	; eneble : for fixed width
  ;;org-goto-auto-isearch				org:1121
  ;;org-edit-src-region-extra				org-src:53
  ;;org-coderef-label-format				org-src:78
  ;;org-edit-fixed-width-region-mode			org-src:84
  org-src-preserve-indentation		nil	; indent!
  org-edit-src-content-indentation	2	; 2 is good
  org-edit-src-persistent-message	t	; nice help
  ;;org-src-window-setup				org-src:135

  ;;;; org-entities
  ;;org-entities-ascii-explanatory			org-entities:47
  ;;org-entities-user					org-entities:71

  ;;;; org-export-general
  ;;org-export-allow-BIND				org-exp:66
  ;;org-export-show-temporary-export-buffer		org-exp:81
  ;;org-export-copy-to-kill-ring			org-exp:86
  ;;org-export-kill-product-buffer-when-displayed	org-exp:93
  org-export-run-in-background nil
  ;;org-export-initial-scope				org-exp:115
  ;;org-export-select-tags				org-exp:126
  ;;org-export-exclude-tags				org-exp:134
  org-export-html-expand t
  ;;org-export-html-link-up				org-exp:166
  ;;org-export-html-link-home				org-exp:172
  ;;org-export-language-setup				org-exp:197
  org-export-default-language "en"
  org-export-skip-text-before-1st-heading nil
  org-export-headline-levels 3
  org-export-with-section-numbers t
  ;;org-export-section-number-format			org-exp:258
  org-export-with-toc t
  org-export-mark-todo-in-toc t
  ;;org-export-with-todo-keywords			org-exp:296
  ;;org-export-with-priority				org-exp:302
  org-export-preserve-breaks nil
  org-export-with-archived-trees 'headline
  org-export-author-info t
  ;;org-export-email-info				org-exp:340
  ;;org-export-creator-info				org-exp:346
  org-export-time-stamp-file t
  org-export-with-timestamps t
  org-export-remove-timestamps-from-toc t
  org-export-with-tags 'not-in-toc
  org-export-with-drawers nil
  ;;org-export-blocks					org-exp-blocks:106
  ;;org-export-interblocks				org-exp-blocks:135
  ;;org-export-blocks-witheld				org-exp-blocks:141
  ;;org-export-blocks-postblock-hook			org-exp-blocks:146

  ;;;; org-export-html
  ;;org-agenda-export-html-style			org-agenda:198
  ;;org-export-html-expand				org-exp:144
  ;;org-export-html-link-up				org-exp:165
  ;;org-export-html-link-home				org-exp:171
  ;;org-export-html-footnotes-section			org-html:54
  ;;org-export-html-footnote-format			org-html:60
  ;;org-export-html-coding-system			org-html:65
  ;;org-export-html-extension				org-html:70
  ;;org-export-html-xml-declaration			org-html:80
  ;;org-export-html-style-include-scripts		org-html:91
  ;;org-export-html-style-include-default		org-html:174
  ;;org-export-html-style				org-html:206
  ;;org-export-html-style-extra			org-html:217
  ;;org-export-html-mathjax-options			org-html:242
  ;;org-export-html-mathjax-template			org-html:325
  ;;org-export-html-tag-class-prefix			org-html:334
  ;;org-export-html-todo-kwd-class-prefix		org-html:343
  ;;org-export-html-preamble				org-html:351
  ;;org-export-html-preamble-format			org-html:362
  ;;org-export-html-postamble				org-html:370
  ;;org-export-html-postamble-format			org-html:389
  ;;org-export-html-home/up-format			org-html:403
  ;;org-export-html-toplevel-hlevel			org-html:414
  org-export-html-link-org-files-as-html t
  org-export-html-inline-images 'maybe
  ;;org-export-html-inline-image-extensions		org-html:443
  ;;org-export-html-table-tag				org-html:451
  ;;org-export-html-validation-link			org-html:517
  org-export-html-with-timestamp nil
  ;;org-export-html-html-helper-timestamp		org-html:531

  ;;;; org-export-htmlize
  ;;org-export-htmlize-output-type			org-html:564
  ;;org-export-htmlize-css-font-prefix			org-html:569
  ;;org-export-htmlized-org-css-url			org-html:581

  ;;;; org-export-icalendar
  ;;org-combined-agenda-icalendar-file			org-icalendar:47
  ;;org-icalendar-alarm-time				org-icalendar:58
  ;;org-icalendar-combined-name			org-icalendar:63
  ;;org-icalendar-combined-description			org-icalendar:68
  ;;org-icalendar-use-plain-timestamp			org-icalendar:73
  ;;org-icalendar-honor-noexport-tag			org-icalendar:78
  ;;org-icalendar-use-deadline				org-icalendar:88
  ;;org-icalendar-use-scheduled			org-icalendar:106
  ;;org-icalendar-categories				org-icalendar:124
  ;;org-icalendar-include-todo				org-icalendar:139
  ;;org-icalendar-include-bbdb-anniversaries		org-icalendar:160
  ;;org-icalendar-include-sexps			org-icalendar:166
  ;;org-icalendar-include-body				org-icalendar:174
  ;;org-icalendar-store-UID				org-icalendar:191
  ;;org-icalendar-timezone				org-icalendar:197
  ;;org-icalendar-date-time-format			org-icalendar:217

  ;;;; org-export-latex
  ;;org-export-latex-default-packages-alist		org:3270
  ;;org-export-latex-packages-alist			org:3292
  ;;org-export-with-TeX-macros				org-exp:480
  ;;org-export-with-LaTeX-fragments			org-exp:504
  ;;org-export-latex-default-class			org-latex:88
  ;;org-export-latex-classes				org-latex:196
  ;;org-export-latex-inputenc-alist			org-latex:220
  ;;org-export-latex-emphasis-alist			org-latex:243
  ;;org-export-latex-title-command			org-latex:251
  ;;org-export-latex-import-inbuffer-stuff		org-latex:257
  org-export-latex-date-format "%Y-%m-%d"
  ;;org-export-latex-todo-keyword-markup		org-latex:272
  ;;org-export-latex-tag-markup			org-latex:285
  ;;org-export-latex-timestamp-markup			org-latex:290
  ;;org-export-latex-timestamp-keyword-markup		org-latex:295
  ;;org-export-latex-href-format			org-latex:302
  ;;org-export-latex-hyperref-format			org-latex:309
  ;;org-export-latex-tables-verbatim			org-latex:314
  ;;org-export-latex-tables-centered			org-latex:319
  ;;org-export-latex-tables-column-borders		org-latex:325
  ;;org-export-latex-low-levels			org-latex:343
  ;;org-export-latex-list-parameters			org-latex:361
  ;;org-export-latex-verbatim-wrap			org-latex:372
  ;;org-export-latex-listings				org-latex:405
  ;;org-export-latex-listings-langs			org-latex:425
  ;;org-export-latex-listings-w-names			org-latex:436
  ;;org-export-latex-minted-langs			org-latex:457
  ;;org-export-latex-listings-options			org-latex:480
  ;;org-export-latex-minted-options			org-latex:503
  ;;org-export-latex-remove-from-headlines		org-latex:535
  ;;org-export-latex-image-default-option		org-latex:539
  ;;org-latex-default-figure-position			org-latex:544
  ;;org-export-latex-tabular-environment		org-latex:549
  ;;org-export-latex-inline-image-extensions		org-latex:559
  ;;org-export-latex-coding-system			org-latex:564

  ;;;; org-export-pdf
  ;;org-latex-to-pdf-process				org-latex:600
  ;;org-export-pdf-logfiles				org-latex:625
  ;;org-export-pdf-remove-logfiles			org-latex:631

  ;;;; org-export-tables
  org-export-with-tables t
  org-export-highlight-first-table-line t
  org-export-table-remove-special-lines t
  ;;org-export-table-remove-empty-lines		org-exp:567
  ;;org-export-prefer-native-exporter-for-tables	org-exp:579
  ;;org-export-table-header-tags			org-html:461
  ;;org-export-table-data-tags				org-html:470
  ;;org-export-table-row-tags				org-html:491
  ;;org-export-html-table-align-individual-fields	org-html:505
  ;;org-export-html-table-use-header-tags-for-first-columnorg-html:511

  ;;;; org-export-translation
  ;;org-use-sub-superscripts				org:453
  ;;org-match-sexp-depth				org:4876
  org-export-with-special-strings t
  org-export-with-emphasize t
  org-export-with-footnotes t
  org-export-with-TeX-macros t
  org-export-with-LaTeX-fragments nil
  org-export-with-fixed-width t
  ;;org-export-latex-verbatim-wrap			org-latex:371

  ;;;; org-faces
  ;;org-faces-easy-properties				org-faces:349
  ;;org-todo-keyword-faces				org-faces:364
  ;;org-priority-faces					org-faces:381
  ;;org-tag-faces					org-faces:421
  ;;org-fontify-quote-and-verse-blocks			org-faces:546
  ;;org-agenda-deadline-faces				org-faces:654
  ;;org-n-level-faces					org-faces:699
  ;;org-cycle-level-faces				org-faces:707

  ;;;; org-footnote
  ;;org-footnote-section				org-footnote:84
  ;;org-footnote-tag-for-non-org-mode-files		org-footnote:96
  org-footnote-define-inline	t	; use inline footnotes
  ;;org-footnote-auto-label				org-footnote:117
  org-footnote-auto-adjust	t	; renumer and reorder
  ;;org-footnote-fill-after-inline-note-extraction	org-footnote:146

  ;;;; org-id
  ;;org-clone-delete-id				org:191
  ;;org-id-uuid-program				org-id:87
  ;;org-id-method					org-id:105
  ;;org-id-prefix					org-id:116
  ;;org-id-include-domain				org-id:130
  ;;org-id-track-globally				org-id:143
  ;;org-id-locations-file				org-id:150
  ;;org-id-extra-files					org-id:169
  ;;org-id-search-archives				org-id:181

  ;;;; org-imenu-and-speedbar
  ;;org-imenu-depth					org:1162

  ;;;; org-indent
  ;;org-indent-boundary-char				org-indent:69
  ;;org-indent-mode-turns-off-org-adapt-indentation	org-indent:79
  ;;org-indent-mode-turns-on-hiding-stars		org-indent:85
  ;;org-indent-indentation-per-level			org-indent:90
  ;;org-indent-fix-section-after-idle-time		org-indent:100

  ;;;; org-keywords
  ;;org-deadline-string				org:602
  ;;org-scheduled-string				org:611
  ;;org-closed-string					org:616
  ;;org-clock-string					org:621
  ;;org-comment-string					org:629
  ;;org-quote-string					org:638
  ;;org-todo-keywords					org:2016
  ;;org-todo-interpretation				org:2072
  ;;org-archive-tag					org:3970

  ;;;; org-latex
  ;;org-format-latex-options				org:3172
  ;;org-format-latex-signal-error			org:3178
  ;;org-format-latex-header				org:3207

  ;;;; org-link
  org-link-abbrev-alist			; link shorthand
  '(("gc" . "http://coord.info/GC%s"))
  org-descriptive-links	t		; hide link if desc
  org-link-file-path-type	'adaptive	; relative in sub, abs other
  ;;org-activate-links					org:1295
  ;;org-make-link-description-function			org:1311

  ;;;; org-link-follow
  ;;org-link-translation-function			org:1434
  ;;org-follow-link-hook				org:1439
  org-tab-follows-link		nil		; dont want that
  org-return-follows-link	t		; follow links on RET
  ;;org-mouse-1-follows-link				org:1460
  ;;org-mark-ring-length				org:1466
  ;;org-link-search-must-match-exact-headline		org:1473
  org-link-frame-setup			; how to follow links
  '((vm   . vm-visit-folder)
    (gnus . org-gnus-no-new-news)
    (file . find-file)
    (wl   . wl))
  ;;org-display-internal-link-with-indirect-buffer	org:1538
  org-open-non-existing-files	t	; open nonexisting files
  ;;org-open-directory-means-index-dot-org		org:1554
  ;;org-link-mailto-program				org:1563
  ;;org-confirm-shell-link-function			org:1581
  ;;org-confirm-shell-link-not-regexp			org:1592
  ;;org-confirm-elisp-link-function			org:1606
  ;;org-confirm-elisp-link-not-regexp			org:1617
  ;;org-file-apps					org:1736

  ;;;; org-link-store
  ;;org-email-link-description-format			org:1337
  ;;org-from-is-user-regexp				org:1349
  org-link-to-org-use-id
  'create-if-interactive-and-no-custom-id
  org-context-in-file-links	t		; store context too
  org-keep-stored-link-after-insertion nil	; remove used links

  ;;;; org-mobile
  ;;org-mobile-files					org-mobile:58
  ;;org-mobile-files-exclude-regexp			org-mobile:68
  ;;org-mobile-directory				org-mobile:73
  ;;org-mobile-use-encryption				org-mobile:84
  ;;org-mobile-encryption-tempfile			org-mobile:91
  ;;org-mobile-encryption-password			org-mobile:111
  ;;org-mobile-inbox-for-pull				org-mobile:128
  ;;org-mobile-index-file				org-mobile:139
  ;;org-mobile-agendas					org-mobile:150
  ;;org-mobile-force-id-on-agenda-items		org-mobile:160
  ;;org-mobile-force-mobile-change			org-mobile:170
  ;;org-mobile-action-alist				org-mobile:197
  ;;org-mobile-checksum-binary				org-mobile:207

  ;;;; org-plain-lists
  ;;org-cycle-include-plain-lists			org-list:151
  ;;org-list-demote-modify-bullet			org-list:183
  ;;org-plain-list-ordered-item-terminator		org-list:205
  ;;org-alphabetical-lists				org-list:215
  ;;org-list-two-spaces-after-bullet-regexp		org-list:225
  org-list-ending-method			'both	; indent and regexp
  org-empty-line-terminates-plain-lists	nil
  ;;org-list-end-regexp				org-list:263
  ;;org-list-automatic-rules				org-list:288
  ;;org-hierarchical-checkbox-statistics		org-list:310
  ;;org-description-max-indent				org-list:317
  ;;org-list-radio-list-templates			org-list:342

  ;;;; org-priorities
  ;;org-enable-priority-commands			org:2471
  org-highest-priority		?A
  org-lowest-priority		?E
  org-default-priority		?C
  org-priority-start-cycle-with-default nil	; don't start cycle with default
  ;;org-get-priority-function				org:2506

  ;;;; org-progress
  ;;org-log-refile					org:1839
  org-log-done			'time	; add CLOSED on completed todos
  ;;org-log-reschedule					org:2281
  ;;org-log-redeadline					org:2305
  org-log-note-clock-out	t	; auto-add add a note
  org-log-done-with-time	t	; both date and time
  ;;org-log-note-headings				org:2352
  ;;org-log-into-drawer				org:2386
  ;;org-log-state-notes-insert-after-drawers		org:2415
  org-log-states-order-reversed t	; newest on top
  org-log-repeat		'time	; record at least time on repeats
  ;;org-effort-property				org:2986

  ;;;; org-properties
  ;;org-property-format				org:2907
  ;;org-use-property-inheritance			org:2933
  ;;org-columns-default-format				org:2956
  ;;org-columns-ellipses				org:2966
  ;;org-columns-modify-value-for-display-function	org:2979
  ;;org-effort-property				org:2985
  ;;org-global-properties				org:3014
  ;;org-columns-skip-archived-trees			org:3985

  ;;;; org-refile
  org-directory		"~/org"		; where to put org-files
  org-default-notes-file	"~/org/notes"	; notes-file
  ;;org-goto-interface					org:1795
  ;;org-goto-max-level					org:1802
  org-reverse-note-order	nil
  ;;org-log-refile					org:1838
  ;;org-refile-targets					org:1874
  ;;org-refile-target-verify-function			org:1899
  ;;org-refile-use-cache				org:1910
  ;;org-refile-use-outline-path			org:1922
  ;;org-outline-path-complete-in-steps			org:1938
  ;;org-refile-allow-creating-parent-nodes		org:1952

  ;;;; org-remember
  ;;org-directory					org:1773
  ;;org-default-notes-file				org:1781
  ;;org-reverse-note-order				org:1810

  ;;;; org-reveal-location
  ;;org-show-hierarchy-above				org:692
  ;;org-show-following-heading				org:705
  ;;org-show-siblings					org:721
  ;;org-show-entry-below				org:733

  ;;;; org-sparse-trees
  ;;org-highlight-sparse-tree-matches			org:1133
  ;;org-remove-highlights-with-change			org:1142
  ;;org-occur-hook					org:1151
  ;;org-sparse-tree-open-archived-trees		org:4002

  ;;;; org-startup
  org-startup-folded		nil	; show all headers
  org-startup-truncated	t	; turn off linewrap
  org-use-sub-superscripts	t	; 
  ;;org-startup-with-beamer-mode			org:469
  org-startup-align-all-tables	nil	; faster
  org-startup-with-inline-images nil	;
  org-insert-mode-line-in-empty-file t	; add -*- line
  org-replace-disputed-keys	t	; the S-bindings isn't any good
  ;;org-use-extra-keys					org:528
  ;;org-disputed-keys					org:548
  ;;org-ellipsis					org:583
  ;;org-hide-block-startup				org:835

  ;;;; org-structure
  ;;org-startup-indented				org:424
  ;;org-indirect-buffer-display			org:747
  ;;org-use-speed-commands				org:759
  ;;org-speed-commands-user				org:777
  ;;org-drawers					org:825
  org-M-RET-may-split-line	nil	; dont split lines
  ;;org-insert-heading-respect-content			org:1081
  ;;org-speed-command-hook				org:16762

  ;;;; org-table
  org-enable-table-editor	'optimized
  ;;org-self-insert-cluster-for-undo			org:1206
  ;;orgtbl-optimized					org-table:68
  ;;orgtbl-radio-table-templates			org-table:93
  ;;org-table-error-on-row-ref-crossing-hline		org-table:268

  ;;;; org-table-calculation
  org-table-use-standard-references t	; B3 instead of @3$2
  org-table-copy-increment	t	; increment on copy-down
  ;;org-calc-default-modes				org-table:220
  ;;org-table-formula-evaluate-inline			org-table:231
  ;;org-table-formula-use-constants			org-table:239
  ;;org-table-formula-constants			org-table:255
  ;;org-table-allow-automatic-line-recalculation	org-table:263
  ;;org-table-relative-ref-may-cross-hline		org-table:284

  ;;;; org-table-editing
  ;;org-table-tab-recognizes-table.el			org:1213
  org-table-automatic-realign	t	; relign tables while editing
  org-table-auto-blank-field	t	; clear cell content on edit
  ;;org-table-fix-formulas-confirm			org-table:167
  org-table-tab-jumps-over-hlines nil	; insert rows before hline

  ;;;; org-table-import-export
  ;;org-table-export-default-format			org-table:301

  ;;;; org-table-settings
  ;;org-table-default-size				org-table:105
  ;;org-table-number-regexp				org-table:121
  ;;org-table-number-fraction				org-table:142

  ;;;; org-tags
  ;;org-todo-state-tags-triggers			org:2225
  ;;org-tag-alist					org:2719
  ;;org-tag-persistent-alist				org:2743
  ;;org-complete-tags-always-offer-all-agenda-tags	org:2763
  ;;org-use-fast-tag-selection				org:2781
  ;;org-fast-tag-selection-single-key			org:2793
  org-tags-column			-79	; looks better than -80
  org-auto-align-tags			t	; do alignment
  org-use-tag-inheritance		t
  ;;org-tags-exclude-from-inheritance			org:2844
  org-tags-match-list-sublevels	nil
  ;;org-tags-sort-function				org:2883
  ;;;org-tag-faces

  ;;;; org-time
  ;;org-remove-highlights-with-change			org:1143
  org-insert-labeled-timestamps-at-point nil	; use next line
  org-time-stamp-rounding-minutes '(0 1)	; don't round minutes
  org-display-custom-times		nil	; use the default iso-dates
  ;;org-time-stamp-custom-formats			org:2572
  ;;org-time-clocksum-format				org:2586
  ;org-time-clocksum-format		"%d:%02d"
  ;;org-time-clocksum-use-fractional			org:2592
  org-time-clocksum-use-fractional	t
  ;;org-time-clocksum-fractional-format		org:2598
  org-deadline-warning-days		14
  org-read-date-prefer-future		t	; prefer future for incomplete
  ;;org-agenda-jump-prefer-future			org:2647
  org-read-date-display-live		t	; show interpreted date
  org-read-date-popup-calendar		t
  ;;org-read-date-minibuffer-setup-hook		org:2675
  ;;org-extend-today-until				org:2690
  org-edit-timestamp-down-means-later	nil
  ;;org-calendar-follow-timestamp-change		org:2703

  ;;;; org-todo
  org-todo-keywords
  '((sequence "TODO(t!)" "DONE(d@)")  ; (w!/@)
    (sequence "REPORT(r)" "BUG(b@/!)" "KNOWNCAUSE(k)" "|" "DONE(d!)" "CANCELLED(c@)")
    )
  ;;org-todo-interpretation				org:2071
  org-use-fast-todo-selection	'prefix
  ;;org-provide-todo-statistics			org:2109
  ;;org-hierarchical-todo-statistics			org:2123
  ;;org-after-todo-state-change-hook			org:2130
  org-enforce-todo-dependencies	t	; don't let marking a parent done befor childs
  org-enforce-todo-checkbox-dependencies	t	; require all CBs checked
  ;;org-treat-insert-todo-heading-as-state-change	org:2204
  ;;org-treat-S-cursor-todo-selection-as-state-change	org:2212
  ;;org-todo-state-tags-triggers			org:2224
  org-log-done		'time		; time is enough for defaults
  ;;org-log-reschedule					org:2280
  ;;org-log-redeadline					org:2304
  ;;org-log-note-clock-out				org:2318
  ;;org-log-note-headings				org:2351
  org-log-into-drawer			t	; hide away logs
  ;;org-log-state-notes-insert-after-drawers		org:2414
  ;;org-log-states-order-reversed			org:2421
  ;;org-todo-repeat-to-state				org:2430
  ;;org-log-repeat					org:2454
  org-track-ordered-property-with-tag	t	; a bit nicer
  ;;org-clock-in-switch-to-state			org-clock:93
  ;;org-clock-out-switch-to-state			org-clock:106
  org-todo-keyword-faces
  '(("TODO" . org-warning)
    ("CANCELLED" . (:foreground "blue" :wigth bold))
    )   
  ;;;org-priority-faces

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

