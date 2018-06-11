;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;; local elisp files which refused to be installed with quelpa
(after! org-protocol  (load! "local/org-protocol-capture-html/org-protocol-capture-html.el"))
;; (after! org-agenda    (load! "local/org-agenda-ng/org-ql.el"))
;; (after! org-agenda    (load! "local/org-agenda-ng/org-agenda.el"))
;; (after! org-agenda    (load! "local/org-sidebar/org-sidebar.el"))

;; load additional org-modules
(add-hook 'org-load-hook '(lambda () (setq org-modules (append '(org-man org-eww org-protocol org-habit) org-modules))))

(after! org
  ;; clock persistence
  (org-clock-persistence-insinuate)

  ;; open all pdf links with org-pfdview
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link))))


  (setq
   +org-dir "~/org/"
   +org-attach-dir "attach/"
   org-export-directory "export/"
   org-crypt-tag-matcher "+crypt-nocrypt"

   org-capture-templates '(("p" "Protocol" entry (file "~/org/BOOKMARKS.org")
                            "**** [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:quote:\n%u\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n"
                            :immediate-finish t :kill-buffer t :prepend t)

                           ("L" "Protocol Link" entry (file "~/org/BOOKMARKS.org")
                            "**** [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:\n%u"
                            :immediate-finish t :kill-buffer t :prepend t)

                           ("w" "Website" entry (file "~/org/WEBSITES.org")
                            "* %a :website:\n\n%U %?\n\n%:initial"
                            :immediate-finish t :kill-buffer t)

                           ("e" "journal Entry" entry (file+olp+datetree "~/org/JOURNAL.org")
                            "**** %?" :tree-type week)

                           ("t" "task" entry (file+headline "~/org/GTD.org" "TASKS")
                            "* [ ] %?" :prepend t)
                           )

   org-agenda-custom-commands
   ' (("P" "Projects" ((tags "+LEVEL=2+CATEGORY=\"PROJECTS\"
                              |+LEVEL=3+CATEGORY=\"PROJECTS\"
                              |+LEVEL=4+CATEGORY=\"PROJECTS\""))
       ((org-agenda-overriding-header "Projects Overview")
        (org-agenda-files '("~/org/GTD.org"))
        ))
      ("T" "Tasks" ((tags "+LEVEL=1+CATEGORY=\"TASKS\"
                          |+LEVEL=2+CATEGORY=\"TASKS\""))
       ((org-agenda-overriding-header "Tasks Overview")
        (org-agenda-files '("~/org/GTD.org"))
        ))
      )
   org-agenda-files '("~/org/GTD.org")
   org-agenda-prefix-format '((agenda  . "  %-12s%6t ")
                              (timeline  . "%s ")
                              (todo  . "     Effort: %6e  ")
                              (tags  . "%l")
                              (search . "%l"))
   org-agenda-todo-list-sublevels t
   org-agenda-log-mode-items '(closed clock state)
   org-agenda-span 2
   org-agenda-start-on-weekday nil
   org-agenda-start-with-log-mode nil
   org-agenda-start-day "1d"
   org-agenda-compact-blocks t
   org-agenda-dim-blocked-tasks 'invisible
   org-tags-match-list-sublevels 'indented
   org-agenda-tags-column 68
   org-agenda-category-icon-alist
   `(("GTD" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center))
   org-show-context-detail '((agenda .minimal)
                             (bookmark-jump . lineage)
                             (isearch . lineage)
                             (default . lineage)
                             )

   ;; org-todo-keywords '()
   ;; org-todo-keyword-faces '()
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies nil
   org-provide-todo-statistics t
   org-checkbox-hierarchical-statistics nil
   org-hierarchical-todo-statistics nil

   org-startup-with-inline-images t
   org-hide-emphasis-markers nil
   org-fontify-whole-heading-line nil

   org-refile-targets '((org-agenda-files :maxlevel . 5))
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil

   org-id-track-globally t
   org-id-locations-file (concat +org-dir ".org-ids-locations")
   org-use-property-inheritance t

   org-log-done 'time
   org-log-redeadline 'time
   org-log-reschedule 'time
   org-log-into-drawer "LOGBOOK"

   org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
   org-drawers (quote ("PROPERTIES" "LOGBOOK"))

   org-clock-auto-clock-resolution (quote when-no-clock-is-running)
   org-clock-report-include-clocking-task t
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist-query-resume nil
   org-clock-history-length 23
   org-clock-out-when-done t
   org-clock-into-drawer t
   org-clock-in-resume t
   org-clock-persist t

   evil-org-key-theme '(textobjects insert navigation additional shift heading)

   )


  ;; hooks
  (add-hook 'org-after-todo-state-change-hook 'org-save-all-org-buffers)
  (add-hook 'org-agenda-after-show-hook 'org-narrow-to-subtree)
  (add-hook 'org-mode-hook #'visual-line-mode)
  ;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (remove-hook 'org-agenda-finalize-hook '+org|cleanup-agenda-files)
  (remove-hook 'org-mode-hook #'auto-fill-mode)


  ;; faces
  (set-face-attribute     'org-level-1 nil                :height 1.0)
  (set-face-attribute     'org-agenda-date nil            :height 1.0)
  (set-face-attribute     'org-agenda-date-today    nil   :height 1.0)
  (set-face-attribute     'org-agenda-date-weekend  nil   :height 1.0)
  (set-face-attribute     'org-agenda-structure     nil   :height 1.0)

  ;; advices
  (advice-add 'org-agenda-exit :before 'org-save-all-org-buffers)

  (advice-add 'org-refile :after
              (lambda (&rest _)
                (org-save-all-org-buffers)))


  (defadvice org-capture-refile (around opened-org-capture-around activate)
    (opened-org-agenda-files)
    ad-do-it
    (kill-org-agenda-files))

  (defadvice org-agenda (around opened-org-capture-around activate)
    (opened-org-agenda-files)
    ad-do-it
    (kill-org-agenda-files))

  (defadvice org-agenda-exit (around opened-org-capture-around activate)
    (opened-org-agenda-files)
    ad-do-it
    (kill-org-agenda-files))

  (defadvice org-agenda-quit (around opened-org-capture-around activate)
    (opened-org-agenda-files)
    ad-do-it
    (kill-org-agenda-files))

  (defadvice org-refile (around opened-org-capture-around activate)
    (opened-org-agenda-files)
    ad-do-it
    (kill-org-agenda-files))

  (defadvice org-clock-in (around opened-org-clock-in activate)
    (opened-org-agenda-files)
    ad-do-it
    (kill-org-agenda-files)
    (org-save-all-org-buffers))

  (defadvice org-clock-out (around opened-org-clock-out activate)
    (opened-org-agenda-files)
    ad-do-it
    (kill-org-agenda-files)
    (org-save-all-org-buffers))

  ;; popups
  (set! :popup "^\\*org-brain\\*$" '((vslot . -1) (size . 0.3) (side . left)) '((select . t) (quit . t) (transient . t)))
  (set! :popup "^CAPTURE.*\\.org$" '((side . bottom) (size . 0.4)) '((select . t)))
  (set! :popup "GTD.org" '((vslot . -1)(size . 0.32) (side . right)) '((select . t) (transient . nil)))
  (set! :popup "README.org" '((size . 0.4) (side . left)) '((select . t) (transient . nil)))
  (set! :popup "work-wiki.org" '((size . 0.4) (side . left)) '((select . t) (transient . nil)))
  (set! :popup "build-wiki.org" '((size . 0.4) (side . left)) '((select . t) (transient . nil)))
  (set! :popup "private-wiki.org" '((size . 0.4) (side . left)) '((select . t) (transient . nil)))
  (set! :popup "environment-wiki.org" '((size . 0.4) (side . left)) '((select . t) (transient . nil)))
  (set! :popup "education-wiki.org" '((size . 0.4) (side . left)) '((select . t) (transient . nil)))
  (set! :popup "^\\*Org Src" '((size . 0.4) (side . right)) '((quit) (select . t)))
  (set! :popup "^\\*Org Agenda.*\\*$" '((slot . -1) (size . 0.32) (side . right)) '((select . t) (modeline . nil) (quit . t)))

  )

;; packages
(def-package! org-brain
  :after org
  :init
  (setq org-brain-path "~/org")
  (set! :evil-state 'org-brain-visualize-mode 'emacs)
  :config
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length 12 )
  :commands
  (org-brain-visualize
   org-brain-goto-end
   org-brain-visualize
   org-brain-add-parent
   org-brain-add-child
   org-brain-add-friendship
   org-brain-add-relationship
   org-brain-add-resource
   org-brain-goto-parent
   org-brain-goto-child
   org-brain-goto-friend
   org-brain-goto-current
   org-brain-goto-end
   org-brain-goto-other-window
   org-brain-remove-child
   org-brain-remove-friendship
   org-brain-remove-parent
   ))
(def-package! org-pdfview
  :after org
  :commands org-pdfview-open
  )
(def-package! org-pomodoro
  :after org
  :commands (org-pomodoro org-pomodoro-remaining-seconds org-pomodoro-state)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
        org-pomodoro-ask-upon-killing nil
        )
  )
(def-package! org-projectile
  :after org
  :commands (org-projectile-todo-files org-projectile-capture-for-current-project)
  :init (setq org-projectile-per-project-filepath "README.org"
              org-projectile-capture-template (format "%s%s" "* TODO %?" :clock-in t)
              org-agenda-files (append (list
                                        ""
                                        ))
              )
  :config (org-projectile-per-project)
  )

(def-package! ereader
  :init (add-to-list 'doom-large-file-modes-list 'ereader-mode))
(def-package! org-ebook :after (org ereader))
(def-package! org-pdfview :after org)
(def-package! ob-javascript :after ob-core)
(def-package! ob-async :after ob-core :commands ob-async-org-babel-execute-src-block)
;; disabled
(def-package! org-fancy-priorities
  :after org
  :disabled
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))
(def-package! org-super-agenda
  :disabled
  :after org
  :commands org-super-agenda-mode
  :init
  ;; I will set this for specific commands only
  (setq org-super-agenda-groups nil)
  :config
  (org-super-agenda-mode))
(def-package! plain-org-wiki
  :disabled
  :after org
  :commands plain-org-wiki)
(def-package! org-edna
  :disabled
  :after org
  :commands org-edna-load
  )
(def-package! ox-hugo
  :disabled
  :after ox)
(def-package! ox-epub
  :disabled
  :after ox)

