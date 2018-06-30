;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-


(add-hook 'org-load-hook '(lambda () (setq org-modules (append '(org-man org-eww org-protocol org-habit) org-modules))))

(after! org
  (set-popup-rule! "^\\*org-brain\\*$"    :size 0.3 :side 'left :vslot -2 :select t :quit nil :transient t)
  (set-popup-rule! "^CAPTURE.*\\.org$"    :size 0.4 :side 'bottom :select t)
  (set-popup-rule! "GTD.org"              :size 0.32 :side 'right :vslot -1  :select t :transient nil)
  (set-popup-rule! "README.org"           :size 0.4 :side 'left :select t :transient nil)
  ;; (set-popup-rule! "work-wiki.org"        :size 0.4 :side 'left :select t :transient nil)
  ;; (set-popup-rule! "build-wiki.org"       :size 0.4 :side 'left :select t :transient nil)
  ;; (set-popup-rule! "private-wiki.org"     :size 0.4 :side 'left :select t :transient nil)
  ;; (set-popup-rule! "environment-wiki.org" :size 0.4 :side 'left :select t :transient nil)
  ;; (set-popup-rule! "education-wiki.org"   :size 0.4 :side 'left :select t :transient nil)
  (set-popup-rule! "^\\*Org Src"          :size 0.4 :side 'right :quit t :select t)
  (set-popup-rule! "^\\*Org Agenda.*\\*$" :size 0.32 :side 'right :slot -1 :select t :modeline nil :quit t)
  (set-popup-rule! "JOURNAL.org"          :size 0.4 :side 'top :select t :transient nil)
  (set-popup-rule! "SOMEDAY.org"          :size 0.4 :side 'right :select t :transient nil)
  (set-popup-rule! "MAYBE.org"            :size 0.4 :side 'right :select t :transient nil)
  (set-popup-rule! "BOOKMARKS.org"        :size 0.4 :side 'top :select t :transient nil)
  (add-hook 'doom-load-theme-hook #'aj/my-org-faces)
  (advice-add '+popup--delete-window :before #'(lambda (&rest _) (when (eq major-mode 'org-mode) (save-buffer))))
  (add-hook 'org-capture-mode-hook 'flyspell-mode)
  (add-hook 'org-after-todo-state-change-hook 'org-save-all-org-buffers)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook! :append 'org-mode-hook #'aj/my-org-faces)
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  ;; clock persistence
  (org-clock-persistence-insinuate)

  ;; open all pdf links with org-pfdview
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link))))
  (quiet!
   ;; register pdfview link type (copied from org-pdfview.el so I can lazy load)
   (org-link-set-parameters "pdfview"
                            :follow #'org-pdfview-open
                            :complete #'org-pdfview-complete-link
                            :store #'org-pdfview-store-link)
   (org-add-link-type "pdfview" 'org-pdfview-open)
   (add-hook 'org-store-link-functions 'org-pdfview-store-link)

   ;; ...and same thing for org-ebook
   (org-link-set-parameters "ebook"
                            :follow #'org-ebook-open
                            :store #'org-ebook-store-link)
   (org-add-link-type "ebook" 'org-ebook-open)
   (add-hook 'org-store-link-functions 'org-ebook-store-link)
   )

  (setq
   +org-dir "~/org/"
   +org-attach-dir "attach/"
   org-export-directory "export/"
   org-crypt-tag-matcher "+crypt-nocrypt"
   ;; settings for export to ical file

   org-tags-match-list-sublevels 'indented
   org-show-context-detail '((agenda .minimal)
                             (bookmark-jump . minimal)
                             (isearch . lineage)
                             (default . minimal)
                             )
   org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file-other-window)
                          (wl . wl-other-frame))
   org-todo-keywords
   ;;           todo     ongoing  hold         zap      done
   '((sequence "[ ](t)" "[-](o)" "[!](h)" "|" "[✘](z)" "[✔](d)")
     (sequnece "STARTED(s)" "|" "FINISHED(f)")
     (sequence "MAYBE(M)" "SOMEDAY(S)" "TODO(T)" "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "DONE(D)" "CANCELLED(c)"))
   org-todo-keyword-faces '(("NEXT" . "#98be65") ("WAITING" . "#c678dd") ("TODO" . "#ECBE7B") ("STARTED" . "#4db5bd"))
   org-enforce-todo-dependencies t
   org-enforce-todo-checkbox-dependencies nil
   org-provide-todo-statistics t
   org-hierarchical-todo-statistics t

   org-startup-with-inline-images t
   org-hide-emphasis-markers nil
   org-fontify-whole-heading-line nil
   org-src-fontify-natively t
   org-imenu-depth 9

   ;; org-refile-targets '((org-agenda-files :maxlevel . 5))
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
   )


  )

(after! org-clock
  (advice-add 'org-clock-in :around (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-clock-out :around (lambda (&rest _) (org-save-all-org-buffers)))
  (setq
   org-clock-auto-clock-resolution (quote when-no-clock-is-running)
   org-clock-report-include-clocking-task t
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist-query-resume nil
   org-clock-history-length 23
   org-clock-out-when-done t
   org-clock-into-drawer t
   org-clock-in-resume t
   org-clock-persist t
   )

  )

(after! ox-icalendar
  org-icalendar-store-UID t
  org-icalendar-combined-agenda-file "~/org/agenda.ics"
  org-icalendar-include-todo '(all)
  org-icalendar-use-scheduled '(event-if-todo event-if-not-todo)
  org-icalendar-use-deadline '(event-if-todo event-if-not-todo)
  )

(after! org-list
  (setq
   org-checkbox-hierarchical-statistics t
   )
  )

(after! org-agenda
  (advice-add 'org-agenda-archive :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-archive-default :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-exit :before 'org-save-all-org-buffers)
  (advice-add 'org-agenda-switch-to :after 'turn-off-solaire-mode)
  (add-hook 'org-agenda-mode-hook #'hide-mode-line-mode)
  (add-hook 'org-agenda-after-show-hook 'org-narrow-to-subtree)
  ;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (remove-hook 'org-agenda-finalize-hook '+org|cleanup-agenda-files)

  (setq

   ;; org-agenda-files '("~/org/GTD.org")
   org-agenda-prefix-format '((agenda  . "  %-12s%6t ")
                              (timeline  . "%s ")
                              (todo  . "     Effort: %6e  ")
                              (tags  . "%l")
                              (search . "%l"))

   org-agenda-tags-column 68
   org-agenda-category-icon-alist
   `(("GTD" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center))
   org-agenda-todo-list-sublevels t
   org-agenda-log-mode-items '(closed clock state)
   org-agenda-span 2
   org-agenda-start-on-weekday nil
   org-agenda-start-with-log-mode nil
   org-agenda-start-day "1d"
   org-agenda-compact-blocks t
   org-agenda-dim-blocked-tasks 'invisible

   org-agenda-custom-commands
   ' (("P" "Projects" ((tags "+LEVEL=2+CATEGORY=\"PROJECTS\"
                              |+LEVEL=3+CATEGORY=\"PROJECTS\"
                              |+LEVEL=4+CATEGORY=\"PROJECTS\"/!+STARTED|+NEXT"))
       ((org-agenda-overriding-header "Projects Overview")
        (org-agenda-files '("~/org/GTD.org"))
        (org-agenda-dim-blocked-tasks nil)
        ))

      ("C" "Current project" ((tags "+LEVEL=1+CATEGORY=\"TASKS\"
                                    |+LEVEL=2+CATEGORY=\"TASKS\""))
       ((org-agenda-files (aj/return-project-org-file))
        (org-agenda-overriding-header (aj/return-short-project-name))
        ))
      ("T" "Tasks" ((tags "+LEVEL=1+CATEGORY=\"TASKS\"
                          |+LEVEL=2+CATEGORY=\"TASKS\""))
       ((org-agenda-overriding-header "Tasks Overview")
        (org-agenda-files '("~/org/GTD.org"))
        ))
      )
   )
  )

(after! ob-core
  (setq
   org-babel-default-header-args '((:session . "none")
                                   (:results . "replace")
                                   (:exports . "code")
                                   (:cache . "no")
                                   (:noweb . "no")
                                   (:hlines . "no")
                                   (:tangle . "no")
                                   (:mkdir . "yes"))
   )
  )

(after! org-protocol
  (load! "local/org-protocol-capture-html/org-protocol-capture-html.el"))

(after! org-archive
  (advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree-default :after #'org-save-all-org-buffers)
  )

(after! org-refile
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  )

(after! evil-org
  (setq evil-org-key-theme '(textobjects insert navigation additional shift heading)))

(after! org-capture
  (setq
   org-capture-templates `(("p" "Protocol" entry (file "~/org/BOOKMARKS.org")
                            "**** [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:quote:\n%u\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n"
                            :immediate-finish t :prepend t)

                           ("L" "Protocol Link" entry (file "~/org/BOOKMARKS.org")
                            "**** [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] :link:\n%u"
                            :immediate-finish t :prepend t)

                           ("w" "Website" entry (file "~/org/WEBSITES.org")
                            "* %c :website:\n\n%U %?\n\n%:initial" :immediate-finish t)

                           ("e" "journal Entry" entry (file+olp+datetree "~/org/JOURNAL.org")
                            "**** %?" :tree-type week)

                           ("t" "task" entry (file+headline "~/org/GTD.org" "TASKS")
                            "* [ ] %?" :prepend t)

                           ("c" "calendar" entry (file+headline "~/org/GTD.org" "CALENDAR")
                            "** %^{Title} %^g\n SCHEDULED: %^{Scheduled to begin}t \nr")

                           ("P" "Project task" entry (file+headline ,(concat (projectile-project-root) "README.org") "TASKS")
                            "* [ ] %?" :prepend t)

                           ("J" "Project journal" entry (file+olp+datetree ,(concat (projectile-project-root) "README.org") "JOURNAL")
                            "**** %?" :tree-type week)
                           )
   )
  )

;; packages
(def-package! org-brain
  ;; :after org
  :commands
  (org-brain-visualize
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
   )
  :init
  (set-evil-initial-state! 'org-brain-visualize-mode 'emacs)
  :config
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length 12 )
  )

(def-package! org-pdfview
  :commands (org-pdfview-open org-pdfview-store-link org-pdfview-complete-link org-pdfview-export)
  )

(def-package! org-pomodoro
  ;; :after org
  :commands (org-pomodoro org-pomodoro-remaining-seconds org-pomodoro-state)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
        org-pomodoro-ask-upon-killing nil
        )
  )

(def-package! org-projectile
  ;; :after org
  :commands (org-projectile-todo-files org-projectile-capture-for-current-project)
  :init (setq org-projectile-per-project-filepath "README.org"
              org-projectile-capture-template (format "%s%s" "* TODO %?" :clock-in t)
              ;; org-agenda-files (append (list
              ;;                           ""
              ;;                           ))
              )
  :config (org-projectile-per-project)
  )

(def-package! ereader
  :commands (ereader-read-epub ereader-mode)
  :mode ("\\.epub\\'". ereader-mode)
  :init (add-to-list 'doom-large-file-modes-list 'ereader-mode)
  :config
  (add-hook 'ereader-mode-hook 'hide-mode-line-mode)
  )

(def-package! org-ebook
  :commands (org-ebook-open org-ebook-store-link)
  )

(def-package! ob-javascript
  :after ob-core)

(def-package! org-starter
  :after org-agenda
  :config
  ;; restrict refile targets
  (defvar +refile-targets-with-headlines t)
  (setq +refile-targets-with-headlines '("GTD.org" "About.org"))

  (defun aj/verify-headlines-for-refile ()
    (if (not (member (buffer-name) +refile-targets-with-headlines)) nil t))

  (setq org-refile-target-verify-function 'aj/verify-headlines-for-refile)

  ;; directories
  (setq org-starter-path `(,org-brain-path ,org-directory))

  ;; files
  (org-starter-define-file "GTD.org"          :agenda t :refile '(:level . 1) :key "g")
  (org-starter-define-file "Books.org"        :agenda t :refile '(:level . 1) :key "b")
  (org-starter-define-file "Podcasts.org"     :agenda t :refile '(:level . 1) :key "p")
  (org-starter-define-file "MOC.org"          :agenda t :refile '(:level . 1) :key "m")
  )

(def-package! counsel-org-starter
  :commands (counsel-org-starter counsel-org-starter-known-file))

(def-package! ob-async
  :commands ob-async-org-babel-execute-src-block
  )

;; (def-package! ox-hugo
;;   :disabled
;;   :after ox)

