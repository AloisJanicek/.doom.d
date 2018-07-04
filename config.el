;;;  -*- lexical-binding: t; -*-
(load! "+bindings")

(add-hook 'org-load-hook '(lambda () (setq org-modules (append '(org-man org-eww org-protocol org-habit) org-modules))))

(set-popup-rule! "*backtrace\*" :size 0.4 :side 'right :select t)

(def-package! all-the-icons-ivy
  :after ivy
  :config
  (dolist (cmd '( counsel-find-file counsel-file-jump
                                    counsel-dired-jump counsel-projectile-find-dir
                                    counsel-projectile-switch-project))
    (ivy-set-display-transformer cmd #'all-the-icons-ivy-file-transformer)))

(def-package! apache-mode
  :mode (("apache\\.conf\\'" . apache-mode)
         ("\\.htaccess\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode)
         ("srm\\.conf\\'"    . apache-mode)
         ("access\\.conf\\'" . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

(def-package! cheatsheet
  :commands (cheatsheet-add cheatsheet-add-group cheatsheet-get cheatsheet-show))

(def-package! counsel-org-starter
  :commands (counsel-org-starter counsel-org-starter-known-file))

(def-package! define-word
  :commands (define-word  define-word-at-point))

(def-package! exwm
  :disabled
  :config
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)
  (setq exwm-workspace-number 4)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "VGA-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output VGA-1 --right-of LVDS-1 --auto")))
  (exwm-randr-enable))

(def-package! ereader
  :commands (ereader-read-epub ereader-mode)
  :mode ("\\.epub\\'". ereader-mode)
  :init (add-to-list 'doom-large-file-modes-list 'ereader-mode)
  :config
  (add-hook 'ereader-mode-hook 'hide-mode-line-mode)
  )

(def-package! fish-mode
  :commands (fish-mode))

(def-package! find-file-in-project
  :commands (ffip ffip-show-diff))

(def-package! gulp-task-runner
  :commands gulp)

(def-package! highlight-blocks
  :commands (highlight-blocks-mode highlight-blocks-now))

(def-package! hungry-delete
  :demand t
  :config
  (setq hungry-delete-except-modes
        '(term-mode help-mode minibuffer-inactive-mode calc-mode))
  (global-hungry-delete-mode 1))

(def-package! ivy-yasnippet
  :commands (ivy-yasnippet))

(def-package! ivy-mpdel
  :disabled
  :config
  (set-popup-rule! "*MPDEL Current Playlist*"       :size 0.4 :side 'left :select t :transient nil))

(def-package! link-hint
  :commands (link-hint-open-all-links
             link-hint-copy-all-links
             link-hint-open-link
             link-hint-copy-link))

(def-package! mpdel
  :disabled
  :config
  (defhydra aj/mpd-control ()
    "Control podcaster:"
    ("a" (ivy-mpdel-artists) "artist")
    ("A" (mpdel-nav-add-to-current-playlist) "Add" :color blue)
    ("l" (ivy-mpdel-stored-playlists) "list")
    ("p" (libmpdel-playback-play-pause) "play/pause" :color blue)
    ("s" (libmpdel-stop) "stop" :color blue)
    ("o" (aj-mpdel-playlist-open) "open" :color blue)))

(def-package! ob-async
  :commands ob-async-org-babel-execute-src-block
  )

(def-package! ob-javascript
  :after ob-core)

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

(def-package! org-ebook
  :commands (org-ebook-open org-ebook-store-link)
  )

(def-package! org-starter
  :after org-agenda
  :config
  ;; directories
  (setq org-starter-path `(,org-brain-path ,org-directory, (concat org-brain-path "private_brain")))

  ;; files
  (org-starter-define-file "GTD.org"          :agenda t :refile '(:level . 1) :key "g")
  (org-starter-define-file "Books.org"        :agenda t :refile '(:level . 1) :key "b")
  (org-starter-define-file "Podcasts.org"     :agenda t :refile '(:level . 1) :key "p")
  (org-starter-define-file "MOC.org"          :agenda t :refile '(:level . 1) :key "m")
  (org-starter-define-file "Yoga.org"         :agenda t :refile '(:level . 1) :key "y")
  )

(def-package! org-super-agenda
  :after org-agenda
  :init (advice-add #'org-super-agenda-mode :around #'doom*shut-up)
  :config (org-super-agenda-mode)
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

(def-package! other-frame-window)

(def-package! outline-magic
  :commands (outline-cycle outline-next-line outline-move-subtree-up outline-move-subtree-down outline-promote outline-demote))

(def-package! plain-org-wiki
  :config
  (setq pow-directory "~/org/brain"))

(def-package! podcaster
  :disabled
  :commands podcaster
  :init
  :config
  (setq podcaster-feeds-urls (list "https://talkpython.fm/episodes/rss" "http://feeds.soundcloud.com/users/soundcloud:users:277306156/sounds.rss"))
  (defhydra aj/podcaster-control ()
    "Control podcaster:"
    ("l" (podcaster) "listen")
    ("p" (podcaster-pause) "pause")
    ("s" (podcaster-stop) "stop")
    ("r" (podcaster-resume) "resume")))

(def-package! robots-txt-mode
  :mode (("/robots\\.txt\\'" . robots-txt-mode)))

(def-package! sdcv
  :commands (sdcv-search-input sdcv-search-pointer)
  :config
  (set-popup-rule! "*SDCV\*" :size 0.4 :side 'top :select t))

(def-package! systemd
  :commands (systemd-mode))

(def-package! xml+
  :commands (xml+-query--generic xml+-query-all xml+-query-first xml+-node-text xml+-node-text--helper))

(def-package! x-path-walker
  :commands (helm-x-path-walker))

(after! apropos
  (set-popup-rule! "*apropos\*"                     :size 0.4 :side 'left :select t)
  (set-popup-rule! "*Apropos\*"                     :size 0.4 :side 'left :select t))

(after! auth-source
  (setq auth-sources '("~/.authinfo.gpg"))
  )

(after! avy
  (setq avy-all-windows t
        avy-background t))

(after! css-mode
  (add-hook 'css-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "CSS"))))
  (add-hook 'scss-mode-hook (lambda () (setq-local counsel-dash-docsets '("Sass" "HTML" "CSS")))))

(after! cus-edit
  (set-popup-rule! "*Customize\*"                   :size 0.4 :side 'left :select t :transient nil))

(after! company
  (setq company-idle-delay 0.6)
  (setq company-minimum-prefix-length 2))

(after! counsel
  (setq counsel-grep-base-command "grep -E -n -i -e %s %s")
  (setq counsel-org-goto-face-style 'org
        counsel-org-headline-display-style 'path
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t)
  (set-popup-rule! "^\\*ivy-occur" :size 0.70 :ttl 0 :quit nil)
  (advice-add #'+ivy-buffer-transformer :override #'+ivy-combined-buffer-transformer))

(after! counsel-projectile
  (advice-add  #'+ivy-projectile-find-file-transformer :override #'+ivy-projectile-find-file-combined-transformer))

(after! epa
  (setq epa-pinentry-mode 'ask))

(after! elisp-mode
  (add-hook 'emacs-lisp-mode-hook
            '(lambda ()
               (dolist (imenu-exp '(("After" "^\\s-*(after! +\\([^ ()\n]+\\)" 1)
                                    ("Hydra" "^\\s-*(defhydra +\\([^ ()\n]+\\)" 1)))
                 (add-to-list 'imenu-generic-expression imenu-exp)))
            t)
  ;; (add-hook 'emacs-lisp-mode-hook '+evil:fold-close-all)
  )

(after! evil
  (setq evil-move-cursor-back nil))

(after! evil-org
  (setq evil-org-key-theme '(textobjects insert navigation additional shift heading)))

(after! emmet-mode
;;; run remaping function before entering emmet-preview
  (advice-add 'emmet-preview :before 'aj/remap-emmet)
  (defadvice emmet-preview-accept (after emmet-after activate) (aj/indent-if-not-webmode)))

(after! eww
  (set-popup-rule! "*eww\*"                         :size 0.4 :side 'left :select t)
  (add-hook 'eww-mode-hook 'visual-line-mode-hook))

(after! faces
  (set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab")
  )

(after! files
  (add-hook 'after-save-hook #'prettier-stylelint-fix-file-and-revert)
  (add-hook 'after-save-hook #'beautify-html-file-and-revert))

(after! flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq flycheck-stylelintrc "~/.stylelintrc.json"
        flycheck-tidyrc "~/.tidyrc")
  (setq-default flycheck-disabled-checkers '(css-csslint scss sass/scss-sass-lint))
  ;; note: broken with default flycheck, needs :branch "fix-1398-quoted-lambdas"
  ;; see: https://github.com/flycheck/flycheck/pull/1440
  ;; see: https://github.com/flycheck/flycheck/issues/1398
  ;;   (flycheck-define-checker javascript-eslint-custom
  ;;     "A Javascript syntax and style checker using eslint.
  ;; See URL `http://eslint.org/'."
  ;;     :command ("eslint" "--format=json"
  ;;               (option-list "--rulesdir" flycheck-eslint-rules-directories)
  ;;               "--stdin" "--stdin-filename" source-original)
  ;;     :standard-input t
  ;;     :error-parser flycheck-parse-eslint
  ;;     :enabled (lambda () (flycheck-eslint-config-exists-p))
  ;;     :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode)
  ;;     :working-directory flycheck-eslint--find-working-directory
  ;;     :error-explainer
  ;;     (lambda (error)
  ;;       (let ((error-code (flycheck-error-id error)))
  ;;         (progn
  ;;           (browse-url (concat "https://eslint.org/docs/rules/" error-code)))))
  ;;     :verify
  ;;     (lambda (_)
  ;;       (let* ((default-directory
  ;;                (flycheck-compute-working-directory 'javascript-eslint))
  ;;              (have-config (flycheck-eslint-config-exists-p)))
  ;;         (list
  ;;          (flycheck-verification-result-new
  ;;           :label "config file"
  ;;           :message (if have-config "found" "missing or incorrect")
  ;;           :face (if have-config 'success '(bold error)))))))
  ;;   (add-to-list 'flycheck-checkers 'javascript-eslint-custom)
  ;; css-styleling checke with explainer
  ;;   (flycheck-define-checker css-stylelint-custom
  ;;     "A CSS syntax and style checker using stylelint.

  ;; See URL `http://stylelint.io/'."
  ;;     :command ("stylelint"
  ;;               (eval flycheck-stylelint-args)
  ;;               (option-flag "--quiet" flycheck-stylelint-quiet)
  ;;               (config-file "--config" flycheck-stylelintrc))
  ;;     :standard-input t
  ;;     :error-parser flycheck-parse-stylelint
  ;;     :error-explainer
  ;;     (lambda (error)
  ;;       (let ((error-code (flycheck-error-id error)))
  ;;         (progn
  ;;           (browse-url (concat "https://stylelint.io/user-guide/rules/" error-code)))))
  ;;     :modes (css-mode))
  ;;   (add-to-list 'flycheck-checkers 'css-stylelint-custom)
  )

(after! flyspell
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC")))

(after! google-translate-default-ui
  (setq google-translate-default-source-language "cs"
        google-translate-default-target-language "en")
  (set-popup-rule! "*Google Translate\*"            :size 0.4 :side 'top :select t))

(after! help
  (set-popup-rule! "*help\*"                        :size 0.4 :side 'left :select t))

(after! helpful
  (set-popup-rule! "*helpful\*"                     :size 0.4 :side 'left :select t))

(after! ibuffer
  (set-popup-rule! "*Ibuffer\*"                     :size 0.4 :side 'left :select t))

(after! imenu-list

  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :select nil :ttl nil :quit nil)
  ;; First create new face which is a copy of hl-line-face
  (copy-face 'hl-line 'hl-line-imenu-list-face)
  ;; Change what you want in this new face
  (set-face-attribute 'hl-line-imenu-list-face nil
                      :background `,(doom-color 'base4) :weight 'bold :underline t)
  ;; Finally, the hook
  (add-hook 'imenu-list-major-mode-hook 'my-imenu-list-hl-line)
  (add-hook 'imenu-list-major-mode-hook 'variable-pitch-mode)
  )

(after! info
  (advice-add 'info :before 'aj/set-info-popup-width))

(after! ivy
  (setq ivy-height 40)
  (map-put ivy-display-functions-alist 't 'ivy-posframe-display-at-frame-center)
  (ivy-set-actions
   'counsel-projectile-bookmark
   '(("d" bookmark-delete "delete")
     ("e" bookmark-rename "edit")))
  (ivy-add-actions
   #'ivy-yasnippet
   '(("e" ivy-yasnippet--copy-edit-snippet-action "Edit snippet as your own"))))

(after! ivy-posframe
  (advice-add #'ivy-posframe-enable :around #'doom*shut-up))

(after! ivy-rich
  (advice-add #'+ivy-recentf-transformer :override #'+ivy-recentf-combined-transformer))

(after! js2-mode
  (add-hook 'js2-mode-hook (lambda () (setq-local counsel-dash-docsets '("JavaScript" "HTML" "CSS"))))
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default js2-basic-offset 2))

(after! json-mode
  (setq js2-basic-offset 2))

(after! loaddefs
  (setq browse-url-browser-function
        '(
          ("wikipedia\\.org" . browse-url-firefox)
          ("github" . browse-url-chromium)
          ("reddit" . browse-url-chromium)
          ("gitlab" . browse-url-chromium)
          ("youtube" . browse-url-chromium)
          ("eslint.org" . browse-url-chromium)
          ("stylelint.io" . browse-url-chromium)
          ("thefreedictionary\\.com" . eww-browse-url)
          ("dictionary\\.com" . eww-browse-url)
          ("merriam-webster\\.com" . eww-browse-url)
          ("." . gk-browse-url)
          ))
  )

(after! magit
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(after! man
  (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "#ff7a79")
  (set-face-attribute 'Man-underline nil :inherit 'underline :foreground "#98be65")
  (set-popup-rule! "*Man\*"                         :size 0.4 :side 'left :select t)
  (set-popup-rule! "*man\*"                         :size 0.6 :side 'left :select t))

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
  (set-popup-rule! "^\\*Org Agenda.*\\*$" :size 0.32 :side 'right :vslot -1 :select t :modeline nil :quit t)
  (set-popup-rule! "JOURNAL.org"          :size 0.4 :side 'top :select t :transient nil)
  (set-popup-rule! "SOMEDAY.org"          :size 0.4 :side 'right :select t :transient nil)
  (set-popup-rule! "GTD-Manual.org"       :size 0.4 :side 'right :select t :transient nil)
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
   org-M-RET-may-split-line '((default . nil))
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
   org-enforce-todo-dependencies nil ;; if t, it hides todo entries with todo children from agenda
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

(after! org-bullets
  (setq org-bullets-bullet-list
        '("◉")))

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
   ' (("i" "Inbox" ((tags-todo "INBOX"))((org-agenda-overriding-header "Inbox")
                                         (org-agenda-hide-tags-regexp "INBOX")))

      ("T" "Tasks" ((tags-todo "*"))((org-agenda-overriding-header "Tasks")
                                     (org-super-agenda-groups
                                      '((:discard (:children t))
                                        (:name "Projects"
                                               :auto-category t
                                               )))))

      ("c" "Clever" ((agenda ""
                             ((org-agenda-overriding-header "")
                              (org-agenda-show-current-time-in-grid t)
                              (org-agenda-span 'day)
                              (org-super-agenda-groups '((:name "Scheduled hours"
                                                                :time-grid t
                                                                )
                                                         (:name "Scheduled today"
                                                                :date today
                                                                :scheduled past ;; ensure overdue items
                                                                :scheduled today
                                                                )
                                                         (:discard (:anything t))))))
                     (tags-todo "*"
                                ((org-agenda-overriding-header "")
                                 (org-super-agenda-groups
                                  '((:discard (:scheduled t))
                                    (:name "Projects"
                                           :children t
                                           :order 2)
                                    (:name "Tasks (not scheduled)"
                                           :children nil
                                           :order 1
                                           )
                                    (:discard (:anything t))
                                    ))))
                     )
       ((org-agenda-prefix-format '((agenda  . "  %-5t %6e ")
                                    (timeline  . "%s ")
                                    (todo  . "     Effort: %6e  ")
                                    (tags  . "        %6e ")
                                    (search . "%l")))
        (org-tags-match-list-sublevels t)
        ))

      ("C" "Current project" ((tags "+LEVEL=1+CATEGORY=\"TASKS\"
                                    |+LEVEL=2+CATEGORY=\"TASKS\""))
       ((org-agenda-files (aj/return-project-org-file))
        (org-agenda-overriding-header (aj/return-short-project-name))))

      ("P" "Projects" ((tags-todo "*" ((org-agenda-overriding-header "Projects")
                                       (org-super-agenda-groups
                                        '((:discard (:children nil))
                                          (:name "Projects"
                                                 :auto-category t))
                                        ))))))
   )
   )

(after! org-archive
  (advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree-default :after #'org-save-all-org-buffers)
  )

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

                           ;; Capture: GTD
                           ("g" "GTD:")
                           ("gi" "INBOX" entry
                            (file+headline +GTD "INBOX")
                            "* TODO %?" :prepend t)

                           ("gm" "MORNING" entry
                            (file+headline +GTD "MORNING")
                            "* TODO %?" :prepend t)

                           ("gw" "WORK" entry
                            (file+headline +GTD "WORK")
                            "* TODO %?" :prepend t)

                           ("gl" "LUNCH" entry
                            (file+headline +GTD "LUNCH")
                            "* TODO %?" :prepend t)

                           ("go" "OUTSIDE" entry
                            (file+headline +GTD "OUTSIDE")
                            "* TODO %?" :prepend t)

                           ("ge" "EVENING" entry
                            (file+headline +GTD "EVENING")
                            "* TODO %?" :prepend t)

                           ("gs" "SLEEP" entry
                            (file+headline +GTD "SLEEP")
                            "* TODO %?" :prepend t)

                           ("c" "calendar" entry (file+headline "~/org/GTD.org" "CALENDAR")
                            "** %^{Title} %^g\n SCHEDULED: %^{Scheduled to begin}t \nr")

                           ("P" "Project task" entry (file+headline ,(concat (projectile-project-root) "README.org") "TASKS")
                            "* [ ] %?" :prepend t)

                           ("J" "Project journal" entry (file+olp+datetree ,(concat (projectile-project-root) "README.org") "JOURNAL")
                            "**** %?" :tree-type week)
                           )
   )
  )

(after! org-clock
  (advice-add 'org-clock-in :around (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-clock-out :around (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add #'org-clock-load :around #'doom*shut-up)

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

(after! org-list
  (setq
   org-checkbox-hierarchical-statistics t
   )
  )

(after! org-protocol
  (load! "local/org-protocol-capture-html/org-protocol-capture-html.el"))

(after! org-refile
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  (setq org-refile-target-verify-function 'aj/verify-headlines-for-refile)
  )

(after! ox-icalendar
  org-icalendar-store-UID t
  org-icalendar-combined-agenda-file "~/org/agenda.ics"
  org-icalendar-include-todo '(all)
  org-icalendar-use-scheduled '(event-if-todo event-if-not-todo)
  org-icalendar-use-deadline '(event-if-todo event-if-not-todo)
  )

(after! persp-mode
  ;; (setq persp-kill-foreign-buffer-action nil)
  ;; collect names of all brain files
  (setq +persp-blacklist (append
                          `,(directory-files (concat org-brain-path "private_brain"))
                          `,(directory-files org-brain-path)))
  ;; TODO
  ;; collect names of all interactively used brain files
  ;; this should be some data structure which would hold list of whitelisted files per perspective
  (setq +persp-whitelist nil)
  (setq persp-emacsclient-init-frame-behaviour-override 'persp-ignore-wconf)
  )

(after! profiler
  (set-popup-rule! "^.*-Profiler-Report.*$"         :size 0.4 :side 'right :select t))

(after! projectile
  (setq projectile-globally-ignored-file-suffixes (append (list ".elc"))
        projectile-globally-ignored-directories (append (list "node_modules"))
        projectile-track-known-projects-automatically nil
        counsel-projectile-sort-projects t
        projectile-ignored-projects nil ))

(after! prodigy
  (prodigy-define-service
    :name "Gulp"
    :command "gulp"
    :cwd (projectile-project-root)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(after! prog-mode
  (add-hook! 'prog-mode-hook 'goto-address-mode))

(after! python
  (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python_3")))))

(after! recentf
  (advice-add #'recentf-cleanup :around #'doom*shut-up))

(after! synosaurus
  (set-popup-rule! "*Synonyms List\*"               :size 0.4 :side 'top :select t))

(after! term
  (add-hook! 'term-mode-hook #'hide-mode-line-mode)
  ;; remap keys for terminal with Evil
  (add-hook! :append term-mode 'aj/set-term-keys)
  (add-hook 'term-mode-hook '(lambda () (interactive)(setq left-fringe-width 0
                                                           right-ringe-width 0))))

(after! tide
  (setq tide-completion-detailed nil
        tide-always-show-documentation nil))

(after! treemacs
  (setq evil-treemacs-state-cursor 'box)
  (setq treemacs-project-follow-cleanup t)

  ;; seems like all the icons doesn't work without png icons being enabled
  (setq treemacs-no-png-images nil)
  ;; Looks actually quite good now with this size setting
  (treemacs-resize-icons 18)
  ;; Override (some?) icons with all-the-icons
  (dolist (item all-the-icons-icon-alist)
    (let* ((extension (car item))
           (icon (apply (cdr item))))
      (ht-set! treemacs-icons-hash
               (s-replace-all '(("\\" . "") ("$" . "") ("." . "")) extension)
               (concat icon " "))))

  (set-face-attribute     'treemacs-root-face nil :height 1.0)
  (add-hook 'treemacs-mode-hook 'variable-pitch-mode))

(after! web-mode
  (add-hook 'web-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "CSS" "Bootstrap 4"))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  (add-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (setq web-mode-enable-current-element-highlight t)
  (set-face-attribute 'web-mode-current-element-highlight-face nil :background "#21242b" :foreground "#51afef")
  (set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "#5B6268")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#5B6268")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#E06C75")
  (set-face-attribute 'web-mode-html-tag-unclosed-face nil :inherit 'web-mode-html-tag-face :underline '(:color "#ff6c6b" :style wave)))

(after! which-key
  (setq which-key-idle-delay 0.8
        which-key-allow-regexps nil
        which-key-allow-evil-operators 1))

(after! wordnut
  (set-popup-rule! "*WordNut\*"                     :size 0.4 :side 'top :select t))

(after! yasnippet
  (push "~/org/snippets" yas-snippet-dirs))

(defhydra aj/agenda ( :body-pre
                      (aj/org-agenda-clever)
                      ;; (progn
                      ;;   ;; (org-agenda nil "g")
                      ;;   ;; (aj/remap-keys-for-org-agenda) ;; remap keys for org agenda
                      ;;   ;; (projectile-project-root)
                      ;;   )
                      :color blue)
  "Agenda"
  ("a" (org-agenda nil "a") "Agenda")
  ("p" (org-agenda nil "P") "Projects Overview")
  ("i" (org-agenda nil "i") "inbox")
  ("t" (org-agenda nil "T") "Tasks Overview")
  )

(defhydra aj/agenda-hydra (:color blue )
  "Agenda:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  ("r" (aj/gtd-review-refile/body) "refile")
  )

(defhydra aj/clocking (:color blue)
  "Clock:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  ("s" (org-clock-out) "stop clock")
  )

(defhydra aj/capture ()
  "Capture:"
  ("k" (org-capture nil "gi") "inbox" :exit t)
  ("m" (org-capture nil "gm") "morning" :exit t)
  ("w" (org-capture nil "gw") "work:" :exit t)
  ("l" (org-capture nil "gl") "lunch:" :exit t)
  ("o" (org-capture nil "go") "outside:" :exit t)
  ("e" (org-capture nil "ge") "evening" :exit t)
  ("s" (org-capture nil "gs") "sleep" :exit t)
  ("j" (org-capture nil "e") "journal Entry" :exit t)
  ("c" (org-capture nil "c") "Calendar:" :exit t)
  ("p" (org-capture nil "P") "project Task:" :exit t)
  ("n" (org-capture nil "J") "project Journal:" :exit t)
  )

(defhydra aj/gtd-goto (:color blue)
  "GTD file:"
  ("g" (aj/goto-GTD) "GTD" )
  ("j" (aj/goto-journal) "journal" )
  ("m" (aj/goto-gtd-manual) "manual")
  )

(defhydra aj/gtd-refile (:color blue)
  "GTD Refile:"
  ("m" (aj/refile-to-file-headline +GTD "MORNING") "morning")
  ("w" (aj/refile-to-file-headline +GTD "WORK") "work")
  ("l" (aj/refile-to-file-headline +GTD "LUNCH") "lunch")
  ("e" (aj/refile-to-file-headline +GTD "EVENING") "evening")
  ("o" (aj/refile-to-file-headline +GTD "OUTSIDE") "outside")
  ("s" (aj/refile-to-file-headline +GTD "SLEEP") "sleep")
  ("c" (aj/refile-to-file-headline +GTD "CALENDAR") "calendar")

  ;; ("s" (aj/gtd-someday-refile/body) "someday")
  ;; ("m" (aj/refile-to-file-headline +MAYBE "Ideas") "maybe")
  )

;; (defhydra aj/gtd-someday-refile ()
;;   "SOMEDAY:"
;;   ("b" (aj/refile-to-file-headline +SOMEDAY "Build" )     "build" )
;;   ("B" (aj/refile-to-file-headline +SOMEDAY "Buy" )       "Buy" )
;;   ("c" (aj/refile-to-file-headline +SOMEDAY "Configure" ) "configure" )
;;   ("d" (aj/refile-to-file-headline +SOMEDAY "Do" )        "do" )
;;   ("g" (aj/refile-to-file-headline +SOMEDAY "Go" )        "go" )
;;   ("h" (aj/refile-to-file-headline +SOMEDAY "Habit" )    "habit" )
;;   ("l" (aj/refile-to-file-headline +SOMEDAY "Learn" )     "learn" )
;;   ("L" (aj/refile-to-file-headline +SOMEDAY "Listen" )    "Listen" )
;;   ("m" (aj/refile-to-file-headline +SOMEDAY "MOC" )       "moc" )
;;   ("p" (aj/refile-to-file-headline +SOMEDAY "Program" )   "program" )
;;   ("r" (aj/refile-to-file-headline +SOMEDAY "Read" )      "read" )
;;   ("W" (aj/refile-to-file-headline +SOMEDAY "Watch" )     "Watch" )
;;   ("w" (aj/refile-to-file-headline +SOMEDAY "Write" )     "write" )
;;   )

(defhydra aj/wiki-select (:color blue)
  "Goto:"
  ("g" (progn
         (widen)
         (org-set-visibility-according-to-property)
         (outline-show-branches)
         (counsel-org-goto-private-wiki))  "goto:" :exit t)
  ("v" (org-brain-visualize (org-brain-entry-at-pt)) "visualize:" :exit t)
  ("f" (lambda () (interactive) (progn (widen) (swiper))) "find:" :exit t)
  ("p" (mixed-pitch-mode) "Pretty" :exit t)
  ("d" (org-decrypt-entries) "Decrypt entries" :exit t)
  )

