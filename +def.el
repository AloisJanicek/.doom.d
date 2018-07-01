;;; ~/.doom.d/+def.el -*- lexical-binding: t; -*-

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
