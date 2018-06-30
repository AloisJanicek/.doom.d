;;; ~/.doom.d/+def.el -*- lexical-binding: t; -*-
(def-package! outline-magic
  :commands (outline-cycle outline-next-line outline-move-subtree-up outline-move-subtree-down outline-promote outline-demote))

(def-package! highlight-blocks
  :commands (highlight-blocks-mode highlight-blocks-now))

(def-package! hungry-delete
  :demand t
  :config
  (setq hungry-delete-except-modes
        '(term-mode help-mode minibuffer-inactive-mode calc-mode))
  (global-hungry-delete-mode 1))

(def-package! find-file-in-project
  :commands (ffip ffip-show-diff))

(def-package! gulp-task-runner
  :commands gulp)

(def-package! ivy-yasnippet
  :commands (ivy-yasnippet))

(def-package! all-the-icons-ivy
  :after ivy
  :config
  (dolist (cmd '( counsel-find-file counsel-file-jump
                                    counsel-dired-jump counsel-projectile-find-dir
                                    counsel-projectile-switch-project))
    (ivy-set-display-transformer cmd #'all-the-icons-ivy-file-transformer)))

(def-package! xml+
  :commands (xml+-query--generic xml+-query-all xml+-query-first xml+-node-text xml+-node-text--helper))

(def-package! cheatsheet
  :commands (cheatsheet-add cheatsheet-add-group cheatsheet-get cheatsheet-show))

(def-package! x-path-walker
  :commands (helm-x-path-walker))

(def-package! apache-mode
  :mode (("apache\\.conf\\'" . apache-mode)
         ("\\.htaccess\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode)
         ("srm\\.conf\\'"    . apache-mode)
         ("access\\.conf\\'" . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

(def-package! robots-txt-mode
  :mode (("/robots\\.txt\\'" . robots-txt-mode)))

(def-package! systemd
  :commands (systemd-mode))

(def-package! fish-mode
  :commands (fish-mode))

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

(def-package! ivy-mpdel
  :disabled
  :config
  (set-popup-rule! "*MPDEL Current Playlist*"       :size 0.4 :side 'left :select t :transient nil))

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

(def-package! sdcv
  :commands (sdcv-search-input sdcv-search-pointer)
  :config
  (set-popup-rule! "*SDCV\*" :size 0.4 :side 'top :select t))

(def-package! define-word
  :commands (define-word  define-word-at-point))

(def-package! other-frame-window)

(def-package! link-hint
  :commands (link-hint-open-all-links
             link-hint-copy-all-links
             link-hint-open-link
             link-hint-copy-link))

(def-package! plain-org-wiki
  :config
  (setq pow-directory "~/org/brain"))
