;;; ~/.doom.d/+def.el -*- lexical-binding: t; -*-
(def-package! avy
  :commands (avy-goto-char-2 avy-goto-line avy-org-refile-as-child)
  :config
  (setq avy-all-windows nil
        avy-background t))
(def-package! outline-magic
  )
(def-package! highlight-blocks)
(def-package! hungry-delete
  :commands hungry-delete-backward
  )
(def-package! find-file-in-project)
(def-package! gulp-task-runner
  :commands gulp
  )
(def-package! ivy-yasnippet
  :disabled
  :commands (ivy-yasnippet))
(def-package! all-the-icons-ivy
  :after ivy
  :config
  (all-the-icons-ivy-setup)
  (ivy-set-display-transformer '+ivy/switch-workspace-buffer 'all-the-icons-ivy-buffer-transformer)
  (ivy-set-display-transformer 'counsel-projectile-find-file 'all-the-icons-ivy-file-transformer)
  (ivy-set-display-transformer 'counsel-file-jump 'all-the-icons-ivy-file-transformer)
  (ivy-set-display-transformer 'counsel-dired-jump 'all-the-icons-ivy-file-transformer)
  )
(def-package! ereader
  :after org
  :commands org-ebook-open
  )
;; (def-package! xml+)
(def-package! cheatsheet
  :disabled
  :commands (
             cheatsheet-add
             cheatsheet-add-group
             cheatsheet-get
             cheatsheet-show
             )
  )
(def-package! x-path-walker
  :commands (helm-x-path-walker)
  :config
  (setq helm-display-function #'helm-posframe-display
        helm-posframe-buffer nil)
  (defun helm-posframe-display (buffer &optional _resume)
    (posframe-show
     (setq helm-posframe-buffer buffer)
     :poshandler #'posframe-poshandler-frame-center
     :left-fringe 10
     :width (window-width)
     :height 40
     :respect-header-line t))

  (defun helm-posframe-cleanup ()
    (posframe-hide helm-posframe-buffer))

  (add-hook 'helm-cleanup-hook #'helm-posframe-cleanup)
  )
(def-package! qml-mode
  :commands (qml-mode)
  )
(def-package! apache-mode
  :commands apache-mode
  :init
  (autoload 'apache-mode "apache-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
  (add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
  (add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
  (add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
  (add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
  )
(def-package! robots-txt-mode
  :commands robots-txt-mode
  :init
  (add-to-list 'auto-mode-alist '("/robots\\.txt\\'" . robots-txt-mode))
  )
(def-package! systemd)
(def-package! fish-mode
  :commands fish-mode)
(def-package! postcss-sorting
  :disabled
  )
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
    ("o" (aj-mpdel-playlist-open) "open" :color blue)
    )
  )
(def-package! ivy-mpdel
  :disabled
  :config
  (set! :popup "*MPDEL Current Playlist*" '((size . 0.4) (side . left)) '((select . t) (transient . nil)))
  )
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
    ("r" (podcaster-resume) "resume"))
  )
(def-package! emms
  :disabled
  :commands emms
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  )
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
  (exwm-randr-enable)
  )
(def-package! sdcv)
(def-package! define-word)
