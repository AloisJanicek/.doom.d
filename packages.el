;; -*- no-byte-compile: t; -*-
;;; packages.el

(package! fish-mode)
(package! apache-mode)
(package! robots-txt-mode)
(package! systemd)
(package! find-file-in-project)
(package! hungry-delete)
(package! gulp-task-runner)
(package! flyspell-lazy)

(package! org-fancy-priorities)
(package! org-super-agenda)
(package! org-projectile)
(package! org-pomodoro)
(package! org-pdfview)
(package! ob-browser)
(package! org-brain)
(package! org-edna)
(package! ox-hugo)
(package! ox-epub)
(package! powerthesaurus :recipe (:fetcher github :repo "SavchenkoValeriy/emacs-powerthesaurus"))
(package! qml-mode)
(package! cheatsheet)
(package! x-path-walker)
(package! outline-magic)
(package! highlight-blocks)
(package! ivy-yasnippet :recipe (:fetcher github :repo "mkcms/ivy-yasnippet"))

(package! ob-javascript :recipe (:fetcher github :repo "zweifisch/ob-javascript" :files ("*")))
(package! ob-async :recipe (:fetcher github :repo "astahlman/ob-async" ))
(package! ereader :recipe (:fetcher github :repo "bddean/emacs-ereader" :files ("*.el")))

;; (package! org-sidebar :recipe (:fetcher github :repo "alphapapa/org-sidebar" :files ("org-sidebar.el")))
;; (package! org-agenda-ng :recipe (:fetcher github :repo "alphapapa/org-agenda-ng" :files ("*.el")))

;; (package! xah-css-mode)

;; (package! mpdel :recipe (:fetcher github :repo "mpdel/mpdel"))
;; (package! ivy-mpdel :recipe (:fetcher github :repo "mpdel/ivy-mpdel"))
;; (package! podcaster)
;; (package! emms)
;; (package! exwm)

;; (package! plain-org-wiki :recipe (:fetcher github :repo "AloisJanicek/plain-org-wiki"))
