;; -*- no-byte-compile: t; -*-
;;; packages.el

;; until it gets merged
(package! flycheck :recipe (:fetcher github :repo "flycheck/flycheck" :branch "fix-1398-quoted-lambdas"))

(package! fish-mode)
(package! apache-mode)
(package! robots-txt-mode)
(package! systemd)
(package! find-file-in-project)
(package! hungry-delete)
(package! gulp-task-runner)
(package! flyspell-lazy)

(package! all-the-icons-ivy)
;; (package! org-super-agenda)
(package! org-projectile)
(package! org-pomodoro)
(package! org-pdfview)
(package! org-brain)
;; (package! org-edna)
;; (package! ox-hugo)
(package! powerthesaurus :recipe (:fetcher github :repo "SavchenkoValeriy/emacs-powerthesaurus"))
(package! qml-mode)
(package! cheatsheet)
(package! x-path-walker)
(package! outline-magic)
(package! highlight-blocks)
(package! ivy-yasnippet :recipe (:fetcher github :repo "mkcms/ivy-yasnippet"))

(package! ob-javascript :recipe (:fetcher github :repo "zweifisch/ob-javascript" :files ("*")))
(package! ob-async :recipe (:fetcher github :repo "astahlman/ob-async" ))
(package! ereader :recipe (:fetcher url :url "https://raw.githubusercontent.com/bddean/emacs-ereader/master/ereader.el"))
(package! org-ebook :recipe (:fetcher url :url "https://raw.githubusercontent.com/bddean/emacs-ereader/master/org-ebook.el"))
(package! sdcv :recipe (:fetcher url :url "https://raw.githubusercontent.com/stardiviner/sdcv.el/master/sdcv.el"))
(package! define-word)
(package! xml+)
(package! other-frame-window)
(package! link-hint)

;; (package! prescient)
;; (package! ivy-prescient)
;; (package! company-prescient)

;; (package! xah-css-mode)

;; (package! mpdel :recipe (:fetcher github :repo "mpdel/mpdel"))
;; (package! ivy-mpdel :recipe (:fetcher github :repo "mpdel/ivy-mpdel"))
;; (package! podcaster)
;; (package! emms)
;; (package! exwm)

;; (package! plain-org-wiki :recipe (:fetcher github :repo "AloisJanicek/plain-org-wiki"))
