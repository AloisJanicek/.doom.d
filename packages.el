;; -*- no-byte-compile: t; -*-
;;; packages.el

;; until it gets merged
(package! flycheck :recipe (:fetcher github :repo "flycheck/flycheck" :branch "fix-1398-quoted-lambdas"))

(package! fish-mode)
(package! apache-mode)
(package! robots-txt-mode)
(package! systemd)
(package! find-file-in-project :disable t)
(package! hungry-delete)
(package! gulp-task-runner :disable t)
(package! flyspell-lazy :disable t)

(package! ob-javascript :recipe (:fetcher github :repo "zweifisch/ob-javascript" :files ("*")))
(package! ob-async :recipe (:fetcher github :repo "astahlman/ob-async"))
(package! emacs-ereader :recipe (:fetcher github :repo "bddean/emacs-ereader" :files ("org-ebook.el" "ereader.el")))
(package! org-starter :recipe (:fetcher github :repo "akirak/org-starter"))
(package! org-super-agenda :disable t)
(package! org-projectile)
(package! org-pomodoro)
(package! org-pdfview)
(package! org-brain)
(package! org-edna :disable t)
(package! ox-hugo :disable t)
(package! powerthesaurus :recipe (:fetcher github :repo "SavchenkoValeriy/emacs-powerthesaurus"))
(package! cheatsheet :disable t)
(package! x-path-walker :disable t)
(package! outline-magic :disable t)
(package! highlight-blocks)
(package! ivy-yasnippet :recipe (:fetcher github :repo "mkcms/ivy-yasnippet"))


(package! sdcv :recipe (:fetcher github :repo "stardiviner/sdcv.el" :files ("sdcv.el")))
(package! define-word)
(package! all-the-icons-ivy)

(package! xml+)
(package! other-frame-window :disable t)
(package! link-hint)
(package! mpdel :recipe (:fetcher github :repo "mpdel/mpdel") :disable t)
(package! ivy-mpdel :recipe (:fetcher github :repo "mpdel/ivy-mpdel") :disable t)
(package! podcaster :disable t)
(package! emms :disable t)
(package! exwm :disable t)
(package! plain-org-wiki :recipe (:fetcher github :repo "AloisJanicek/plain-org-wiki") :disable t)
