;; -*- no-byte-compile: t; -*-
;;; packages.el

;; until it gets merged
(package! all-the-icons-ivy
  )

(package! apache-mode
  )

(package! cheatsheet
  :disable t)

(package! define-word
  )

(package! emacs-ereader
  :recipe (:fetcher github :repo "bddean/emacs-ereader" :files ("org-ebook.el" "ereader.el")))

(package! emms
  :disable t)

(package! exwm
  :disable t)

(package! find-file-in-project
  :disable t)

(package! fish-mode
  )

;; (package! flycheck
;;   :recipe (:fetcher github :repo "flycheck/flycheck" :branch "fix-1398-quoted-lambdas"))

(package! flyspell-lazy
  :disable t)

(package! gulp-task-runner
  :disable t)

(package! highlight-blocks
  )

(package! hungry-delete
  )

(package! ivy-mpdel
  :recipe (:fetcher github :repo "mpdel/ivy-mpdel") :disable t)

(package! ivy-yasnippet
  :recipe (:fetcher github :repo "mkcms/ivy-yasnippet"))

(package! link-hint
  )

(package! mpdel
  :recipe (:fetcher github :repo "mpdel/mpdel") :disable t)

(package! ob-async
  :recipe (:fetcher github :repo "astahlman/ob-async"))

(package! ob-javascript
  :recipe (:fetcher github :repo "zweifisch/ob-javascript" :files ("*")))

(package! org-brain
  )

(package! org-edna
  :disable t)

(package! org-pdfview
  )

(package! org-pomodoro
  )

(package! org-starter
  :recipe (:fetcher github :repo "akirak/org-starter"))

(package! org-super-agenda
  )

(package! other-frame-window
  :disable t)

(package! outline-magic
  :disable t)

(package! ox-hugo
  :disable t)

(package! plain-org-wiki
  :recipe (:fetcher github :repo "AloisJanicek/plain-org-wiki") :disable t)

(package! podcaster
  :disable t)

(package! powerthesaurus
  :recipe (:fetcher github :repo "SavchenkoValeriy/emacs-powerthesaurus"))

(package! robots-txt-mode
  )

(package! sdcv
  :recipe (:fetcher github :repo "stardiviner/sdcv.el" :files ("sdcv.el")))

(package! systemd
  )

(package! x-path-walker
  ;; yay -S python-lxml
  )

(package! xml+)
