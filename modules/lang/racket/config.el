;;;  -*- lexical-binding: t; -*-

(def-package! racket-mode
  :config
  (set-repl-handler! 'racket-mode #'+racket/repl)
  )

(def-package! ob-racket
  :after (racket-mode org)
  :config
  (setq org-babel-racket-command "/usr/bin/racket")
  )
