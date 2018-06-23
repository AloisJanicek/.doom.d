;;;  -*- lexical-binding: t; -*-

(def-package! racket-mode
  :config
  (set-repl-handler! 'racket-mode #'+racket/repl)
  (set-popup-rule! "*Racket REPL\*" :quit nil :ttl nil :side 'bottom :size 0.4)
  )

(def-package! ob-racket
  :after (racket-mode org)
  :config
  (setq org-babel-racket-command "/usr/bin/racket")
  )
