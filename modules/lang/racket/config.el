;;;  -*- lexical-binding: t; -*-

(def-package! racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :config
  (set-repl-handler! 'racket-mode #'+racket/repl)
  (set-popup-rule! "*Racket REPL\*" :quit nil :ttl nil :side 'bottom :size 0.4)
  (set-popup-rule! "*Racket Describe*"        :select t :side 'left  :size 0.4)

  (fset 'racket--do-describe 'aj/racket--do-describe-for-company)
  (evil-set-initial-state 'racket-repl-mode 'insert)

  (add-hook! '(racket-mode-hook racket-repl-mode-hook)
    (rainbow-delimiters-mode +1)
    (custom-set-faces `(racket-selfeval-face ((t (:foreground ,(doom-color 'numbers) :weight bold)))))
    (custom-set-faces `(racket-keyword-argument-face ((t (:foreground ,(doom-color 'dark-cyan))))))
    )

  ;; wrap lines so you can see whole thing without horizontal scroll and $ symbols everywhere
  (add-hook 'racket-describe-mode-hook 'visual-line-mode)

  (map!
   :map racket-repl-mode-map
   "C-k"  #'evil-window-up
   "C-h"  #'evil-window-left
   :map racket-describe-mode-map
   :nv "o" #'link-hint-open-link
   :map racket-mode-map
   :localleader

   ;; when in normal mode, evaluate whole buffer
   :desc    "run"                   :n    "r"     #'racket-run

   ;; otherwise send region to REPL
   :desc    "run/send-region"       :v    "r"     #'racket-send-region
   :desc    "send-definition"       :nv   "d"     #'racket-send-definition
   :desc    "send-last-sexp"        :nv   "s"     #'racket-send-last-sexp

   ;; require offline racket documentation
   ;; settings:
   ;; open =drracket=
   ;; Preferences -> Browsers -> [/usr/bin/xdg-open " ] <URL> ["     ]
   :desc    "doc"                   :nv   "h"     #'racket-doc
   :desc    "describe"              :nv   "."     #'racket-describe
   ;; code manipulation -------------
   ;; racket-backward-up-list
   ;; racket-cycle-paren-shapes
   ;; racket-unalign
   ;; racket-align

   ;; language tooling -------------
   ;; racket-base-requires
   ;; racket-find-collection


   ;; mode specific ----------------
   ;; racket-bug-report

   ;; syntax check mode ------------------ evilize it
   :desc "check syntax"         :nv   "c"     #'racket-check-syntax-mode
   ;; racket-check-syntax-mode-goto-def
   ;; racket-check-syntax-mode-goto-next-def
   ;; racket-check-syntax-mode-goto-next-use
   ;; racket-check-syntax-mode-goto-prev-def
   ;; racket-check-syntax-mode-goto-prev-use
   ;; racket-check-syntax-mode-help
   ;; racket-check-syntax-mode-quit
   ;; racket-check-syntax-mode-rename

   ;; expand racket macros and send them to repl?
   ;; racket-expand-again
   ;; racket-expand-definition
   ;; racket-expand-last-sexp
   ;; racket-expand-region

   ;; tests
   :desc    "tests"                   :nv   "t"     #'racket-test
   :desc    "raco-tests"              :nv   "T"     #'racket-raco-test
   ;; racket-test
   ;; racket-raco-test
   :desc    "fold-all-tests"              :nv   "f"     #'racket-fold-all-tests
   :desc    "unfold-all-tests"              :nv   "F"     #'racket-unfold-all-tests
   ;; racket-unfold-all-tests


   ;; probably don't need those
   ;; racket-indent-line
   ;; racket-insert-closing
   ;; racket-insert-lambda
   ;; racket-smart-open-bracket

   ;; loger
   :desc    "logger"              :nv   "l"     #'racket-logger
   ;; racket-logger-clear
   ;; racket-logger-exit
   ;; racket-logger-mode
   ;; racket-logger-mode-menu
   ;; racket-logger-next-item
   ;; racket-logger-previous-item
   ;; racket-logger-topic-level

   ;; racket-mode
   ;; racket-mode-menu

   ;; racket-profile-mode
   :desc    "profile"              :nv   "o"     #'racket-profile

   ;; run current file in shell with racket <file.rkt>
   :desc    "run with racket"              :nv   "R"     #'racket-racket

   ;; racket-repl-mode
   ;; racket-repl
   ;; racket-repl--clean-image-cache
   ;; racket-repl-eval-or-newline-and-indent
   ;; racket-repl-switch-to-edit
   ;; racket-repl-mode-menu

   ;; racket-run-and-switch-to-repl
   ;; racket-run-with-errortrace

   ;; requires -----------
   ;; racket-tidy-requires
   ;; racket-trim-requires
   ;; racket-open-require-path

   ;; racket-unicode-input-method-enable

   ;; racket-view-last-image

   (:desc "visit" :prefix "v"
     :desc    "definition"                   :nv   "d"     #'racket-visit-definition
     :desc    "module"                   :nv   "m"     #'racket-visit-module

     ;; visit
     ;; racket-visit-definition
     ;; racket-visit-module
     ;; racket-unvisit
     )
   )
  )
;; (setq +eval-runners nil)
(def-package! ob-racket
  :after (racket-mode org)
  :config
  (setq org-babel-racket-command "/usr/bin/racket")
  )
