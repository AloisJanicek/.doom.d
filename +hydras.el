;;; ~/.doom.d/+hydras.el -*- lexical-binding: t; -*-
(defhydra aj/scratch-menu (:color blue)
  "Select mode:"
  ("o" (org-mode) "Org" )
  ("e" (emacs-lisp-mode) "Elisp" )
  )


(defhydra aj/agenda-hydra (:color blue )
  "Agenda:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  ("r" (aj/gtd-review-refile/body) "refile")
  )

(defhydra aj/clocking (:color blue)
  "Clock:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  ("s" (org-clock-out) "stop clock")
  )

(defhydra aj/agenda ( :body-pre
                      (progn
                        (org-agenda nil "g")
                        (aj/remap-keys-for-org-agenda) ;; remap keys for org agenda
                        (projectile-project-root)
                        )
                      :color blue)
  "Agenda"
  ("c" (org-agenda nil "c") "Coding")
  )

(defhydra aj/capture-clocked-task ()
  "Capture clocked task:"
  ("p" (org-capture nil "cp") "Personal" :exit t)
  ("w" (org-capture nil "cw") "Work" :exit t)
  ("e" (org-capture nil "ce") "Environment" :exit t)
  ("d" (org-capture nil "cd") "eDucation" :exit t))

(defhydra aj/capture-task ()
  "Capture task:"
  ("p" (org-capture nil "tp") "Personal" :exit t)
  ("w" (org-capture nil "tw") "Work" :exit t)
  ("e" (org-capture nil "te") "Environment" :exit t)
  ("d" (org-capture nil "td") "eDucation" :exit t))

(defhydra aj/capture-interuption ()
  "Capture interuption:"
  ("p" (org-capture nil "ip") "Personal" :exit t )
  ("w" (org-capture nil "iw") "Work" :exit t )
  ("e" (org-capture nil "ie") "Environment" :exit t )
  ("d" (org-capture nil "id") "eDucation" :exit t )
  )

(defhydra aj/capture-caffeine ()
  "Caffeine strength:"
  ("w" (org-capture nil "cw") "Weak" :exit t)
  ("s" (org-capture nil "cs") "Strong" :exit t)
  )

(defhydra aj/capture-statistics ()
  "Capture log stuff:"
  ("w" (org-capture nil "sw") "Weight" :exit t)
  ("f" (org-capture nil "sf") "Food" :exit t)
  ("c" (aj/capture-caffeine/body) "Caffeine" :exit t)
  )

(defhydra aj/capture-journal ()
  "Capture journal:"
  ("p" (org-capture nil "jp") "Private" :exit t)
  ("w" (org-capture nil "jw") "Work" :exit t)
  ("d" (org-capture nil "jd") "eDucation" :exit t)
  ("e" (org-capture nil "je") "Environment" :exit t)
  )

(defhydra aj/capture ()
  "Capture:"
  ("j" (aj/capture-journal/body) "Journal:" :exit t)
  ("t" (aj/capture-task/body) "Task:" :exit t)
  ("c" (aj/capture-clocked-task/body) "Clocked task:" :exit t)
  ("i" (aj/capture-interuption/body) "Interruption:" :exit t)
  ("s" (aj/capture-statistics/body) "Statistics:" :exit t)
  ("n" (org-capture nil "n") "Note clock:" :exit t)
  )

(defhydra aj/wiki-select (:color blue)
  "Goto:"
  ("g" (progn
         (widen)
         (org-set-visibility-according-to-property)
         (outline-show-branches)
         (counsel-org-goto-private-wiki))  "goto:" :exit t)
  ("v" (org-brain-visualize (org-brain-entry-at-pt)) "visualize:" :exit t)
  ("f" (lambda () (interactive) (progn (widen) (swiper))) "find:" :exit t)
  ("p" (mixed-pitch-mode) "Pretty" :exit t)
  ("d" (org-decrypt-entries) "Decrypt entries" :exit t)
  )

(defhydra aj/gtd-review-refile ()
  "Refile"
  ("p" (aj/refile-to-file-headline "~/org/gtd.org" "PERSONAL") "PERSONAL")
  ("w" (aj/refile-to-file-headline "~/org/gtd.org" "WORK") "WORK")
  ("e" (aj/refile-to-file-headline "~/org/gtd.org" "ENVIRONMENT") "ENVIRONMENT")
  ("d" (aj/refile-to-file-headline "~/org/gtd.org" "EDUCATION") "EDUCATION")
  )
