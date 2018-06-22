;;; ~/.doom.d/+hydras.el -*- lexical-binding: t; -*-
(defhydra aj/gtd-refile (:color blue)
  "GTD Refile:"
  ("t" (aj/refile-to-file-headline +GTD "TASKS") "task")
  ("p" (aj/refile-to-file-headline +GTD "PROJECTS") "project")
  ("h" (aj/refile-to-file-headline +GTD "HABITS") "habit")
  ("r" (aj/refile-to-file-headline +GTD "REOCCURRING") "reoccurring")
  ("c" (aj/refile-to-file-headline +GTD "CALENDAR") "calendar")
  ("f" (aj/refile-to-file-headline +GTD "FINANCE") "finance")
  ("s" (aj/gtd-someday-refile/body) "someday")
  ("m" (aj/refile-to-file-headline +MAYBE "Ideas") "maybe")
  )

(defhydra aj/gtd-someday-refile ()
  "SOMEDAY:"
  ("b" (aj/refile-to-file-headline +SOMEDAY "Build" )     "build" )
  ("B" (aj/refile-to-file-headline +SOMEDAY "Buy" )       "Buy" )
  ("c" (aj/refile-to-file-headline +SOMEDAY "Configure" ) "configure" )
  ("d" (aj/refile-to-file-headline +SOMEDAY "Do" )        "do" )
  ("g" (aj/refile-to-file-headline +SOMEDAY "Go" )        "go" )
  ("h" (aj/refile-to-file-headline +SOMEDAY "Habit" )    "habit" )
  ("l" (aj/refile-to-file-headline +SOMEDAY "Learn" )     "learn" )
  ("L" (aj/refile-to-file-headline +SOMEDAY "Listen" )    "Listen" )
  ("m" (aj/refile-to-file-headline +SOMEDAY "MOC" )       "moc" )
  ("p" (aj/refile-to-file-headline +SOMEDAY "Program" )   "program" )
  ("r" (aj/refile-to-file-headline +SOMEDAY "Read" )      "read" )
  ("W" (aj/refile-to-file-headline +SOMEDAY "Watch" )     "Watch" )
  ("w" (aj/refile-to-file-headline +SOMEDAY "Write" )     "write" )
  )

(defhydra aj/gtd-goto (:color blue)
  "GTD file:"
  ("g" (aj/goto-GTD) "GTD" )
  ("j" (aj/goto-journal) "journal" )
  ("s" (aj/goto-someday) "someday" )
  ("m" (aj/goto-maybe) "maybe" )
  )


(defhydra aj/agenda-hydra (:color blue )
  "Agenda:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  ("r" (aj/gtd-review-refile/body) "refile")
  )

(defhydra aj/agenda ( :body-pre
                      (progn
                        ;; (org-agenda nil "g")
                        ;; (aj/remap-keys-for-org-agenda) ;; remap keys for org agenda
                        ;; (projectile-project-root)
                        )
                      :color blue)
  "Agenda"
  ("a" (org-agenda nil "a") "Agenda")
  ("p" (org-agenda nil "P") "Projects Overview")
  ("t" (org-agenda nil "T") "Tasks Overview")
  )

(defhydra aj/clocking (:color blue)
  "Clock:"
  ("c" (aj/clock-menu) "clock" )
  ("p" (org-pomodoro) "pomodoro" )
  ("s" (org-clock-out) "stop clock")
  )

(defhydra aj/capture ()
  "Capture:"
  ("k" (org-capture nil "e") "journal Entry" :exit t)
  ("t" (org-capture nil "t") "Task:" :exit t)
  ("p" (org-capture nil "P") "project Task:" :exit t)
  ("j" (org-capture nil "J") "project Journal:" :exit t)
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

(defhydra aj/switch-mode-menu (:color blue)
  "Select mode:"
  ("o" (org-mode) "Org" )
  ("e" (emacs-lisp-mode) "Elisp" )
  )
