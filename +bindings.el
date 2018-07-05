;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

(map!
 ;; global
 (:map global-map
   :nimve "M-1"   (λ! (+workspace/switch-to 0))
   :nimve "M-2"   (λ! (+workspace/switch-to 1))
   :nimve "M-3"   (λ! (+workspace/switch-to 2))
   :nimve "M-4"   (λ! (+workspace/switch-to 3))
   :nimve "M-5"   (λ! (+workspace/switch-to 4))
   :nimve "M-6"   (λ! (+workspace/switch-to 5))
   :nimve "M-7"   (λ! (+workspace/switch-to 6))
   :nimve "M-8"   (λ! (+workspace/switch-to 7))
   :nimve "M-9"   (λ! (+workspace/switch-to 8))
   :nimve "M-0"   #'+workspace/switch-to-last
   :nimve "M-t"   #'+workspace/new

   :i     "C-'"       #'forward-char
   :i     "C-;"       #'backward-char
   :invme "H-o"       #'other-frame
   "C-s"       #'ispell-word
   :inv   "M-y"       #'counsel-yank-pop
   "C-="       #'recenter-top-bottom
   "C-\\"      #'move-to-window-line-top-bottom
   "C-<right>" #'next-buffer
   "C-<left>"  #'previous-buffer
   "M-p"       #'ivy-yasnippet
   :ne    "M-f"       #'aj/my-swiper
   "<f2>"      #'which-key-show-top-level
   "<f3>"      #'which-key-show-major-mode
   "<f4>"      #'which-key-show-minor-mode-keymap
   "<f5>"      #'which-key-show-keymap
   (:prefix "g"
     :n "2" #'avy-goto-char-2
     :n "h" #'avy-goto-char-timer
     :n "j" #'avy-goto-line-below
     :n "k" #'avy-goto-line-above
     )
   )

 ;; modes
 (:after css-mode
   (:map css-mode-map
     (:localleader
       :desc "Colors"        :nv              "c" #'counsel-colors-web
       )
     )
   )
 (:after counsel
   :map ivy-minibuffer-map
   "TAB" #'ivy-alt-done
   "RET" #'ivy-done
   "C-f" #'ivy-call
   "C-d" #'ivy-immediate-done
   )
 (:after emmet
   (:map emmet-mode-keymap
     :i "M-r" #'aj/mark-region-and-preview-emmet
     :i "M-E" #'emmet-expand-yas
     :i "M-e" #'emmet-expand-line
     )
   )
 (:after flycheck
   :map flycheck-error-list-mode-map
   :ne                                        "j" #'flycheck-error-list-next-error
   :ne                                        "k" #'flycheck-error-list-previous-error
   )
 (:after inferior-python
   (:map inferior-python-mode-map
     :ienv "C-l" #'evil-window-up
     )
   )
 (:after info
   (:map Info-mode-map
     :nve                                     "o"      #'ace-link-info
     )
   )
 (:after magit
   (:map git-commit-mode-map
     (:localleader
       :desc "Finalize"        :nv            "f" #'with-editor-finish
       :desc "Finalize"        :nv            "k" #'with-editor-cancel
       )
     )
   (:map magit-mode-map
     :iemnv "C-k" #'evil-window-up
     ;; :iemnv "C-j" #'evil-window-down
     ;; :iemnv "C-h" #'evil-window-left
     ;; :iemnv "C-l" #'evil-window-right
     )
   )
 (:after man
   (:map Man-mode-map
     :nv                                      "J" #'Man-next-section
     :nv                                      "K" #'Man-previous-section
     (:localleader
       :desc "section"        :nv             "s" #'Man-goto-section
       :desc "follow"        :nv              "f" #'man-follow
       )
     ))
 (:after org
   (:map org-mode-map

     "C-]"   #'org-insert-subheading
     "<tab>" #'org-cycle
     :i   "M-r"   (λ! (let ((hydra-lv nil)) (aj/gtd-refile/body)))
     :inv "M-l"   #'aj/insert-link-in-org
     :n   "J"     #'outline-next-visible-heading
     :n   "K"     #'outline-previous-visible-heading

     (:localleader
       (:desc "decrypt:"          :prefix     "d"
         :desc "encrypt entry"     :nv        "e" #'org-encrypt-entry
         :desc "Encrytp entries"     :nv      "E" #'org-encrypt-entries
         :desc "decrypt entry"       :nv      "d" #'org-decrypt-entry
         :desc "Decrypt entries"       :nv    "D" #'org-decrypt-entry
         )
       :desc "Todo"         :nv               "t" #'org-todo
       :desc "Open"         :nv               "o" #'ace-link
       :desc "Edna"           :nv             "E" #'org-edna-edit
       :desc "Strike"         :nv             "=" #'aj-strike-through-org-headline

       (:desc "Tags"          :prefix         "g"
         :desc "Tags"           :nv           "g" #'counsel-org-tag
         :desc "Search"         :nv           "s" #'org-tags-view
         :desc "Region"         :nv           "r" #'org-change-tag-in-region
         )

       (:desc "Attach"          :prefix       "a"
         :desc "Dispatch"           :nv       "d" #'org-attach
         )
       (:desc "Archive"          :prefix      "A"
         :desc "Subtree"           :nv        "s" #'org-archive-subtree
         )

       (:desc "jump:"          :prefix        "j"
         :desc "headline"           :nv       "h" #'counsel-org-goto
         )
       :desc "Wiki"       :nv                 "w"     (λ! (progn
                                                            (widen)
                                                            (org-set-visibility-according-to-property)
                                                            (outline-show-branches)
                                                            (counsel-org-goto-private-wiki))
                                                          )
       (:desc "Refile:"       :prefix         "r"
         :desc "targets"       :nv            "t" #'org-refile
         :desc "visible"       :nv            "v" #'avy-org-refile-as-child
         :desc "Journal"        :nv           "j" (λ! (org-refile-to-datetree "~/org/JOURNAL.org"))
         :desc "GTD"            :nv           "g" (λ! (let ((hydra-lv nil)) (aj/gtd-refile/body)))
         )

       (:desc "Export"        :prefix "e"
         :desc "dispatch"    :nv       "d" #'org-export-dispatch
         (:desc "ical"        :prefix   "i"
           :desc "current buffer"    :nv "c" #'org-icalendar-export-to-ics
           :desc "agenda files"    :nv   "a" #'org-icalendar-combine-agenda-files
           )
         )
       (:desc "Clock"        :prefix          "c"
         :desc "IN"           :nv             "i" #'org-clock-in
         :desc "OUT"          :nv             "o" #'org-clock-out
         :desc "Goto"         :nv             "g" #'org-clock-goto
         :desc "Pomodoro"     :nv             "p" #'org-pomodoro
         )

       (:desc "Babel"        :prefix          "b"
         :desc "tangle"           :nv         "t" #'org-babel-tangle
         :desc "execute"          :nv         "e" #'org-babel-execute-src-block
         )

       (:desc "eXecute"        :prefix        "x"
         :desc "execute"          :nv         "x" #'org-babel-execute-src-block
         :desc "eXecute ALL"          :nv     "e" #'org-babel-execute-src-block
         )


       (:desc "Link"        :prefix           "l"
         :desc "store"            :nv         "s" #'org-store-link
         :desc "insert"           :nv         "i" #'org-insert-link
         :desc "headline"         :nv         "h" #'aj/insert-link-into-org-heading
         :desc "list"             :nv         "l" #'aj/insert-link-into-org-list-item
         :desc "open"             :nv         "o" #'org-open-at-point
         )

       (:desc "footnote"          :prefix     "f"
         :desc "action"             :nv       "a" #'org-footnote-action
         )

       (:desc "property"          :prefix     "p"
         :desc "set"              :nv         "s" #'org-set-property
         )
       (:desc "insert:"           :prefix     "i"
         :desc "id"                 :nv       "i" #'org-id-get-create
         :desc "drawer"             :nv       "d" #'org-insert-drawer
         (:desc "timestamp:"          :prefix "t"
           :desc "active"               :nv   "a" #'org-time-stamp
           :desc "inactive"             :nv   "i" #'org-time-stamp-inactive
           )
         )
       (:desc "hydras"            :prefix     "h"
         :desc "refile"            :nv           "r" (λ! (let ((hydra-lv nil)) (aj/gtd-review-refile/body)))
         )

       (:desc "Mind"          :prefix         "m"
         :desc "Visualize"    :nv             "v" #'aj/org-brain-visualize-entry-at-pt
         (:desc "Add"         :prefix         "a"
           :desc "Parent"     :nv             "p" #'org-brain-add-parent
           :desc "Child"      :nv             "c" #'org-brain-add-child
           :desc "Friend"     :nv             "f" #'org-brain-add-friendship
           :desc "Relationship"     :nv       "R" #'org-brain-add-relationship
           :desc "Resource"     :nv           "r" #'org-brain-add-resource
           )
         (:desc "Goto"         :prefix        "g"
           :desc "Parent"     :nv             "p" #'org-brain-goto-parent
           :desc "Child"      :nv             "c" #'org-brain-goto-child
           :desc "Friend"     :nv             "f" #'org-brain-goto-friend
           :desc "Current"     :nv            "C" #'org-brain-goto-current
           :desc "End"     :nv                "e" #'org-brain-goto-end
           :desc "Other window"     :nv       "o" #'org-brain-goto-other-window
           )
         (:desc "Remove"         :prefix      "r"
           :desc "Child"     :nv              "c" #'org-brain-remove-child
           :desc "Friendship"      :nv        "f" #'org-brain-remove-friendship
           :desc "Parent"     :nv             "p" #'org-brain-remove-parent
           )
         )

       (:desc "View"           :prefix        "v"
         :desc "Columns"          :nv         "c" #'org-columns
         :desc "Widen"            :nv         "w" #'widen
         :desc "Element"          :nv         "e" #'org-narrow-to-element
         :desc "Block"            :nv         "b" #'org-narrow-to-block
         :desc "Subtree"          :nv         "s" #'org-narrow-to-subtree
         :desc "Sparse tree"      :nv         "p" #'org-sparse-tree
         )
       )
     )
   (:map evil-org-mode-map
     :localleader
     :nvmoe "d" nil
     (:desc "decrypt:"          :prefix "d"
       :desc "encrypt entry"     :nv "e" #'org-encrypt-entry
       :desc "Encrytp entries"     :nv "E" #'org-encrypt-entries
       :desc "decrypt entry"       :nv "d" #'org-decrypt-entry
       :desc "Decrypt entries"       :nv "D" #'org-decrypt-entry
       )
     :nvmoe "c" nil
     (:desc "Clock"        :prefix "c"
       :desc "IN"           :nv "i" #'org-clock-in
       :desc "OUT"          :nv "o" #'org-clock-out
       :desc "Goto"         :nv "g" #'org-clock-goto
       :desc "Pomodoro"     :nv "p" #'org-pomodoro
       )
     :desc "Todo"         :nv "t" #'org-todo
     :desc "Schedule"     :nv "s" #'org-schedule
     )
   )
 (:after org-agenda
   (:map org-agenda-mode-map
     :mn                                      "t"     #'org-agenda-todo
     :mn                                      "j"     #'org-agenda-next-item
     :mn                                      "k"     #'org-agenda-previous-item
     :mn                                      "z"     #'org-agenda-view-mode-dispatch
     :mn                                      "o"     #'org-agenda-open-link
     :iemnv "C-k" #'evil-window-up
     :iemnv "C-j" #'evil-window-down
     :iemnv "C-h" #'evil-window-left
     :iemnv "C-l" #'evil-window-right
     (:prefix "d"
       :m         "s"     #'org-agenda-schedule
       :m         "r"     #'org-agenda-refile
       )
     (:prefix "c"
       :m         "t"     #'counsel-org-tag-agenda
       )
     )
   )
 (:after org-brain
   (:map org-brain-visualize-mode-map
     :ienv                                    "o" #'my/org-brain-goto-current
     :ienv                                    "J" #'link-hint-open-link
     )
   )
 (:after org-capture
   (:map org-capture-mode-map
     :inve [escape]       #'org-capture-finalize
     (:localleader
       :desc "Schedule"     :nv               "s" #'org-schedule
       :desc "Todo"         :nv               "t" #'org-todo
       :desc "View-columns" :nv               "v" #'org-columns
       :desc "Finalize"     :nv               "f" #'org-capture-finalize
       :desc "Kill"         :nv               "k" #'org-capture-kill
       :desc "Refile"       :nv               "r" #'org-capture-refile
       :desc "Clock"        :prefix           "c"
       :desc "clock-IN"     :nv               "i" #'org-clock-in
       :desc "clock-OUT"    :nv               "o" #'org-clock-out))
   )
 (:after pdf-tools
   (:map pdf-view-mode-map
     "j" #'pdf-view-next-line-or-next-page
     "k" #'pdf-view-previous-line-or-previous-page
     "l" #'org-store-link
     "i" #'counsel-imenu
     :nimve                                   "y" #'pdf-view-kill-ring-save
     )
   )
 (:after popup-buffer
   :map +popup-buffer-mode-map
   "C-l"  #'evil-window-right
   )
 (:after treemacs
   (:map treemacs-mode-map
     :iemnv "C-k" #'evil-window-up
     :iemnv "C-j" #'evil-window-down
     :iemnv "C-h" #'evil-window-left
     :iemnv "C-l" #'evil-window-right
     :iemnv "C-<SPC>" #'treemacs-peek
     )
   )
 (:after yasnippet
   (:map snippet-mode-map
     (:localleader
       :desc "tryout"     :nv               "t" #'yas-tryout-snippet
       :desc "load"       :nv               "l" #'yas-load-snippet-buffer
       :desc "load&close" :nv               "f" #'yas-load-snippet-buffer-and-close
       )
     )
   )

 ;; leader

 (:leader
   ;; eval expression        "`"
   ;; digits 1 - 9, 0
   ;; -
   ;; =
   (:desc "workspace" :prefix "TAB"
     :desc "Save session as"          :nv     "S" #'aj/save-session-as
     :desc "Save session"             :nv     "a" #'+workspace/save-session
     )
   (:desc "quit"     :prefix "q"
     :desc "Ask to save and quit"     :nv     "a" #'evil-quit-all
     )
   ;; (:desc "eeeeeeee" :prefix "e"
   ;;   )
   (:desc "remote"   :prefix "r"
     :desc "backup"                   :nv     "b" #'aj/my-backup
     )
   (:desc "toggle"   :prefix "t"
     :desc "light/dark theme"         :nv     "t" #'aj/toggle-doom-theme
     :desc "Themes"                   :nv     "T" #'counsel-load-theme
     :desc "Modeline"                 :nv     "m" #'hide-mode-line-mode
     :desc "Flyspell"                 :nv     "s" #'aj/enable-flyspell-check-if-prog
     :desc "Swap dictionaries"        :nv     "S" (λ! (aj/swap-two-ispell-dicts "english" "czech"))
     :desc "Highlight-blocks"         :nv     "B" #'highlight-blocks-mode
     )
   ;; (:desc "yyyyyyyy" :prefix "y"
   ;;   )
   ;; universal argument     "u"
   (:desc "insert"   :prefix "i"
     :desc "entity"                   :nv     "e" #'counsel-org-entity
     :desc "unicode"                  :nv     "u" #'counsel-unicode-char
     :desc "bash history"             :nv     "h" #'counsel-yank-bash-history
     )
   (:desc "open"     :prefix "o"
     :desc "link"                     :nv     "o" #'link-hint-open-link
     :desc "Agenda"                   :nv     "A" #'org-agenda
     :desc "App: Podcast"             :nv     "p" #'podcaster
     :desc "App: MPD"                 :nv     "m" (λ! (let ((hydra-lv nil)) (aj/mpd-control/body)))
     :desc "Clock"                    :nv     "c" #'aj/clock-menu
     :desc "Imenu-list"               :nv     "i" #'aj/open-imenu-sidebar
     :desc "Links"                    :nv     "l" #'aj/goto-bookmarks
     :desc "GTD"                      :nv     "g" #'aj/gtd-goto/body
     :desc "GTD"                      :nv     "g" (λ! (let ((hydra-lv nil)) (aj/gtd-goto/body)))
     :desc "Sidebar"                   :nv     "s" #'+treemacs/toggle
     (:desc "Wiki"      :prefix               "w"
       :desc "private"                  :nv   "p" #'aj/goto-private-wiki
       :desc "work"                     :nv   "w" #'aj/goto-work-wiki
       :desc "build"                    :nv   "b" #'aj/goto-build-wiki
       :desc "environment"              :nv   "e" #'aj/goto-environment-wiki
       :desc "eDucation"                :nv   "d" #'aj/goto-education-wiki
       ))
   (:desc "project"  :prefix "p"
     :desc "Agenda"                   :nv     "a" #'aj/project
     :desc "bootstrap"                :nv     "B" #'aj/project-bootstrap
     :desc "directories"              :nv     "d" #'counsel-projectile-find-dir
     :desc "files"                    :nv     "f" #'counsel-projectile-find-file
     :desc "buffer"                   :nv     "b" #'counsel-projectile-switch-to-buffer
     :desc "Capture"                  :nv     "x" #'aj/org-projectile-capture-for-current-project
     :desc "invalidate cache"         :nv     "i" #'projectile-invalidate-cache
     :desc "P README"                 :nv     "p" #'aj/better-open-current-projectile-org-file
     :desc "Switch"                   :nv     "s" #'counsel-projectile-switch-project
     :desc "Add"                      :nv     "A" #'aj/projectile-add-known-project-and-save
     :desc "Services"                 :nv     "t" #'prodigy
     :desc "set variable"             :nv     "v" #'projectile-edit-dir-locals
     :desc "Remove"                   :nv     "R" #'projectile-remove-known-project
     )
   ;; previous               "["
   ;; next                   "]"
   :desc "clock"     :nv "\\" (λ! (let ((hydra-lv nil)) (aj/clocking/body)))
   :desc "agenda"    :nv "a" (λ! (let ((hydra-lv nil)) (aj/agenda/body)))
   (:desc "snippet"  :prefix "s"
     :desc "Preview"                  :nv     "p" #'ivy-yasnippet
     )
   (:desc "dict"     :prefix "d"
     :desc "word/stardict"            :nv     "s" #'sdcv-search-pointer
     :desc "input/stardict"           :nv     "i" #'sdcv-search-input
     :desc "online"                   :nv     "o" #'define-word
     :desc "online/point"             :nv     "p" #'define-word-at-point
     :desc "webster"                  :nv     "w" #'browse-webster-at-point
     :desc "synosaurus"               :nv     "j" #'synosaurus-lookup
     :desc "synosaurus"               :nv     "r" #'synosaurus-choose-and-replace
     :desc "wordnut"                  :nv     "k" #'wordnut-lookup-current-word
     :desc "google at point"          :nv     "g" #'google-translate-at-point
     :desc "google at point reverse:" :nv     "G" #'google-translate-at-point-reverse
     :desc "dictionary"               :nv     "d" #'browse-dictionary-at-point
     )
   (:desc "file"     :prefix "f"
     :desc "ag-cwd"                   :nv     "g" #'+ivy/ag-from-cwd
     :desc "ag-project"               :nv     "G" #'+ivy/ag
     :desc "rg-cwd"                   :nv     "h" #'+ivy/rg-from-cwd
     :desc "rg-project"               :nv     "H" #'+ivy/rg
     :desc "file"                     :nv     "f" #'counsel-find-file
     :desc "jump org"                 :nv     "o" #'aj/jump-to-org-dir
     :desc "jump file"                :nv     "j" #'counsel-file-jump
     :desc "jump dir"                 :nv     "k" #'counsel-dired-jump
     )
   (:desc "git"      :prefix "g"
     :desc "/log"                     :nv     "/" #'counsel-git-log
     )
   (:desc "help"     :prefix "h"
     :desc "helpful-symbol"           :nv     "a" #'helpful-symbol
     :desc "update-diff"              :nv     "u" #'obsoke/ediff-dotfile-and-template
     :desc "Info"                     :nv     "i" #'info
     :desc "Info on symbol"           :nv     "I" #'counsel-info-lookup-symbol
     :desc "Manual"                   :nv     "m" #'man
     :desc "Echo"                     :nv     "e" #'view-echo-area-messages
     :desc "Pop on error"             :nv     "P" #'toggle-debug-on-error
     )
   (:desc "jump:"    :prefix "j"
     :desc "file"                     :nv     "f" #'counsel-file-jump
     :desc "session"                  :nv     "S" #'+workspace/load-session
     :desc "workspace"                :nv     "i" #'+workspace/switch-to
     :desc "window"                   :nv     "o" #'ace-select-window
     :desc "shell-buffer"             :nv     "s" #'counsel-switch-to-shell-buffer
     :desc "word"                     :nv     "w" #'evil-avy-goto-word-1
     :desc "line"                     :nv     "l" #'evil-avy-goto-line
     :desc "directory"                :nv     "d" #'counsel-dired-jump
     :desc "view"                     :nv     "v" #'ivy-switch-view
     :desc "clock"                    :nv     "c" #'org-clock-jump-to-current-clock
     :desc "buffer"                   :nv     "b" #'counsel-ibuffer
     :desc "project bookmark"         :nv     "p" #'counsel-projectile-bookmark
     )
   :desc "capture"   :nv "k" (λ! (let ((hydra-lv nil)) (aj/capture/body)))
   (:desc "link"     :prefix "l"
     :desc "Org-store-link"           :nv     "s" #'org-store-link
     :desc "Org-copy-link"            :nv     "c" #'my-org-retrieve-url-from-point
     :desc "Open"                     :nv     "f" #'link-hint-open-link
     :desc "Open all links"           :nv     "a" #'link-hint-open-all-links
     :desc "Copy"                     :nv     "c" #'link-hint-copy-link
     :desc "Copy all links"           :nv     "C" #'link-hint-copy-all-links
     )
   ;; evil-ex                ";"
   :desc "popup"     :nv "'" #'+popup/toggle
   ;; (:desc "zzzzzzzz" :prefix "z" )
   ;; scratch-buffer         "x"
   (:desc "code"     :prefix "c"
     :desc "eval-last-sexp"           :nv     "s" #'eval-last-sexp
     :desc "macro-expand"             :nv     "m" #'macrostep-expand
     :desc "imenu-outline"            :nv     "o" #'counsel-imenu
     ;;          :desc "Help in Dashdocs"      :nv     "h" #'counsel-dash
     :desc "Help in Dashdocs"         :nv     "h" (lambda! (progn (require 'helm-dash) (counsel-dash)))
     :desc "Info about error"         :nv     "i" #'flycheck-explain-error-at-point
     )
   (:desc "view:"    :prefix "v"
     :desc "brain-visualize"          :nv     "v" #'org-brain-visualize
     :desc "jump"                     :nv     "j" #'ivy-switch-view
     :desc "save"                     :nv     "s" #'ivy-push-view
     :desc "pop"                      :nv     "p" #'ivy-pop-view
     )
   (:desc "buffer"   :prefix "b"
     :desc "List"                     :nv     "l" #'ibuffer-list-buffers
     :desc "Kill buffers"             :nv     "K" #'kill-buffer
     )
   (:desc "notes"    :prefix "n"
     :desc "notes"     :nv "n" (λ! (counsel-find-file org-brain-path))
     :desc "private"   :nv "p" (λ! (counsel-find-file (concat org-brain-path "/private_brain/")))
     :desc "grep"      :nv "g" (λ! (+ivy/rg nil nil org-brain-path))
     :desc "visualize" :nv "v" #'aj/visualize-brain-and-take-care-of-buffers
     )
   ;; (:desc "mmmmmmmm" :prefix "m" )
   ;; switch buffer          ","
     :desc "Switch buffer"            :nv     "," #'persp-switch-to-buffer
   ;; find file              ","
   (:desc "search"   :prefix "/"
     :desc "Swiper"                   :nv     "/" #'aj/my-swiper
     )
   )
 )
