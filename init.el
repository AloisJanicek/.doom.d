;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(doom! :feature
       ;; debugger          ; stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       (lookup           ; helps you navigate your code and documentation
        +devdocs         ; ...on devdocs.io online
        +docsets)        ; ...or in Dash docsets locally
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       (syntax-checker   ; tasing you for every semicolon you forget
        +childframe)     ; use childframes for error popups (Emacs 26+ only)
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       (company          ; the ultimate code completion backend
        +auto            ; as-you-type code completion
        +childframe)
       ;; (helm             ; the *other* search engine for love and life
       ;;  +fuzzy)          ; enable fuzzy search backend for helm
       ;; ido               ; the other *other* search engine...
       (ivy              ; a search engine for love and life
        ;; +fuzzy           ; enable fuzzy search backend for ivy
        +childframe)

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       evil-goggles      ; display visual hints when editing in evil
       ;; fci               ; a `fill-column' indicator
       hl-todo           ; highlight todo/fixme/note tags
       nav-flash         ; blink the current line after jumping
       ;; neotree           ; a project drawer, like NERDTree for vim
       treemacs          ; a project drawer, like neotree but cooler
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;; pretty-code       ; replace bits of code with pretty symbols
       ;; tabbar            ;  an (incomplete) tab bar for Emacs
       ;; unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows

       :editor
       ;; parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates

       :emacs
       dired             ; making dired pretty [functional]
       ediff             ; comparing files in Emacs
       electric   ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       imenu             ; an imenu sidebar and searchable code index
       term              ; terminals in Emacs
       vc                ; version-control and Emacs, sitting in a tree

       :tools
       editorconfig      ; let someone else argue about tabs vs spaces
       ;; ein               ; tame Jupyter notebooks with emacs
       gist              ; interacting with github gists
       ;; macos             ; MacOS-specific commands
       ;; make              ; run make tasks from Emacs
       magit             ;
       ;; password-store    ; password manager for nerds
       pdf               ; pdf enhancements
       prodigy           ;  managing external services & code builders
       rgb               ; creating color strings
       ;; tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp
       ;; wakatime

       :lang
       racket
       ;; assembly          ; assembly for fun or debugging
       ;; (cc +irony +rtags); C/C++/Obj-C madness
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;; crystal           ; ruby at the speed of c
       ;; clojure           ; java with a lisp
       ;; csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;; erlang            ; an elegant language for a more civilized age
       ;; elixir            ; erlang done right
       ;; elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;; ess               ; emacs speaks statistics
       ;; go                ; the hipster dialect
       ;; (haskell +intero) ; a language that's lazier than I am
       ;; hy                ; readability of scheme w/ speed of python
       ;; (java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;; julia             ; a better, faster MATLAB
       ;; latex             ; writing papers in Emacs has never been so fun
       ;; ledger            ; an accounting system in Emacs
       ;; lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;; nim               ; python + lisp at the speed of c
       ;; nix               ; I hereby declare "nix geht mehr!"
       ;; ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present)        ; Emacs for presentations
       ;; perl              ; write code no one else can comprehend
       ;; php               ; perl's insecure younger brother
       ;; plantuml          ; diagrams for confusing people more
       ;; purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       ;; qt                ; the 'cutest' gui framework ever
       rest              ; Emacs as a REST client
       ;; ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;; rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;; scala             ; java, but good
       sh                ; she sells (ba|z)sh shells on the C xor
       ;; solidity          ; do you need a blockchain? No.
       ;; swift             ; who asked for emoji variables?
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;; (email +gmail)    ; emacs as an email client
       ;; irc               ; how neckbeards socialize
       ;; (rss +org)        ; emacs as an RSS reader
       ;; twitter           ; twitter client https://twitter.com/vnought
       (write            ; emacs as a word processor (latex + org + markdown)
        +wordnut         ; wordnet (wn) search
        +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
       ;; floobits          ; peer programming for a price
       ;; impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;; literate

       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       (default +bindings +snippets +evil-commands))

(defvar +GTD "~/org/GTD.org"
  "File where things must be done.")

(defvar +SOMEDAY "~/org/SOMEDAY.org"
  "File where things must be done.")

(defvar +MAYBE "~/org/MAYBE.org"
  "File where things must be done.")

(defvar org-files nil
  "Lists of org files I always want to have opened for quick access.")

(defvar +org-projectile-per-project-filepath "README.org"
  "Org file in every project which can be used to contribute into agenda")

(defvar +JOURNAL "~/org/JOURNAL.org"
  "File where things are logged.")

(defvar +persp-blacklist nil
  "Contains list files which should not be considered as part of workspace")

(defvar +aj/time-block nil
  "Is a list of sequences where first item is string representing time in
\"%H:%S\" format, second item is list of integers indicating time which should
have a grid line in agenda and it is being passed to `org-agenda-grid' and
third item is string representing tag with leading plus sign \"+\" to which
should be agenda-view filtered by `org-agenda-tag-filter-preset'.")

(defvar +refile-targets-with-headlines t
  "List of org files which should be allowed offer refile under headlines")

(setq org-files (directory-files-recursively "~/org/brain" ".org"))
(add-to-list 'org-files (expand-file-name +GTD))
(add-to-list 'org-files (expand-file-name +SOMEDAY))
(add-to-list 'org-files (expand-file-name +MAYBE))

(setq user-mail-address "janicek.dev@gmail.com"
      user-full-name    "Alois Janíček"
      +refile-targets-with-headlines '("GTD.org" "About.org" "SOMEDAY.org" "LISTS.org")
      +aj/time-blocks
      '(["06:30" (0630 0700 0730 0800) "MORNING"]
        ["08:00" (0800 0830 0900 0930 1000 1030 1100 1130 1200) "WORK"]
        ["12:00" (1200 1230) "LUNCH"]
        ["12:30" (1230 1300 1330 1400 1430 1500 1530 1600 1630) "WORK"]
        ["16:30" (1630 1700 1730 1800 1830 1900 1930) "OUTSIDE"]
        ["19:30" (1930 2000 2030 2100) "EVENING"]
        ["21:00" (2100 2130 2200) "SLEEP"]
        ["22:00" (2200 0600) nil])
      org-directory "~/org/"
      org-brain-path "~/org/brain/"
      +org-attach-dir "attach/"
      +org-export-dir "export"
      +file-templates-dir "~/org/templates"
      doom-font                   (font-spec :family "Iosevka" :size 16)
      doom-big-font               (font-spec :family "Iosevka" :size 24)
      doom-variable-pitch-font    (font-spec :family "Roboto" :size 16)
      doom-unicode-font           (font-spec :family "Iosevka" :size 16)
      +doom-modeline-bar-width    4
      doom-neotree-project-size 1
      doom-neotree-line-spacing 0
      doom-theme 'doom-one
      doom-neotree-folder-size 1.0
      doom-neotree-chevron-size 0.6
      all-the-icons-scale-factor 1
      +doom-quit-messages '("")
      doom-scratch-buffer-major-mode t
      )

(setq-default tab-width 2)

(def-package-hook! langtool
  :pre-config
  (setq langtool-language-tool-jar t)
  (setq langtool-java-classpath
        "/usr/share/languagetool:/usr/share/java/languagetool/*")
  nil)

(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

(provide 'init.el)
