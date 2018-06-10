;;; init.el -*- lexical-binding: t; -*-
;;
;; doom! call
(doom! :feature
      ;debugger          ; FIXME stepping through code, to help you add bugs
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
       version-control   ; remember, remember that commit in November
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       (company          ; the ultimate code completion backend
        +auto            ; as-you-type code completion
        +childframe)
      ;(helm             ; the *other* search engine for love and life
      ; +fuzzy)          ; enable fuzzy search backend for helm
      ;ido               ; the other *other* search engine...
       (ivy              ; a search engine for love and life
        ;+fuzzy           ; enable fuzzy search backend for ivy
        +childframe)

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       evil-goggles      ; display visual hints when editing in evil
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
      ;tabbar            ; FIXME an (incomplete) tab bar for Emacs
      ;unicode           ; extended unicode support for various languages
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows

       :emacs
       dired             ; making dired pretty [functional]
       ediff             ; comparing files in Emacs
       electric-indent   ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       imenu             ; an imenu sidebar and searchable code index
       term              ; terminals in Emacs

       :tools
       editorconfig      ; let someone else argue about tabs vs spaces
      ;ein               ; tame Jupyter notebooks with emacs
       gist              ; interacting with github gists
      ;macos             ; MacOS-specific commands
      ;make              ; run make tasks from Emacs
       magit             ;
      ;password-store    ; password manager for nerds
       pdf               ; pdf enhancements
       prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       rotate-text       ; cycle region at point between text candidates
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

       :lang
      ;assembly          ; assembly for fun or debugging
      ;cc                ; C/C++/Obj-C madness
      ;crystal           ; ruby at the speed of c
      ;clojure           ; java with a lisp
      ;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
      ;erlang            ; an elegant language for a more civilized age
      ;elixir            ; erlang done right
      ;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
      ;ess               ; emacs speaks statistics
      ;go                ; the hipster dialect
      ;(haskell +intero) ; a language that's lazier than I am
      ;hy                ; readability of scheme w/ speed of python
      ;(java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
      ;julia             ; a better, faster MATLAB
      ;latex             ; writing papers in Emacs has never been so fun
      ;ledger            ; an accounting system in Emacs
      ;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
      ;nim               ; python + lisp at the speed of c
      ;nix               ; I hereby declare "nix geht mehr!"
      ;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present)        ; Emacs for presentations
      ;perl              ; write code no one else can comprehend
      ;php               ; perl's insecure younger brother
      ;plantuml          ; diagrams for confusing people more
      ;purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
      ;qt                ; the 'cutest' gui framework ever
       rest              ; Emacs as a REST client
      ;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
      ;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
      ;scala             ; java, but good
       sh                ; she sells (ba|z)sh shells on the C xor
      ;solidity          ; do you need a blockchain? No.
      ;swift             ; who asked for emoji variables?
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
      ;(email +gmail)    ; emacs as an email client
      ;irc               ; how neckbeards socialize
      ;(rss +org)        ; emacs as an RSS reader
      ;twitter           ; twitter client https://twitter.com/vnought
      (write            ; emacs as a word processor (latex + org + markdown)
       +wordnut         ; wordnet (wn) search
       +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
      ;floobits          ; peer programming for a price
      ;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
      ;literate

       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       (default +bindings +snippets +evil-commands))
;;
;; Global variables
;;

;; Wiki files locations
(defvar +private-wiki "~/org/wiki/private-wiki.org"
  "File where my private wiki is.")
(defvar +work-wiki "~/org/wiki/work-wiki.org"
  "File where my work wiki is.")
(defvar +environment-wiki "~/org/wiki/environment-wiki.org"
  "File where my environment wiki is.")
(defvar +education-wiki "~/org/wiki/education-wiki.org"
  "File where my education wiki is.")
(defvar +build-wiki "~/org/wiki/build-wiki.org"
  "File where my build wiki is.")

(setq browse-url-browser-function
      '(
        ("wikipedia\\.org" . browse-url-firefox)
        ("github" . browse-url-chromium)
        ("thefreedictionary\\.com" . eww-browse-url)
        ("dictionary\\.com" . eww-browse-url)
        ("merriam-webster\\.com" . eww-browse-url)
        ("." . gk-browse-url)
        ))

;; Set user credentials
(setq user-mail-address "janicek.dev@gmail.com"
      user-full-name    "Alois Janíček")
(setq auth-sources '("~/.authinfo.gpg"))
;; Customize fonts
(setq doom-font                   (font-spec :family "Iosevka" :size 16)
      doom-big-font               (font-spec :family "Iosevka" :size 24)
      doom-variable-pitch-font    (font-spec :family "Roboto" :size 16)
      doom-unicode-font           (font-spec :family "Iosevka" :size 16)
      )

(set-face-attribute 'fixed-pitch-serif nil :family "Iosevka Slab")
(setq-default tab-width 2)
;; Customize UI
(setq +doom-modeline-bar-width    4
      doom-neotree-project-size 1
      doom-neotree-line-spacing 0
      doom-theme 'doom-one
      doom-neotree-folder-size 1.0
      doom-neotree-chevron-size 0.6
      all-the-icons-scale-factor 1
      )

;; Customize files and directory variables
(setq +org-dir "~/org/"
      +org-attach-dir "./attach/"
      +file-templates-dir "~/org/templates"
      )

;; Customize which-key
(setq which-key-idle-delay 0.8
      which-key-allow-regexps nil
      which-key-allow-evil-operators 1
      )

;; start me maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; disable distracting messages when exiting doom
(setq +doom-quit-messages '(""))
;; scratch: inherit major mode from latest active buffer
(setq doom-scratch-buffer-major-mode t)
