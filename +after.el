;;; ~/.doom.d/+after.el -*- lexical-binding: t; -*-
(after! files
  (add-hook 'after-save-hook #'prettier-stylelint-fix-file-and-revert)
  (add-hook 'after-save-hook #'beautify-html-file-and-revert))

(after! prog-mode
  (add-hook! 'prog-mode-hook 'goto-address-mode))

(after! css-mode
  (add-hook 'css-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "CSS"))))
  (add-hook 'scss-mode-hook (lambda () (setq-local counsel-dash-docsets '("Sass" "HTML" "CSS")))))

(after! epa
  (setq epa-pinentry-mode 'ask))

(after! flyspell
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC")))

(after! man
  (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "#ff7a79")
  (set-face-attribute 'Man-underline nil :inherit 'underline :foreground "#98be65")
  (set-popup-rule! "*Man\*"                         :size 0.4 :side 'left :select t)
  (set-popup-rule! "*man\*"                         :size 0.6 :side 'left :select t))

(after! eww
  (set-popup-rule! "*eww\*"                         :size 0.4 :side 'left :select t))

(after! ibuffer
  (set-popup-rule! "*Ibuffer\*"                     :size 0.4 :side 'left :select t))

(after! info
  (advice-add 'info :before 'aj/set-info-popup-width))

(after! cus-edit
  (set-popup-rule! "*Customize\*"                   :size 0.4 :side 'left :select t :transient nil))

(after! helpful
  (set-popup-rule! "*helpful\*"                     :size 0.4 :side 'left :select t))

(after! help
  (set-popup-rule! "*help\*"                        :size 0.4 :side 'left :select t))

(after! apropos
  (set-popup-rule! "*apropos\*"                     :size 0.4 :side 'left :select t)
  (set-popup-rule! "*Apropos\*"                     :size 0.4 :side 'left :select t))

(after! profiler
  (set-popup-rule! "^.*-Profiler-Report.*$"         :size 0.4 :side 'right :select t))

(after! evil
  (setq evil-move-cursor-back nil))

(after! prodigy
  (prodigy-define-service
    :name "Gulp"
    :command "gulp"
    :cwd (projectile-project-root)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(after! yasnippet
  (push "~/org/snippets" yas-snippet-dirs))

(after! projectile
  (setq projectile-globally-ignored-file-suffixes (append (list ".elc"))
        projectile-globally-ignored-directories (append (list "node_modules"))
        projectile-track-known-projects-automatically nil
        counsel-projectile-sort-projects t
        projectile-ignored-projects nil ))

(after! persp-mode
  ;; (setq persp-kill-foreign-buffer-action nil)
  ;; collect names of all brain files
  (setq +persp-blacklist (append
                          `,(directory-files (concat org-brain-path "private_brain"))
                          `,(directory-files org-brain-path)))
  ;; TODO
  ;; collect names of all interactively used brain files
  ;; this should be some data structure which would hold list of whitelisted files per perspective
  (setq +persp-whitelist nil)
  (setq persp-emacsclient-init-frame-behaviour-override 'persp-ignore-wconf)
  )

(after! term
  (add-hook! 'term-mode-hook #'hide-mode-line-mode)
  ;; remap keys for terminal with Evil
  (add-hook! :append term-mode 'aj/set-term-keys)
  (add-hook 'term-mode-hook '(lambda () (interactive)(setq left-fringe-width 0
                                                           right-ringe-width 0))))

(after! magit
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

                                        ; note: broken with default flycheck, needs :branch "fix-1398-quoted-lambdas"
                                        ; see: https://github.com/flycheck/flycheck/pull/1440
                                        ; see: https://github.com/flycheck/flycheck/issues/1398
(after! flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (setq flycheck-stylelintrc "~/.stylelintrc.json"
        flycheck-tidyrc "~/.tidyrc")
  (setq-default flycheck-disabled-checkers '(css-csslint scss sass/scss-sass-lint))
  (flycheck-define-checker javascript-eslint-custom
    "A Javascript syntax and style checker using eslint.
See URL `http://eslint.org/'."
    :command ("eslint" "--format=json"
              (option-list "--rulesdir" flycheck-eslint-rules-directories)
              "--stdin" "--stdin-filename" source-original)
    :standard-input t
    :error-parser flycheck-parse-eslint
    :enabled (lambda () (flycheck-eslint-config-exists-p))
    :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode)
    :working-directory flycheck-eslint--find-working-directory
    :error-explainer
    (lambda (error)
      (let ((error-code (flycheck-error-id error)))
        (progn
          (browse-url (concat "https://eslint.org/docs/rules/" error-code)))))
    :verify
    (lambda (_)
      (let* ((default-directory
               (flycheck-compute-working-directory 'javascript-eslint))
             (have-config (flycheck-eslint-config-exists-p)))
        (list
         (flycheck-verification-result-new
          :label "config file"
          :message (if have-config "found" "missing or incorrect")
          :face (if have-config 'success '(bold error)))))))
  (add-to-list 'flycheck-checkers 'javascript-eslint-custom)
  ;; css-styleling checke with explainer
  (flycheck-define-checker css-stylelint-custom
    "A CSS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
    :command ("stylelint"
              (eval flycheck-stylelint-args)
              (option-flag "--quiet" flycheck-stylelint-quiet)
              (config-file "--config" flycheck-stylelintrc))
    :standard-input t
    :error-parser flycheck-parse-stylelint
    :error-explainer
    (lambda (error)
      (let ((error-code (flycheck-error-id error)))
        (progn
          (browse-url (concat "https://stylelint.io/user-guide/rules/" error-code)))))
    :modes (css-mode))
  (add-to-list 'flycheck-checkers 'css-stylelint-custom))

(after! company
  (setq company-idle-delay 0.6)
  (setq company-minimum-prefix-length 2))

(after! json-mode
  (setq js2-basic-offset 2))

(after! js2-mode
  (add-hook 'js2-mode-hook (lambda () (setq-local counsel-dash-docsets '("JavaScript" "HTML" "CSS"))))
  (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default js2-basic-offset 2))

(after! emmet-mode
;;; run remaping function before entering emmet-preview
  (advice-add 'emmet-preview :before 'aj/remap-emmet)
  (defadvice emmet-preview-accept (after emmet-after activate) (aj/indent-if-not-webmode)))

(after! python
  (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python_3")))))

(after! tide
  (setq tide-completion-detailed nil
        tide-always-show-documentation nil))

(after! ivy
  (setq ivy-height 40)
  (map-put ivy-display-functions-alist 't 'ivy-posframe-display-at-frame-center)
  (ivy-set-actions
   'counsel-projectile-bookmark
   '(("d" bookmark-delete "delete")
     ("e" bookmark-rename "edit")))
  (ivy-add-actions
   #'ivy-yasnippet
   '(("e" ivy-yasnippet--copy-edit-snippet-action "Edit snippet as your own"))))

(after! ivy-rich
  (advice-add #'+ivy-recentf-transformer :override #'+ivy-recentf-combined-transformer))

(after! counsel
  (setq counsel-grep-base-command "grep -E -n -i -e %s %s")
  (setq counsel-org-goto-face-style 'org
        counsel-org-headline-display-style 'path
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t)
  (set-popup-rule! "^\\*ivy-occur" :size 0.70 :ttl 0 :quit nil)
  (advice-add #'+ivy-buffer-transformer :override #'+ivy-combined-buffer-transformer))

(after! counsel-projectile
  (advice-add  #'+ivy-projectile-find-file-transformer :override #'+ivy-projectile-find-file-combined-transformer))

(after! org-bullets
  (setq org-bullets-bullet-list
        '("◉")))

(after! web-mode
  (add-hook 'web-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "CSS" "Bootstrap 4"))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  (add-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (add-hook 'web-mode-hook 'flycheck-mode)
  (setq web-mode-enable-current-element-highlight t)
  (set-face-attribute 'web-mode-current-element-highlight-face nil :background "#21242b" :foreground "#51afef")
  (set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "#5B6268")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#5B6268")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#E06C75")
  (set-face-attribute 'web-mode-html-tag-unclosed-face nil :inherit 'web-mode-html-tag-face :underline '(:color "#ff6c6b" :style wave)))

(after! google-translate-default-ui
  (setq google-translate-default-source-language "cs"
        google-translate-default-target-language "en")
  (set-popup-rule! "*Google Translate\*"            :size 0.4 :side 'top :select t))

(after! synosaurus
  (set-popup-rule! "*Synonyms List\*"               :size 0.4 :side 'top :select t))

(after! wordnut
  (set-popup-rule! "*WordNut\*"                     :size 0.4 :side 'top :select t))

(after! avy
  (setq avy-all-windows t
        avy-background t))

(after! treemacs
  (setq evil-treemacs-state-cursor 'box)
  (setq treemacs-project-follow-cleanup t)

  ;; seems like all the icons doesn't work without png icons being enabled
  (setq treemacs-no-png-images nil)
  ;; Looks actually quite good now with this size setting
  (treemacs-resize-icons 18)
  ;; Override (some?) icons with all-the-icons
  (dolist (item all-the-icons-icon-alist)
    (let* ((extension (car item))
           (icon (apply (cdr item))))
      (ht-set! treemacs-icons-hash
               (s-replace-all '(("\\" . "") ("$" . "") ("." . "")) extension)
               (concat icon " "))))

  (set-face-attribute     'treemacs-root-face nil :height 1.0)
  (add-hook 'treemacs-mode-hook 'variable-pitch-mode))

(after! imenu-list
  ;; First create new face which is a copy of hl-line-face
  (copy-face 'hl-line 'hl-line-imenu-list-face)
  ;; Change what you want in this new face
  (set-face-attribute 'hl-line-imenu-list-face nil
                      :background `,(doom-color 'base4) :weight 'bold :underline t)
  ;; Finally, the hook
  (add-hook 'imenu-list-major-mode-hook 'my-imenu-list-hl-line)
  (add-hook 'imenu-list-major-mode-hook 'variable-pitch-mode))

(after! loaddefs
  (setq browse-url-browser-function
        '(
          ("wikipedia\\.org" . browse-url-firefox)
          ("github" . browse-url-chromium)
          ("reddit" . browse-url-chromium)
          ("gitlab" . browse-url-chromium)
          ("youtube" . browse-url-chromium)
          ("eslint.org" . browse-url-chromium)
          ("stylelint.io" . browse-url-chromium)
          ("thefreedictionary\\.com" . eww-browse-url)
          ("dictionary\\.com" . eww-browse-url)
          ("merriam-webster\\.com" . eww-browse-url)
          ("." . gk-browse-url)
          ))
  )

(after! which-key
  (setq which-key-idle-delay 0.8
        which-key-allow-regexps nil
        which-key-allow-evil-operators 1))
