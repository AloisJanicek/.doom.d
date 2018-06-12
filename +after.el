;;; ~/.doom.d/+after.el -*- lexical-binding: t; -*-

(after! epa
  (setq epa-pinentry-mode 'ask))
(after! flyspell
  (setq flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil
        )
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))
  )
(after! man
  (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "#ff7a79")
  (set-face-attribute 'Man-underline nil :inherit 'underline :foreground "#98be65")
  (set! :popup "*Man\*" '((size . 0.4) (side . left)) '((select . t) ))
  (set! :popup "*man\*" '((size . 0.6) (side . left)) '((select . t) )))
(after! eww
  (set! :popup "*eww\*" '((size . 0.4) (side . left)) '((select . t) )))
(after! ibuffer
  (set! :popup "*Ibuffer\*" '((size . 0.4) (side . left)) '((select . t) )))
(after! info
  (advice-add 'info :before 'aj/set-info-popup-width))
(after! cus-edit
  (set! :popup "*Customize\*" '((size . 0.4) (side . left)) '((select . t) (transient . nil))))
(after! helpful
  (set! :popup "*helpful\*" '((size . 0.4) (side . left)) '((select . t) )))
(after! help
  (set! :popup "*help\*" '((size . 0.4) (side . left)) '((select . t) )))
(after! apropos
  (set! :popup "*apropos\*" '((size . 0.4) (side . left)) '((select . t) ))
  (set! :popup "*Apropos\*" '((size . 0.4) (side . left)) '((select . t) )))
(after! profiler
  (set! :popup "^.*-Profiler-Report.*$"'((size . 0.4) (side . right)) '((select . t))))
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
        projectile-ignored-projects '(
                                      )))
(after! persp-mode
  (setq persp-kill-foreign-buffer-action nil))
(after! magit
  ;; (set! :popup "^.*magit" '((slot . -1) (side . right) (size . 80)) '((modeline . nil) (select . t)))
  ;; (set! :popup "^.*magit.*popup\\*" '((slot . 0) (side . right)) '((modeline . nil) (select . t)))
  ;; (set! :popup "^.*magit-revision:.*" '((slot . 2) (side . right) (window-height . 0.6)) '((modeline . nil) (select . t)))
  ;; (set! :popup "^.*magit-diff:.*" '((slot . 2) (side . right) (window-height . 0.6)) '((modeline . nil) (select . nil)))
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  )
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
  (add-to-list 'flycheck-checkers 'css-stylelint-custom)
  )
(after! company
  (setq company-idle-delay 0.6)
  (setq company-minimum-prefix-length 2))
(after! json-mode
  (setq js2-basic-offset 2))
(after! js2-mode
  (setq-default indent-tabs-mode nil)
  (setq-default js2-basic-offset 2))
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
  )
(after! org-bullets
  (setq org-bullets-bullet-list
        '("◉")))
(after! counsel
  (setq counsel-grep-base-command "grep -E -n -i -e %s %s")
  (setq counsel-org-goto-face-style 'org
        counsel-org-headline-display-style 'path
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t))
(after! web-mode
  (setq web-mode-enable-current-element-highlight t)
  (set-face-attribute 'web-mode-current-element-highlight-face nil :background "#21242b" :foreground "#51afef")
  (set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "#5B6268")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#5B6268")
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#E06C75")
  (set-face-attribute 'web-mode-html-tag-unclosed-face nil :inherit 'web-mode-html-tag-face :underline '(:color "#ff6c6b" :style wave))
  )
(after! google-translate-default-ui
  (setq google-translate-default-source-language "cs"
        google-translate-default-target-language "en")
  (set! :popup "*Google Translate\*" '((size . 0.4) (side . top)) '((select . t) ))
  )

(after! synosaurus
  (set! :popup "*Synonyms List\*" '((size . 0.4) (side . top)) '((select . t) ))
  )
(after! wordnut
  (set! :popup "*WordNut\*" '((size . 0.4) (side . top)) '((select . t) ))
  )
