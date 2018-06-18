;;;  -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+hydras")
(load! "+org")
(load! "+after")
(load! "+def")

;; hooks
(add-hook 'web-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "CSS" "Bootstrap 4"))))
(add-hook 'css-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "CSS"))))
(add-hook 'scss-mode-hook (lambda () (setq-local counsel-dash-docsets '("Sass" "HTML" "CSS"))))
(add-hook 'js2-mode-hook (lambda () (setq-local counsel-dash-docsets '("JavaScript" "HTML" "CSS"))))
(add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python_3"))))
(add-hook 'js2-mode-hook 'eslintd-fix-mode)
(add-hook 'org-agenda-mode-hook #'hide-mode-line-mode )
(add-hook 'after-save-hook #'prettier-stylelint-fix-file-and-revert)
(add-hook 'after-save-hook #'beautify-html-file-and-revert)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook 'er/add-web-mode-expansions)
(add-hook! 'web-mode-hook #'flycheck-mode)
(add-hook 'term-mode-hook '(lambda () (interactive)(setq left-fringe-width 0
                                                         right-ringe-width 0)))
(add-hook! 'term-mode-hook #'hide-mode-line-mode)
(add-hook 'ereader-mode-hook 'hide-mode-line-mode)
;; remap keys for terminal with Evil
(add-hook! :append term-mode 'aj/set-term-keys)

;;; run remaping function before entering emmet-preview
(advice-add 'emmet-preview :before 'aj/remap-emmet)
;; advices
(advice-add '+popup--delete-window :before #'(lambda (&rest _) (when (eq major-mode 'org-mode) (save-buffer))))
(defadvice emmet-preview-accept (after emmet-after activate) (aj/indent-if-not-webmode))

;; misc
(set-popup-rule! "*backtrace\*"'((size . 0.4) (side . right)) '((select . t)))
