;;; ~/.doom.d/autoload/helpers.el -*- lexical-binding: t; -*-
;;;###autoload
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))
;;;###autoload
(defun my-yank-org-link (text)
  (string-match org-bracket-link-regexp text)
  (insert (substring text (match-beginning 1) (match-end 1))))
;;;###autoload
(defun counsel-org-goto-open-org-link (x)
  "Open selected link"
  (org-goto-marker-or-bmk (cdr x))
  (org-open-at-point)
  (bury-buffer)
  ;; (kill-buffer)
  )
;;;###autoload
(defun counsel-org-goto-wiki-action (x)
  "Go to headline in candidate X."
  (org-goto-marker-or-bmk (cdr x))
  (outline-show-branches)
  ;; (forward-line 1)
  ;; (org-cycle)
  ;; (forward-line -1)
  (org-narrow-to-subtree))
;;;###autoload
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
;;;###autoload
(defun aj/org-refile-to-file-as-top-level (filename)
  "Move current headline as a top level headline at top of specified file
https://www.reddit.com/r/emacs/comments/74i8sy/how_to_copy_an_org_item_to_a_specific_headerfile/
"
  (let ((pos (save-excursion
               (find-file filename)
               (goto-char (point-min))
               (forward-line))))
    (org-refile nil nil (list nil filename nil pos)))
  (switch-to-buffer (current-buffer)))
;;;###autoload
(defun my/refile (file headline &optional arg)
  "Refile to a specific location.
With a 'C-u' ARG argument, we jump to that location (see
`org-refile').
Use `org-agenda-refile' in `org-agenda' mode."
  (let* ((pos (with-current-buffer (or (get-buffer file)	;Is the file open in a buffer already?
                                       (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
                (or (org-find-exact-headline-in-buffer headline)
                    (error "Can't find headline `%s'" headline))))
         (filepath (buffer-file-name (marker-buffer pos)));If we're given a relative name, find absolute path
         (rfloc (list headline filepath nil pos)))
    (if (and (eq major-mode 'org-agenda-mode) (not (and arg (listp arg)))) ;Don't use org-agenda-refile if we're just jumping
        (org-agenda-refile nil rfloc)
      (org-refile arg nil rfloc))))
;;;###autoload
(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))
;;;###autoload
(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))
;;;###autoload
(defun opened-org-agenda-files ()
  ;; (let ((files (org-agenda-files)))
  (let ((files (org-projectile-todo-files)))
    (setq opened-org-agenda-files nil)
    (mapcar
     (lambda (x)
       (when (get-file-buffer x)
         (push x opened-org-agenda-files)))
     files)))
;;;###autoload
(defun kill-org-agenda-files ()
  ;; (let ((files (org-agenda-files)))
  (let ((files (org-projectile-todo-files)))
    (mapcar
     (lambda (x)
       (when
           (and
            (get-file-buffer x)
            (not (member x opened-org-agenda-files)))
         (kill-buffer (get-file-buffer x))))
     files)))
;;;###autoload
(defun aj/return-short-project-name ()
  "Returns short project name - based on projectile"
  (format "Project: %s"
          (replace-regexp-in-string "/proj/\\(.*?\\)/.*"
                                    "\\1"
                                    (projectile-project-name))))
;;;###autoload
(defun message-off-advice (oldfun &rest args)
  "Quiet down messages in adviced OLDFUN."
  (let ((message-off (make-symbol "message-off")))
    (unwind-protect
        (progn
          (advice-add #'message :around #'ignore (list 'name message-off))
          (apply oldfun args))
      (advice-remove #'message message-off))))
;;;###autoload
(defun aj/remap-keys-for-org-agenda ()
  "Remap keys for org-agenda, call it before opening org agenda"
  (evil-define-key 'motion org-agenda-mode-map
    "j" 'org-agenda-next-item
    "k" 'org-agenda-previous-item
    "z" 'org-agenda-view-mode-dispatch
    "h" 'aj/agenda-hydra/body
    "\C-h" 'evil-window-left
    ))
;;;###autoload
(defun aj/indent-if-not-webmode ()
  (if (equal 'web-mode major-mode) nil
    (newline-and-indent)))
;;;###autoload
(defun er/add-web-mode-expansions ()
  (require 'html-mode-expansions)
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(
                              web-mode-mark-and-expand
                              er/mark-html-attribute
                              er/mark-inner-tag
                              er/mark-outer-tag
                              ))))
;;;###autoload
(defun aj/remap-emmet (&optional beg end)
  "remaps keys for emmet-preview-keymap"
  (map!
   :map emmet-preview-keymap
   "M-r" #'emmet-preview-accept))
;;;###autoload
(defun aj/set-info-popup-width (&optional asdf asds)
  "Set width of info popup buffer"
  (if doom-big-font-mode
      (set! :popup "*info*" '((size . 0.6) (side . left)) '((select . t) (transient . nil)))
    (set! :popup "*info*" '((size . 0.4) (side . left)) '((select . t) (transient . nil)))))
;;;###autoload
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        css-indent-offset 2
        )
  )
;;;###autoload
(defun aj/insert-link-in-org()
  (interactive)
  (org-insert-link)
  ;; (evil-org-open-below 1)
  )
;;;###autoload
(defun josh/org-capture-refile-but-with-args (file headline &optional arg)
  "Copied from `org-capture-refile' since it doesn't allow passing arguments. This does."
  (unless (eq (org-capture-get :type 'local) 'entry)
    (error
     "Refiling from a capture buffer makes only sense for `entry'-type templates"))
  (let ((pos (point))
	(base (buffer-base-buffer (current-buffer)))
	(org-capture-is-refiling t)
	(kill-buffer (org-capture-get :kill-buffer 'local)))
    (org-capture-put :kill-buffer nil)
    (org-capture-finalize)
    (save-window-excursion
      (with-current-buffer (or base (current-buffer))
	(org-with-wide-buffer
	 (goto-char pos)
	 (my/refile file headline arg))))
    (when kill-buffer (kill-buffer base))))

;;;###autoload
(defun aj/my-org-faces ()
  "set org faces how I like them"
  (set-face-attribute     'org-level-1 nil                :height 1.0 :background nil)
  (set-face-attribute     'org-level-2 nil                :height 1.0)
  (set-face-attribute     'org-level-3 nil                :height 1.0)
  (set-face-attribute     'org-level-4 nil                :height 1.0)
  (set-face-attribute     'org-agenda-date nil            :height 1.0)
  (set-face-attribute     'org-agenda-date-today    nil   :height 1.0)
  (set-face-attribute     'org-agenda-date-weekend  nil   :height 1.0)
  (set-face-attribute     'org-agenda-structure     nil   :height 1.0)
  (setq org-fontify-whole-heading-line nil)
  )
