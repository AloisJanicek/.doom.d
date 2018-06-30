;;; ~/.doom.d/autoload/functions.el -*- lexical-binding: t; -*-
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
      (set-popup-rule! "*info*"                         :size 0.6 :side 'left :select t :transient nil)
    (set-popup-rule! "*info*"                         :size 0.4 :side 'left :select t :transient nil)
    ))
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
;;;###autoload
(defun aj/projectile-add-known-project-and-save (project-root)
  "Add PROJECT-ROOT to the list of known projects and save it to the list of known projects."
  (interactive (list (read-directory-name "Add to known projects: " "~/repos/")))
  (unless (projectile-ignored-project-p project-root)
    (setq projectile-known-projects
          (delete-dups
           (cons (file-name-as-directory (abbreviate-file-name project-root))
                 projectile-known-projects))))
  (projectile-save-known-projects))

;;;###autoload
(defun +ivy-projectile-find-file-combined-transformer (str)
  "Highlight entries that have been visited. This is the opposite of
`counsel-projectile-find-file'. And apply all-the-icons"
  (let ((s (format "%s\t%s"
                   (propertize "\t" 'display (all-the-icons-icon-for-file str))
                   str)))
    (cond ((get-file-buffer (projectile-expand-root str))
           (propertize s 'face '(:weight ultra-bold :slant italic)))
          (t s))))
;;;###autoload
(defun +ivy-recentf-combined-transformer (str)
  "Dim recentf entries that are not in the current project of the buffer you
started `counsel-recentf' from. Also uses `abbreviate-file-name'. And apply all-the-icons"
  (let* ((s (abbreviate-file-name str))
         (s (format "%s\t%s"
                    (propertize "\t" 'display (all-the-icons-icon-for-file str))
                    str))
         )
    (if (file-in-directory-p str (doom-project-root))
        s
      (propertize s 'face 'ivy-virtual))))
;;;###autoload
(defun +ivy-combined-buffer-transformer (str)
  "Dim special buffers, buffers whose file aren't in the current buffer, and
virtual buffers. Uses `ivy-rich' under the hood. And apply all-the-icons"
  (let* ((buf (get-buffer str))
         (mode (buffer-local-value 'major-mode buf))
         (s (format "%s\t%s"
                    (propertize "\t" 'display (or
                                               (all-the-icons-ivy--icon-for-mode mode)
                                               (all-the-icons-ivy--icon-for-mode (get mode 'derived-mode-parent))))
                    (all-the-icons-ivy--buffer-propertize buf str)))
         )
    (require 'ivy-rich)
    (cond (buf (ivy-rich-switch-buffer-transformer s))
          ((and (eq ivy-virtual-abbreviate 'full)
                ivy-rich-switch-buffer-align-virtual-buffer)
           (ivy-rich-switch-buffer-virtual-buffer s))
          ((eq ivy-virtual-abbreviate 'full)
           (propertize (abbreviate-file-name str) 's 'ivy-virtual))
          (t (propertize s 'face 'ivy-virtual)))))
;; this hook saves an ics file once an org-buffer is saved

;;;###autoload
(defun my-icalendar-agenda-export()
  "Export org agenda into ical file when saving GTD org file. Useful when in after-save-hook"
  (if (string= (expand-file-name +GTD) (buffer-file-name))
      (org-icalendar-combine-agenda-files)))


;;;###autoload
(defun aj/enable-flyspell-check-if-prog ()
  "Toggle flyspell mode with check for progn-derived mode"
  (interactive)
  (if (not flyspell-mode)
      (progn
        (flyspell-mode 1)
        (if (derived-mode-p 'prog-mode)
            (flyspell-prog-mode)))
    (flyspell-mode 0)))

;;;###autoload
(defun aj/swap-two-ispell-dicts (dict1 dict2)
  "If dict1 is active switch to dict2 or do it backwards"
  (interactive)
  (if (string= dict1 ispell-local-dictionary)
      (progn
        (ispell-change-dictionary dict2)
        (flyspell-mode 1))
    (progn
      (ispell-change-dictionary dict1)
      (flyspell-mode 1))
    ))
;;;###autoload
(defun my-imenu-list-hl-line ()
  (set (make-local-variable 'hl-line-face) ; This is how to make it local
       'hl-line-imenu-list-face)
  (hl-line-mode))

;;;###autoload
(defun aj/time-from-h-m (hm)
  "Takes HM which is a string representing time in format \"%H:%M\"
and returns that weird time number which Emacs understands."
  (let ((year (format-time-string "%Y" (current-time)))
        (space " ")
        (seconds ":00"))
    (date-to-time (concat (format-time-string "%a %b %d " (current-time))
                          hm seconds space year))))
;;;###autoload
(defun aj/goto-journal ()
  (interactive)
  (persp-remove-buffer "JOURNAL.org")
  (if (get-buffer "JOURNAL.org")
      (progn
        (pop-to-buffer "JOURNAL.org")
        (emacs-lock-mode 'kill))
    (progn
      (pop-to-buffer (find-file-noselect +JOURNAL))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode))))
;;;###autoload
(defun aj/goto-someday ()
  (interactive)
  (persp-remove-buffer "SOMEDAY.org")
  (if (get-buffer "SOMEDAY.org")
      (progn
        (pop-to-buffer "SOMEDAY.org")
        (emacs-lock-mode 'kill)
        (widen)
        (goto-char (point-min))
        (forward-line 3)
        (outline-show-branches)
        )
    (progn
      (pop-to-buffer (find-file-noselect +SOMEDAY))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode)
      (widen)
      (goto-char (point-min))
      (forward-line 3)
      (outline-show-branches)
      )))
;;;###autoload
(defun aj/goto-maybe ()
  (interactive)
  (persp-remove-buffer "MAYBE.org")
  (if (get-buffer "MAYBE.org")
      (progn
        (pop-to-buffer "MAYBE.org")
        (emacs-lock-mode 'kill)
        (widen)
        (goto-char (point-min))
        (forward-line 3)
        )
    (progn
      (pop-to-buffer (find-file-noselect +MAYBE))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode)
      (widen)
      (goto-char (point-min))
      (forward-line 3)
      )))
;;;###autoload
(defun aj/goto-GTD ()
  (interactive)
  (persp-remove-buffer "GTD.org")
  (if (get-buffer "GTD.org")
      (progn
        (pop-to-buffer "GTD.org")
        (emacs-lock-mode 'kill)
        (widen)
        (goto-char (point-min))
        (forward-line 6)
        )
    (progn
      (pop-to-buffer (find-file-noselect +GTD))
      (emacs-lock-mode 'kill)
      (widen)
      (goto-char (point-min))
      (forward-line 6)
      (turn-off-solaire-mode))))
;;;###autoload
(defun aj/goto-bookmarks ()
  "Selects and opens links"
  (interactive)
  (persp-remove-buffer "BOOKMARKS.org")
  (if (get-buffer +BOOKMARKS)
      (progn
        (pop-to-buffer "BOOKMARKS.org")
        (emacs-lock-mode 'kill)
        (widen)
        (counsel-org-goto-bookmarks))
    (progn
      (pop-to-buffer (find-file-noselect +BOOKMARKS))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode)
      (widen)
      (counsel-org-goto-bookmarks))))
;;;###autoload
(defun aj/goto-private-wiki ()
  "Go to my private wiki and browse it"
  (interactive)
  (persp-remove-buffer "private-wiki.org")
  (require 'counsel)
  (if (get-buffer "private-wiki.org")
      (progn
        (pop-to-buffer "private-wiki.org")
        (emacs-lock-mode 'kill)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +private-wiki))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode)
      (counsel-org-goto-private-wiki))))
;;;###autoload
(defun aj/goto-environment-wiki ()
  "Go to my environment wiki and browse it"
  (interactive)
  (persp-remove-buffer "environment-wiki.org")
  (require 'counsel)
  (if (get-buffer "environment-wiki.org")
      (progn
        (pop-to-buffer "environment-wiki.org")
        (emacs-lock-mode 'kill)
        (goto-char (point-min))
        (forward-line 8)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +environment-wiki))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode)
      (counsel-org-goto-private-wiki))))
;;;###autoload
(defun aj/goto-education-wiki ()
  "Go to my environment wiki and browse it"
  (interactive)
  (persp-remove-buffer "education-wiki.org")
  (require 'counsel)
  (if (get-buffer "education-wiki.org")
      (progn
        (pop-to-buffer "education-wiki.org")
        (emacs-lock-mode 'kill)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +education-wiki))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode)
      (counsel-org-goto-private-wiki))))
;;;###autoload
(defun aj/goto-work-wiki ()
  "Go to my work wiki and browse it,narrow it"
  (interactive)
  (persp-remove-buffer "work-wiki.org")
  (require 'counsel)
  (if (get-buffer "work-wiki.org")
      (progn
        (pop-to-buffer "work-wiki.org")
        (goto-char (point-min))
        (emacs-lock-mode 'kill)
        (goto-char (point-min))
        (forward-line 6)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +work-wiki))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode)
      (counsel-org-goto-private-wiki))))
;;;###autoload
(defun aj/goto-build-wiki ()
  "Go to my work wiki and browse it,narrow it"
  (interactive)
  (persp-remove-buffer "build-wiki.org")
  (require 'counsel)
  (if (get-buffer "build-wiki.org")
      (progn
        (pop-to-buffer "build-wiki.org")
        (emacs-lock-mode 'kill)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +build-wiki))
      (emacs-lock-mode 'kill)
      (turn-off-solaire-mode)
      (counsel-org-goto-private-wiki))))
;;;###autoload
(defun aj-strike-through-org-headline ()
  "Strikes through headline in org mode.
Searches for beginning of text segment of a headline under the point, inserts \"+\",
then tests if headlines has tags and inserts another \"+\" sign at the end
of text segment of current headline.
"
  (interactive)
  (save-excursion
    (goto-char (search-backward "\*"))
    (evil-forward-WORD-begin)
    (insert "+")
    (if (equal (org-get-tags-string) "")
        (progn
          (end-of-line)
          (insert "+")
          (save-buffer))
      (progn
        (search-forward ":")
        (backward-char 2)
        (insert "+")
        (save-buffer))
      )))
;;;###autoload
(defun aj/org-agenda-current-file ()
  "Show org agenda list for current file only"
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)))
;;;###autoload
(defun obsoke/ediff-dotfile-and-template ()
  "ediff the current `dotfile' with the template"
  (interactive)
  (ediff-files
   "~/.doom.d/init.el"
   "~/.emacs.d/init.example.el"))
;;;###autoload
(defun my-org-retrieve-url-from-point-for-ivy (x)
  (interactive)
  (with-ivy-window
    (org-goto-marker-or-bmk (cdr x))
    (forward-char 4)
    (let* ((link-info (assoc :link (org-context)))
           (text (when link-info
                   ;; org-context seems to return nil if the current element
                   ;; starts at buffer-start or ends at buffer-end
                   (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                   (or (caddr link-info) (point-max)))))
           (my-buffer (buffer-name)))
      (if (not text)
          (error "Not in org link")
        (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)
        (kill-new text)
        (kill-buffer my-buffer)
        ))))
;;;###autoload
(defun my-org-retrieve-url-from-point (&optional x)
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 ;; org-context seems to return nil if the current element
                 ;; starts at buffer-start or ends at buffer-end
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)

      (kill-new text))))
;;;###autoload
(defun my-smarter-kill-ring-save ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-ring-save)
    (when (eq major-mode 'org-mode)
      (call-interactively #'my-org-retrieve-url-from-point))))
;;;###autoload
(defun counsel-org-goto-bookmarks ()
  "Browse my bookmarks"
  (interactive)
  (ivy-read "Goto: " (counsel-org-goto--get-headlines)
            :history 'counsel-org-goto-history
            ;; :action 'aj/create-new-org-l1-heading
            :action 'counsel-org-goto-open-org-link
            :caller 'counsel-org-goto))
;;;###autoload
(defun aj/create-new-org-l1-heading (x)
  "Creates new top level heading in current org file from which ivy was called"
  (interactive)
  (with-ivy-window
    (goto-char (point-min))
    (org-insert-heading-respect-content)
    (insert x)
    (org-id-get-create)
    (goto-char (point-min))
    (forward-line 1)
    (org-cycle)
    (evil-open-below 1)))
;;;###autoload
(defun counsel-org-goto-private-wiki ()
  "Go to a different location in my private wiki file."
  (interactive)
  (let ((ivy-height 40)
        (ivy-posframe-font (font-spec :family "Iosevka" :size 18))
        (ivy-posframe-parameters `((min-width . 120)
                                   (height . 30)
                                   (min-height . ,ivy-height)
                                   (internal-border-width . 20))))
    (ivy-read "Goto: " (counsel-org-goto--get-headlines)
              :history 'counsel-org-goto-history
              ;; :action 'aj/create-new-org-l1-heading
              :action 'counsel-org-goto-wiki-action
              :caller 'counsel-org-goto))
  )

;;;###autoload
(defun aj/refile-to-file-headline (file headline &optional arg)
  "Refile to HEADLINE in FILE. Clean up org-capture if it's activated.

With a `C-u` ARG, just jump to the headline."
  (interactive "P")
  (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode)))
    (cond
     ((and arg (listp arg))	    ;Are we jumping?
      (my/refile file headline arg))
     ;; Are we in org-capture-mode?
     (is-capturing      	;Minor mode variable that's defined when capturing
      (josh/org-capture-refile-but-with-args file headline arg))
     (t
      (my/refile file headline arg)))
    (when (or arg is-capturing)
      (setq hydra-deactivate t))))
;;;###autoload
(defun my/org-pomodoro-text-time ()
  "Return status info about org-pomodoro and if org-pomodoro is not running, try to print info about org-clock.
If either org-pomodoro or org-clock aren't active, print \"No Active Task \" "
  (interactive)
  (if (featurep 'org-pomodoro)
      (cond ((equal :none org-pomodoro-state)
             (if (org-clock-is-active)
                 (format "Clocked task: %d minutes - %s"
                         (org-clock-get-clocked-time) (substring-no-properties org-clock-heading))
               "No Active task"))
            ((equal :pomodoro org-pomodoro-state)
             (format "%d - Pomodoro: %d minutes - %s"
                     org-pomodoro-count (/ (org-pomodoro-remaining-seconds) 60) (substring-no-properties org-clock-heading)))
            ((equal :short-break org-pomodoro-state) "Short Break")
            ((equal :long-break org-pomodoro-state) "Long Break"))))
;;;###autoload
(defun aj/update-org-clock-heading ()
  "Updates org-clock-heading"
  (interactive)
  (save-excursion
    (org-clock-goto)
    (setq org-clock-heading
          (cond ((and org-clock-heading-function
                      (functionp org-clock-heading-function))
                 (funcall org-clock-heading-function))

                ((nth 4 (org-heading-components))
                 (replace-regexp-in-string
                  "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
                  (match-string-no-properties 4)))
                (t "???")))
    (bury-buffer)))
;;;###autoload
(defun aj/return-project-org-file ()
  "Returns project org file"
  (interactive)
  (list (concat (projectile-project-root) "README.org")))
;;;###autoload
(defun aj/return-plain-string-project-org-file ()
  "Returns project org file"
  (interactive)
  (concat (projectile-project-root) "README.org"))
;;;###autoload
(defun aj/find-and-open-org-projectile-file ()
  "Find and open org-projectile file"
  (interactive)
  (find-file (concat (projectile-project-root) "README.org"))
  (goto-char (org-find-exact-headline-in-buffer "TASKS"))
  )
;;;###autoload
(defun aj/goto-current-org-projectile-file ()
  "Go to the current org-projectile-file"
  (interactive)
  (save-excursion
    (find-file (concat (projectile-project-root) "README.org"))
    (counsel-org-goto)))
;;;###autoload
(defun aj/org-projectile-capture-for-current-project ()
  "Call standard capture template for current org-projectile file"
  (interactive)
  (org-capture nil "h")
  )
;;;###autoload
(defun aj/org-brain-per-project ()
  "Opens org-brain-visualize for current projectile project."
  (interactive)
  (let ((org-brain-path (projectile-project-root)))
    (org-brain-visualize (aj/return-plain-string-project-org-file))))
;;;###autoload
(defun my/org-brain-goto (&optional entry goto-file-func)
  "Goto buffer and position of org-brain ENTRY.
If ENTRY isn't specified, ask for the ENTRY.
Unless GOTO-FILE-FUNC is nil, use `pop-to-buffer-same-window' for opening the entry."
  (interactive)
  (require 'org-brain)
  (org-brain-stop-wandering)
  (unless entry (setq entry (org-brain-choose-entry
                             "Entry: "
                             (append (org-brain-files t)
                                     (org-brain-headline-entries))
                             nil t)))
  (let ((marker (org-brain-entry-marker entry)))
    (apply (or goto-file-func #'pop-to-buffer-same-window)
           (list (marker-buffer marker)))
    (widen)
    (org-set-visibility-according-to-property)
    (goto-char (marker-position marker))
    ;; (org-show-entry)
    (outline-show-branches)
    (org-narrow-to-subtree)
    )
  entry)
;;;###autoload
(defun my/org-brain-goto-current (&optional same-window)
  "Use `org-brain-goto' on `org-brain-entry-at-pt', in other window..
If run with `\\[universal-argument]', or SAME-WINDOW as t, use current window."
  (interactive "P")
  (require 'org-brain)
  (if same-window
      (my/org-brain-goto (org-brain-entry-at-pt))
    (my/org-brain-goto (org-brain-entry-at-pt) #'pop-to-buffer)))
;;;###autoload
(defun aj/org-brain-visualize-entry-at-pt ()
  "Helper function for direct visualizing of entry at point"
  (interactive)
  (require 'org-brain)
  (progn
    (org-brain-visualize (org-brain-entry-at-pt))))
;;;###autoload
;; (defun pack-info-add-directories ()
;;   (interactive)
;;   (require 'info)
;;   (require 'f)
;;   (require 'dash)
;;   (let ((old-info-dirs Info-additional-directory-list))
;;     (setq Info-additional-directory-list nil)
;;     (setq Info-additional-directory-list
;;           (-concat
;;            (--filter (file-exists-p (expand-file-name "dir" it))
;;                      (f-directories package-user-dir))
;;            old-info-dirs))))
;;;###autoload
(defun aj/clock-menu ()
  "Present recent clocked tasks"
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-clock-in-last))
;;;###autoload
(defun aj/better-open-current-projectile-org-file ()
  "Opens current project org file as popup buffer to quickly peak into"
  (interactive)
  (let ((my-buffer (concat (projectile-project-name) "/README.org")))
    (if (get-file-buffer my-buffer)
        (pop-to-buffer my-buffer)
      (pop-to-buffer (find-file-noselect (concat (projectile-project-root) "README.org"))))))
;;;###autoload
(defun aj/project ()
  (interactive)
  "Shows project agenda"
  (progn
    (projectile-project-root)
    (projectile-project-name)
    (org-agenda nil "C"))
  )
;;;###autoload
(defun aj-mpdel-playlist-open (&optional playlist)
  "Open a buffer to popup with PLAYLIST, current playlist if nil."
  (interactive)
  (let* ((playlist (or playlist (libmpdel-current-playlist)))
         (buffer (mpdel-playlist--buffer playlist)))
    (with-current-buffer buffer
      (mpdel-playlist-mode)
      (setq mpdel-playlist-playlist playlist)
      (mpdel-playlist-refresh buffer))
    (pop-to-buffer buffer)
    (mpdel-playlist--register-to-hooks buffer)))
;;;###autoload
(defun aj/toggle-doom-theme ()
  "Toggle between light and dark theme"
  (interactive)
  (if (equal 'doom-one doom-theme)
      (progn
        (setq doom-theme 'doom-solarized-light)
        (doom/reload-theme))
    (progn
      (setq doom-theme 'doom-one)
      (doom/reload-theme))))
;;;###autoload
(defun aj/my-swiper ()
  "Launch swiper with different ivi-height (12)"
  (interactive)
  (let ((ivy-height 15))
    (counsel-grep-or-swiper)))
;;;###autoload
(defun aj/mark-region-and-preview-emmet ()
  "Marks whole line before current point possition and starts emmet-preview for marked region"
  (interactive)
  (let ((end (point))
        (beg (progn
               (evil-first-non-blank)
               (point))))
    (evil-last-non-blank)
    (forward-char)
    (emmet-preview beg end)))
;;;###autoload
(defun aj/set-term-keys ()
  (interactive)
  (evil-define-key 'insert term-raw-map
    (kbd "C-h") 'evil-window-left
    (kbd "C-j") 'evil-window-down
    (kbd "C-k") 'evil-window-up
    (kbd "C-<right>") 'next-buffer
    (kbd "C-<left>") 'previous-buffer
    (kbd "M-1") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 0)))
    (kbd "M-2") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 1)))
    (kbd "M-3") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 2)))
    (kbd "M-4") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 3)))
    (kbd "M-5") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 4)))
    (kbd "M-6") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 5)))
    (kbd "M-7") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 6)))
    (kbd "M-8") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to 7)))
    (kbd "M-0") (function
                 (lambda nil
                   (interactive)
                   (+workspace/switch-to-last)))
    (kbd "M-t") (function
                 (lambda nil
                   (interactive)
                   (+workspace/new)))
    ;; (kbd "C-l") 'evil-window-right
    )
  )
;;;###autoload
(defun aj/insert-link-into-org-heading ()
  "Marks current heading text and then inserts link"
  (interactive)
  (progn
    (end-of-line)
    (set-mark (point))
    (search-backward "*")
    (forward-char)
    (forward-char)
    (org-insert-link)
    )
  )
;;;###autoload
(defun aj/insert-link-into-org-list-item ()
  "Marks current list item text and then inserts link"
  (interactive)
  (progn
    (end-of-line)
    (set-mark (point))
    (search-backward "-")
    (forward-char)
    (forward-char)
    (org-insert-link)
    )
  )
;;;###autoload
(defun aj/save-session-as ()
  "Save current session and ask for the name, because you calling it with C-U prefix"
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively '+workspace/save-session))
;;;###autoload
(defun beautify-html-file-and-revert ()
  "Beautify file with html-beautify and only if major mode is web-mode"
  (interactive)
  (when (eq major-mode 'web-mode)
    (message "html-beautify taking care of your markup" (buffer-file-name))
    (shell-command (concat "html-beautify --quiet --replace -s 2 -w 120 -A \"auto\" -I -E \"\" --max-preserve-newlines 0 -f " (buffer-file-name)))
    (revert-buffer t t)))
;;;###autoload
(defun prettier-stylelint-fix-file-and-revert ()
  "Prettify current file and apply autofixes only in css-mode"
  (interactive)
  (when (or (eq major-mode 'css-mode) (eq major-mode 'scss-mode))
    (message "prettier-stylelint fixing the file" (buffer-file-name))
    (shell-command (concat "prettier-stylelint --quiet --write " (buffer-file-name)))
    (revert-buffer t t)))
;;;###autoload
(defun aj/update-my-doom-theme ()
  "Update my Doom theme. I should not this this way, but..."
  (interactive)
  (progn
    (byte-compile-file "/tmp/doom-breeze-theme.el")
    (shell-command "cd /tmp/ && cp doom-breeze* ~/.emacs.d/.local/packages/elpa/doom-themes*")
    (shell-command "ls ~/.emacs.d/.local/packages/elpa/doom-themes*")
    )
  )
;;;###autoload
(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Bash history:") collection))))
      (kill-new val)
      (message "%s => kill-ring" val))))
;;;###autoload
(defun aj/my-backup ()
  "Execute shell script for backup"
  (interactive)
  (progn
    (shell-command "backup-org.sh")
    ))
;;;###autoload
(defun aj/insert-file-octals-identify-into-src-block-header ()
  "For file under the point it inserts its file permission in octal format at the end of the current line"
  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         ($path
          (replace-regexp-in-string
           "^sudo::" "" $inputStr)))
    (progn
      (end-of-line)
      (if (file-exists-p $path)
          (insert (concat " :tangle-mode (identity #o" (replace-regexp-in-string "\n" ""(shell-command-to-string (concat "stat -c %a " $path))) ")" ))
        (print "file doesn't exists")))))
;;;###autoload
(defun aj/go-to-per-project-bookmark()
  "First it updates bookmark file location to project-specific and then calls counsel on it"
  (interactive)
  (let ((bookmark-default-file (concat (projectile-project-name) "/bookmarks")))
    (counsel-bookmark)))

;;;###autoload
                                        ; TODO: replace "link: " with actual domain name - useful for hyper links with titles
(defun gk-browse-url (&rest args)
  "Prompt for whether or not to browse with EWW, if no browse
with external browser."
  (apply
   (if (y-or-n-p (concat "link: " "Browse with EWW? "))
       'eww-browse-url
     #'browse-url-xdg-open)
   args))

;;;###autoload
(defun aj/jump-to-org-dir ()
  "Jumps to org directory"
  (interactive)
  (let ((default-directory "~/org/"))
    (counsel-find-file)))

;;;###autoload
(defun counsel-projectile-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist."
  (interactive)
  (require 'bookmark)
  (let ((projectile-bookmarks (projectile-bookmarks)))
    (ivy-read "Create or jump to bookmark: "
              projectile-bookmarks
              :action (lambda (x)
                        (cond ((and counsel-bookmark-avoid-dired
                                    (member x projectile-bookmarks)
                                    (file-directory-p (bookmark-location x)))
                               (with-ivy-window
                                 (let ((default-directory (bookmark-location x)))
                                   (counsel-find-file))))
                              ((member x projectile-bookmarks)
                               (with-ivy-window
                                 (bookmark-jump x)))
                              (t
                               (bookmark-set x))))
              :caller 'counsel-projectile-bookmark)))


;;;###autoload
(defun projectile-bookmarks ()
  (let ((bmarks (bookmark-all-names)))
    (cl-remove-if-not #'workspace-bookmark-p bmarks)))

;;;###autoload
(defun workspace-bookmark-p (bmark)
  (let ((bmark-path (expand-file-name (bookmark-location bmark))))
    (string-prefix-p (bmacs-project-root) bmark-path)))

;;;###autoload
(defun bmacs-project-root ()
  "Get the path to the root of your project.
If STRICT-P, return nil if no project was found, otherwise return
`default-directory'."
  (let (projectile-require-project-root)
    (projectile-project-root)))
;;;###autoload
(defun browse-webster-at-point ()
  (interactive)
  (browse-url (concat "https://www.merriam-webster.com/dictionary/" (thing-at-point 'word))))
;;;###autoload
(defun browse-dictionary-at-point ()
  (interactive)
  (browse-url (concat "https://dictionary.com/browse/" (thing-at-point 'word))))

;;;###autoload
(defun ivy-yasnippet--copy-edit-snippet-action (template-name)
  (let ((inhibit-read-only t))
    (ivy-yasnippet--revert))
  (yas-new-snippet)
  (erase-buffer)
  (insert-file-contents
   (yas--template-get-file
    (ivy-yasnippet--lookup-template template-name))
   nil 0 500))

;;;###autoload
(defun aj/new-project-init-and-register (fp gitlab project)
  (call-process-shell-command (concat "cd " fp " && " "git init"))
  (if (string-equal "yes" gitlab)
      (progn
        (call-process-shell-command (concat "lab project create " project))
        (call-process-shell-command (concat "cd " fp " && " "git remote rename origin old-origin"))
        (call-process-shell-command (concat "cd " fp " && " "git remote add origin git@gitlab.com:AloisJanicek/" project ".git"))
        (call-process-shell-command (concat "cd " fp " && " "git push -u origin --all"))
        (call-process-shell-command (concat "cd " fp " && " "git push -u origin --tags"))))
  (aj/projectile-add-known-project-and-save fp)
  (projectile-switch-project-by-name fp))

;;;###autoload
(defun aj/project-bootstrap ()
  (interactive)
  (let* ((project (read-string "New project name: "))
         (directory (read-directory-name "Directory: " "~/repos/"))
         (template (ivy-read "Template: " '("web-starter-kit" "other")))
         (gitlab (ivy-read "Gitlab?:" '("yes" "no")))
         (full-path (concat directory project))
         )
    ;; create directory
    (make-directory full-path)

    (if (string-equal template "web-starter-kit")
        (progn
          (call-process-shell-command (concat "git clone git@gitlab.com:AloisJanicek/web-starter-kit.git " full-path))
          (delete-directory (concat full-path "/.git/") t)
          (aj/new-project-init-and-register full-path gitlab project)
          )
      (aj/new-project-init-and-register full-path gitlab project))))

;; TODO
;;;###autoload
(defun aj/visualize-brain-and-take-care-of-buffers ()
  "Visualize all brain org files and them hide them from perspectives"
  (interactive)
  (let ((persp-autokill-buffer-on-remove nil))
    (call-interactively 'org-brain-visualize)
    (persp-remove-buffer +persp-blacklist)))

;; TODO
;;;###autoload
(defun aj/browse-brain-files ()
    "browse brain files and bring selected one to the current perspective")

;;;###autoload
(defun aj/open-agenda-time-dependent ()
  "Open `org-agenda' depending on what time is it"
  (interactive)
  (mapcar (lambda (element)
            (let ((hm (car element))
                  (agenda-key (cdr element)))
              (if (not (time-less-p (current-time) (aj/time-from-h-m hm)))
                  (org-agenda nil agenda-key))))
          +aj/time-blocks))

;;;###autoload
(defun aj/open-imenu-sidebar ()
  "Remove `+imenu|clean-on-popup-close' form `+popup-buffer-mode-hook' and open
imenu-list sidbar so it doesn't get closed in any other way then from inside of it"
  (interactive)
  (progn
    (require 'imenu-list)
    (remove-hook '+popup-buffer-mode-hook '+imenu|cleanup-on-popup-close)
    (imenu-list)))