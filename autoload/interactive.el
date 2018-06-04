;;; ~/.doom.d/autoload/interactive.el -*- lexical-binding: t; -*-
;;;###autoload
(defun aj/goto-bookmarks ()
  "Go to my private wiki and browse it,narrow it"
  (interactive)
  (let ((my-current-buffer (buffer-name)))
    (progn
      (find-file "~/org/bookmarks.org")
      (widen)
      (counsel-org-goto-bookmarks)
      (switch-to-buffer my-current-buffer))))
;;;###autoload
(defun aj/goto-private-wiki ()
  "Go to my private wiki and browse it"
  (interactive)
  (require 'counsel)
  (if (get-buffer "private-wiki.org")
      (progn
        (pop-to-buffer "private-wiki.org")
        (emacs-lock-mode 'kill)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +private-wiki))
      (emacs-lock-mode 'kill)
      (counsel-org-goto-private-wiki))))
;;;###autoload
(defun aj/goto-environment-wiki ()
  "Go to my environment wiki and browse it"
  (interactive)
  (require 'counsel)
  (if (get-buffer "environment-wiki.org")
      (progn
        (pop-to-buffer "environment-wiki.org")
        (emacs-lock-mode 'kill)
        (goto-line 8)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +environment-wiki))
      (emacs-lock-mode 'kill)
      (counsel-org-goto-private-wiki))))
;;;###autoload
(defun aj/goto-education-wiki ()
  "Go to my environment wiki and browse it"
  (interactive)
  (require 'counsel)
  (if (get-buffer "education-wiki.org")
      (progn
        (pop-to-buffer "education-wiki.org")
        (emacs-lock-mode 'kill)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +education-wiki))
      (emacs-lock-mode 'kill)
      (counsel-org-goto-private-wiki))))
;;;###autoload
(defun aj/goto-work-wiki ()
  "Go to my work wiki and browse it,narrow it"
  (interactive)
  (require 'counsel)
  (if (get-buffer "work-wiki.org")
      (progn
        (pop-to-buffer "work-wiki.org")
        (emacs-lock-mode 'kill)
        (goto-line 5)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +work-wiki))
      (emacs-lock-mode 'kill)
      (counsel-org-goto-private-wiki))))
;;;###autoload
(defun aj/goto-build-wiki ()
  "Go to my work wiki and browse it,narrow it"
  (interactive)
  (require 'counsel)
  (if (get-buffer "build-wiki.org")
      (progn
        (pop-to-buffer "build-wiki.org")
        (emacs-lock-mode 'kill)
        (aj/wiki-select/body))
    (progn
      (pop-to-buffer (find-file-noselect +build-wiki))
      (emacs-lock-mode 'kill)
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
    (forward-line 1)
    (org-cycle)
    (evil-open-below 1)))
;;;###autoload
(defun counsel-org-goto-private-wiki ()
  "Go to a different location in my private wiki file."
  (interactive)
  (ivy-read "Goto: " (counsel-org-goto--get-headlines)
            :history 'counsel-org-goto-history
            ;; :action 'aj/create-new-org-l1-heading
            :action 'counsel-org-goto-wiki-action
            :caller 'counsel-org-goto))
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
(defun pack-info-add-directories ()
  (interactive)
  (require 'info)
  (require 'f)
  (require 'dash)
  (let ((old-info-dirs Info-additional-directory-list))
    (setq Info-additional-directory-list nil)
    (setq Info-additional-directory-list
          (-concat
           (--filter (file-exists-p (expand-file-name "dir" it))
                     (f-directories package-user-dir))
           old-info-dirs))))
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
    (org-agenda nil "p"))
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
        (setq doom-theme 'doom-one-light)
        (doom//reload-theme))
    (progn
      (setq doom-theme 'doom-one)
      (doom//reload-theme))))
;;;###autoload
(defun aj/my-swiper ()
  "Launch swiper with different ivi-height (12)"
  (interactive)
  (let ((ivy-height 12))
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
