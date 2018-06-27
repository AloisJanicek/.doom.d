;;;  -*- lexical-binding: t; -*-

;;;###autoload
(defun +racket/repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*Racket REPL*")
       (progn (racket-repl)
              (let ((buf (get-buffer "*Racket REPL*")))
                (bury-buffer buf)
                buf)))))
;;;###autoload
(defun aj/racket--do-describe-for-company (sym &optional pop-to)
  "A helper for `racket-describe' and company-mode.

POP-TO should be t for the former (in which case some buttons are
added) and nil for the latter.

Returns the buffer in which the description was written."
  ;; (let* ((bufname "*Racket Describe*")
  (let* ((bufname " *company-documentation*")
         (html (racket--repl-command "describe %s" sym))
         ;; Emacs shr renderer removes leading &nbsp; from <td> elements
         ;; -- which messes up the indentation of s-expressions including
         ;; contracts. So replace &nbsp with `spc' in the source HTML,
         ;; and replace `spc' with " " after shr-insert-document outputs.
         (spc (string #x2020)) ;unlikely character (hopefully)
         (dom (with-temp-buffer
                (insert html)
                (goto-char (point-min))
                (while (re-search-forward "&nbsp;" nil t)
                  (replace-match spc t t))
                (libxml-parse-html-region (point-min) (point-max))))
         ;; Work around what seems to be a bug with shr -- inserting
         ;; elements out of order, when an existing Racket Describe buffer
         ;; hasn't had a quit-window -- by re-creating the bufer.
         ;; (buf (get-buffer bufname))
         ;; (_   (and buf (kill-buffer buf)))
         ;; (buf (get-buffer-create bufname))
         (pop-to nil)
         )
    (with-current-buffer (get-buffer-create bufname)
      (read-only-mode -1)
      (erase-buffer)
      (let ((shr-use-fonts nil))
        (shr-insert-document dom))
      (goto-char (point-min))
      (while (re-search-forward spc nil t)
        (replace-match " " t t))
      (current-buffer)
      )
    ;; (bury-buffer bufname)
    ))
