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
