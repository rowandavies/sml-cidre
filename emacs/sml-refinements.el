;; Extension of sml-mode to handle refinement annotations
;; Author: Rowan Davies
;;
;; Designed to work with sml-mode 3.9.5 and 3.3 or similar.
;; 
;; This code extends sml-mode so that it performs fontification of
;; refinement annotations in SML programs, using
;; `font-lock-refinement-face' and
;; `font-lock-refinement-keyword-face'.
;;
;; Currently it also does fontification of strings and comments, since
;; it is based on the sml-mode version 3.3 code for these.  (We need
;; to detect comments and strings anyway to avoid fontifying inside
;; them.)
;;
;; This code can be a little slow with large SML programs.  You may
;; want to use either `fast-lock-mode', `lazy-lock-mode', or
;; `lazy-shot-mode'.
;;
;; I haven't yet attempted to extend the sml-mode indentation to
;; refinement annotations.
;;
;; Customizations can be added to this code using
;; `sml-refinements-load-hook'.
;;
;; To use this mode:
;;
;; Make sure that this file is on your load path, perhaps by putting 
;; something like the following in your .emacs file.
;;
;;    (setq load-path (cons "/afs/cs/user/rowan/ssml/emacs" load-path))
;; 
;; Then, add the following to your .emacs file:
;;
;;    (add-hook 'sml-mode-hook '(lambda () "" (require 'sml-refinements)))
;;
;; If you've already loaded sml-mode, you may need to restart emacs.


(require 'sml-mode)

;; Add the function sml-refinement-comments-and-string (defined below)
;; to variable sml-font-lock-keywords (in sml-mode 3.9.5) or variable
;; sml-font-lock-all (sml-mode 3.3) so that it is called whenever SML
;; code needs to be fontified.
(if (boundp 'sml-font-lock-keywords)
    ;; sml-mode 3.9.5	 
    (setq sml-font-lock-keywords (append (delete '(sml-refinement-comments-and-strings)
						 sml-font-lock-keywords) 
					 '((sml-refinement-comments-and-strings))))
  ;; sml-mode 3.2 or 3.3 (or similar)
  (require 'sml-font)
  (defun sml-font-lock-setup ()  ;; Override version in sml-font.el!
     "Set buffer-local font-lock variables and possibly turn on
     font-lock WITH refinements (overides version in sml-font)."
     (let ((new-font-lock (boundp 'font-lock-defaults)))
       (or sml-font-lock-all
	   (setq sml-font-lock-all
		 (append
		  ;(and new-font-lock (list (list 'sml-font-comments-and-strings)))
		  sml-font-lock-extra-keywords
		  (list (list sml-font-lock-standard-keywords 1
			      'font-lock-keyword-face))
		  '((sml-refinement-comments-and-strings)) ;; Added for refinements
		 )))
       (cond (new-font-lock
	      (make-local-variable 'font-lock-defaults)
	      (setq font-lock-defaults `(sml-font-lock-all t)))
	     (t
	      (setq font-lock-keywords sml-font-lock-all))))
     (and sml-font-lock-auto-on (turn-on-font-lock)) ))

(defface font-lock-refinement-keyword-face   '((t (:foreground "seagreen")))
  "Font Lock mode face used for keywords in refinement comments."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-refinement-keyword-face 'font-lock-refinement-keyword-face
  "Variable for face to use for keywords in refinement comments,
  although you should use face directly instead.")

(defface font-lock-refinement-face   '((t (:foreground "seagreen")))
  "Font Lock mode face used in refinement comments."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-refinement-face 'font-lock-refinement-face
  "Variable for face to use for refinement comments, although you
  should use face directly instead.")

(defvar sml-refine-cache '((0 . normal))
  "List of (POSITION . STATE) pairs for an SML buffer.
The STATE is either `normal', `comment', `string', `refinements' or
`refcomment'.  The POSITION is immediately after the token that
caused the state change.")

(defvar sml-refinement-keywords "\\(\\<\\(val\\)\\|\\(datasort\\)\\|\\(and\\)\\|\\(of\\)\\|\\(sortdef\\)\\)\\|\\(\\(assumesig\\)\\|\\(subsort\\)\\>\\)"
  "Keywords that may appear in ssml refinement comments")

(make-variable-buffer-local 'sml-refine-cache)

(defun sml-refinement-comments-and-strings (limit)
  "Fontify SML comments and strings up to LIMIT.
Handles nested comments and SML's escapes for breaking a string over lines.
Uses sml-refine-cache to maintain the fontification state over the buffer."
  (let ((beg (point))
	last class
        (refcomment-regexp (concat "\\((\\*\\)\\|\\(\\]\\*)\\)\\|" sml-refinement-keywords)))
    (while (< beg limit)
      (while (and sml-refine-cache
		  (> (car (car sml-refine-cache)) beg))
	(setq sml-refine-cache (cdr sml-refine-cache)))
      (setq last (car (car sml-refine-cache)))
      (setq class (cdr (car sml-refine-cache)))
      (goto-char last)
      (cond
       ((eq class 'normal)
	(cond
	 ((not (re-search-forward "\\((\\*[^[]\\)\\|\\(\"\\)\\|\\((\\*\\[\\)" limit 'end))
	  (goto-char limit))
	 ((match-beginning 1)
	  (setq sml-refine-cache (cons (cons (point) 'comment) sml-refine-cache)))
	 ((match-beginning 2)
	  (setq sml-refine-cache (cons (cons (point) 'string) sml-refine-cache)))
	 ((match-beginning 3)
	  (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-refinement-face)
	  (setq sml-refine-cache (cons (cons (point) 'refinements) sml-refine-cache)))))
       ((eq class 'comment)
	(cond
	 ((let ((nest 1))
	    (while (and (> nest 0)
			(re-search-forward "\\((\\*\\)\\|\\(\\*)\\)" limit 'end))
	      (cond
	       ((match-beginning 1) (setq nest (+ nest 1)))
	       ((match-beginning 2) (setq nest (- nest 1)))))
	    (> nest 0))
	  (goto-char limit))
	 (t
	  (setq sml-refine-cache (cons (cons (point) 'normal) sml-refine-cache))))
	(put-text-property (- last 3) (point) 'face 'font-lock-comment-face))
       ((eq class 'refcomment)
	(cond
	 ((let ((nest 1))
	    (while (and (> nest 0)
			(re-search-forward "\\((\\*\\)\\|\\([^]]\\*)\\)" limit 'end))
	      (cond
	       ((match-beginning 1) (setq nest (+ nest 1)))
	       ((match-beginning 2) (setq nest (- nest 1)))))
	    (> nest 0))
	  (goto-char limit))
	 (t
	  (setq sml-refine-cache (cons (cons (point) 'refinements) sml-refine-cache))
         ))
	(put-text-property (- last 3) (point) 'face 'font-lock-comment-face))
       ((eq class 'refinements)
	(cond
 	 ((prog1 (not (re-search-forward refcomment-regexp limit 'end))
  	         (put-text-property last (point) 'face 'font-lock-refinement-face) nil)
	  (goto-char limit))
	 ((match-beginning 1)
	  (setq sml-refine-cache (cons (cons (point) 'refcomment) sml-refine-cache)))
	 ((match-beginning 2) 
	  (setq sml-refine-cache (cons (cons (point) 'normal) sml-refine-cache)))
	 (t
	  (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-refinement-keyword-face)
  	  (setq sml-refine-cache (cons (cons (point) 'refinements) sml-refine-cache)))))
       ((eq class 'string)
	(while (and (re-search-forward
		     "\\(\"\\)\\|\\(\\\\\\s-*\\\\\\)\\|\\(\\\\\"\\)" limit 'end)
		     (not (match-beginning 1))))
	(cond
	 ((match-beginning 1)
	  (setq sml-refine-cache (cons (cons (point) 'normal) sml-refine-cache)))
	 (t
	  (goto-char limit)))
	(put-text-property (- last 1) (point) 'face 'font-lock-string-face)))
      (setq beg (point)))))

(run-hooks 'sml-refinements-load-hook)

(provide 'sml-refinements)