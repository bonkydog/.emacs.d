;;; -*- lexical-binding: t; -*-

;;; Paredit

(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

(require 'paredit)
(require 'thingatpt)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook    'enable-paredit-mode)

;; Paredit extensions from Emacs Live

(defun live-paredit-next-top-level-form ()
  (interactive)
  (while (ignore-errors (paredit-backward-up) t))
  (live-paredit-forward))

(defun live-paredit-previous-top-level-form ()
  (interactive)
  (if (ignore-errors (paredit-backward-up) t)
      (while (ignore-errors (paredit-backward-up) t))
    (paredit-backward)))

(defun live-paredit-forward ()
  "Feels more natural to move to the beginning of the next item
   in the sexp, not the end of the current one."
  (interactive)
  (if (and (not (paredit-in-string-p))
           (save-excursion
             (ignore-errors
               (forward-sexp)
               (forward-sexp)
               t)))
      (progn
        (forward-sexp)
        (forward-sexp)
        (backward-sexp))
    (paredit-forward)))

(defun live-paredit-forward-slurp-sexp-neatly ()
  (interactive)
  (save-excursion
    (cond ((or (paredit-in-comment-p)
               (paredit-in-char-p))
           (error "Invalid context for slurping S-expressions."))
          ((paredit-in-string-p)
           (paredit-forward-slurp-into-string))
          (t

           (save-excursion
             (paredit-forward-up)
             (paredit-backward-down)
             (paredit-forward-slurp-sexp)
             (just-one-space)))))
  (when (not (save-excursion
               (ignore-errors
                 (backward-sexp)
                 t)))
    (delete-horizontal-space)))

(defun live-paredit-forward-kill-sexp (&optional arg)
  (interactive "p")
  (cond ((or (paredit-in-comment-p)
             (paredit-in-string-p)) (kill-word (or arg 1)))
        (t (kill-sexp (or arg 1)))))

(defun live-paredit-backward-kill-sexp (&optional arg)
  (interactive "p")
  (cond ((or (paredit-in-comment-p)
             (paredit-in-string-p)) (backward-kill-word (or arg 1)))
        (t (backward-kill-sexp (or arg 1)))))

(defun live-paredit-backward-kill ()
  (interactive)
  (let ((m (point-marker)))
    (paredit-backward-up)
    (forward-char)
    (delete-region (point) m)))

(defun live-paredit-delete-horizontal-space ()
  (interactive)
  (just-one-space -1)
  (paredit-backward-delete))

(defun live-paredit-tidy-trailing-parens ()
  (interactive)
  (save-excursion
    (while (ignore-errors (paredit-forward-up) t))
    (backward-char)
    (live-paredit-delete-horizontal-space)
    (while
        (or
         (eq (char-before) ?\))
         (eq (char-before) ?\})
         (eq (char-before) ?\]))
      (backward-char)
      (live-paredit-delete-horizontal-space))))

(defun live-paredit-reindent-defun (&optional argument)
  "Reindent the definition that the point is on. If the point is
  in a string or a comment, fill the paragraph instead, and with
  a prefix argument, justify as well. Doesn't mess about with
  Clojure fn arglists when filling-paragraph in docstrings.

  Also tidies up trailing parens when in a lisp form"
  (interactive "P")
  (cond ((paredit-in-comment-p) (fill-paragraph argument))
        ((paredit-in-string-p) (progn
                                 (save-excursion
                                   (paredit-forward-up)
                                   (insert "\n"))
                                 (fill-paragraph argument)
                                 (save-excursion
                                   (paredit-forward-up)
                                   (delete-char 1))))
        (t (when (not (live-paredit-top-level-p))
             (progn (save-excursion
                      (end-of-defun)
                      (beginning-of-defun)
                      (indent-sexp))
                    (live-paredit-tidy-trailing-parens))))))


(defun live-paredit-forward-down ()
  "Doesn't freeze Emacs if attempted to be called at end of
   buffer. Otherwise similar to paredit-forward-down."
  (interactive)
  (if (save-excursion
          (forward-comment (buffer-size))
          (not (live-end-of-buffer-p)))
      (paredit-forward-down)
    (error "unexpected end of buffer")))

(defun live-paredit-top-level-p ()
  "Returns true if point is not within a given form i.e. it's in
  toplevel 'whitespace'"
  (not
   (save-excursion
     (ignore-errors
       (paredit-forward-up)
       t))))

(defun live-paredit-copy-sexp-at-point ()
  (interactive)
    (kill-new (thing-at-point 'sexp)))


(define-key paredit-mode-map (kbd "C-c l k") 'paredit-splice-sexp-killing-forward)
(define-key paredit-mode-map (kbd "C-c l w") 'paredit-splice-sexp-killing-backward)
(define-key paredit-mode-map (kbd "C-c l l") 'align-cljlet)
(define-key paredit-mode-map (kbd "C-c l t") 'fill-paragraph)
(define-key paredit-mode-map (kbd "C-c l j") 'live-paredit-forward-slurp-sexp-neatly)
(define-key paredit-mode-map (kbd "C-M-e")   'paredit-backward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-s")   'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-j")   'live-paredit-forward-slurp-sexp-neatly)
(define-key paredit-mode-map (kbd "C-M-y")   'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-z")   'align-cljlet)
(define-key paredit-mode-map (kbd "M-S")     'paredit-split-sexp)
(define-key paredit-mode-map (kbd "M-s")     'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "M-j")     'paredit-join-sexps)
(define-key paredit-mode-map (kbd "M-P")     'live-paredit-previous-top-level-form)
(define-key paredit-mode-map (kbd "M-N")     'live-paredit-next-top-level-form)
(define-key paredit-mode-map (kbd "C-M-f")   'live-paredit-forward)
(define-key paredit-mode-map (kbd "M-q")     'live-paredit-reindent-defun)
(define-key paredit-mode-map (kbd "M-d")     'live-paredit-forward-kill-sexp)
(define-key paredit-mode-map (kbd "M-k")     'live-paredit-backward-kill)
(define-key paredit-mode-map (kbd "M-\\")    'live-paredit-delete-horizontal-space)
(define-key paredit-mode-map (kbd "C-M-i")   'paredit-forward-down)
(define-key paredit-mode-map (kbd "C-M-n")   'paredit-forward-up)
(define-key paredit-mode-map (kbd "C-M-p")   'paredit-backward-down)
(define-key paredit-mode-map (kbd "C-M-u")   'paredit-backward-up)
(define-key paredit-mode-map (kbd "M-T")     'transpose-sexps)
(define-key paredit-mode-map (kbd "C-M-k")   'live-paredit-copy-sexp-at-point)


;;; Snippets

(require 'yasnippet)


;;; Multiple Cursors

(require 'multiple-cursors)


;;; Clojure Mode

(require 'clojure-mode)


;;; Cider (Clojure REPL)

(require 'cider)


;;; Clojure Refactoring

(require 'clj-refactor)

;;; Paredit

(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

(require 'paredit)
(require 'thingatpt)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook    'enable-paredit-mode)

;; Paredit extensions from Emacs Live

(defun live-paredit-next-top-level-form ()
  (interactive)
  (while (ignore-errors (paredit-backward-up) t))
  (live-paredit-forward))

(defun live-paredit-previous-top-level-form ()
  (interactive)
  (if (ignore-errors (paredit-backward-up) t)
      (while (ignore-errors (paredit-backward-up) t))
    (paredit-backward)))

(defun live-paredit-forward ()
  "Feels more natural to move to the beginning of the next item
   in the sexp, not the end of the current one."
  (interactive)
  (if (and (not (paredit-in-string-p))
           (save-excursion
             (ignore-errors
               (forward-sexp)
               (forward-sexp)
               t)))
      (progn
        (forward-sexp)
        (forward-sexp)
        (backward-sexp))
    (paredit-forward)))

(defun live-paredit-forward-slurp-sexp-neatly ()
  (interactive)
  (save-excursion
    (cond ((or (paredit-in-comment-p)
               (paredit-in-char-p))
           (error "Invalid context for slurping S-expressions."))
          ((paredit-in-string-p)
           (paredit-forward-slurp-into-string))
          (t

           (save-excursion
             (paredit-forward-up)
             (paredit-backward-down)
             (paredit-forward-slurp-sexp)
             (just-one-space)))))
  (when (not (save-excursion
               (ignore-errors
                 (backward-sexp)
                 t)))
    (delete-horizontal-space)))

(defun live-paredit-forward-kill-sexp (&optional arg)
  (interactive "p")
  (cond ((or (paredit-in-comment-p)
             (paredit-in-string-p)) (kill-word (or arg 1)))
        (t (kill-sexp (or arg 1)))))

(defun live-paredit-backward-kill-sexp (&optional arg)
  (interactive "p")
  (cond ((or (paredit-in-comment-p)
             (paredit-in-string-p)) (backward-kill-word (or arg 1)))
        (t (backward-kill-sexp (or arg 1)))))

(defun live-paredit-backward-kill ()
  (interactive)
  (let ((m (point-marker)))
    (paredit-backward-up)
    (forward-char)
    (delete-region (point) m)))

(defun live-paredit-delete-horizontal-space ()
  (interactive)
  (just-one-space -1)
  (paredit-backward-delete))

(defun live-paredit-tidy-trailing-parens ()
  (interactive)
  (save-excursion
    (while (ignore-errors (paredit-forward-up) t))
    (backward-char)
    (live-paredit-delete-horizontal-space)
    (while
        (or
         (eq (char-before) ?\))
         (eq (char-before) ?\})
         (eq (char-before) ?\]))
      (backward-char)
      (live-paredit-delete-horizontal-space))))

(defun live-paredit-reindent-defun (&optional argument)
  "Reindent the definition that the point is on. If the point is
  in a string or a comment, fill the paragraph instead, and with
  a prefix argument, justify as well. Doesn't mess about with
  Clojure fn arglists when filling-paragraph in docstrings.

  Also tidies up trailing parens when in a lisp form"
  (interactive "P")
  (cond ((paredit-in-comment-p) (fill-paragraph argument))
        ((paredit-in-string-p) (progn
                                 (save-excursion
                                   (paredit-forward-up)
                                   (insert "\n"))
                                 (fill-paragraph argument)
                                 (save-excursion
                                   (paredit-forward-up)
                                   (delete-char 1))))
        (t (when (not (live-paredit-top-level-p))
             (progn (save-excursion
                      (end-of-defun)
                      (beginning-of-defun)
                      (indent-sexp))
                    (live-paredit-tidy-trailing-parens))))))


(defun live-paredit-forward-down ()
  "Doesn't freeze Emacs if attempted to be called at end of
   buffer. Otherwise similar to paredit-forward-down."
  (interactive)
  (if (save-excursion
          (forward-comment (buffer-size))
          (not (live-end-of-buffer-p)))
      (paredit-forward-down)
    (error "unexpected end of buffer")))

(defun live-paredit-top-level-p ()
  "Returns true if point is not within a given form i.e. it's in
  toplevel 'whitespace'"
  (not
   (save-excursion
     (ignore-errors
       (paredit-forward-up)
       t))))

(defun live-paredit-copy-sexp-at-point ()
  (interactive)
    (kill-new (thing-at-point 'sexp)))


(define-key paredit-mode-map (kbd "C-c l k") 'paredit-splice-sexp-killing-forward)
(define-key paredit-mode-map (kbd "C-c l w") 'paredit-splice-sexp-killing-backward)
(define-key paredit-mode-map (kbd "C-c l l") 'align-cljlet)
(define-key paredit-mode-map (kbd "C-c l t") 'fill-paragraph)
(define-key paredit-mode-map (kbd "C-c l j") 'live-paredit-forward-slurp-sexp-neatly)
(define-key paredit-mode-map (kbd "C-M-e")   'paredit-backward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-s")   'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-j")   'live-paredit-forward-slurp-sexp-neatly)
(define-key paredit-mode-map (kbd "C-M-y")   'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-z")   'align-cljlet)
(define-key paredit-mode-map (kbd "M-S")     'paredit-split-sexp)
(define-key paredit-mode-map (kbd "M-s")     'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "M-j")     'paredit-join-sexps)
(define-key paredit-mode-map (kbd "M-P")     'live-paredit-previous-top-level-form)
(define-key paredit-mode-map (kbd "M-N")     'live-paredit-next-top-level-form)
(define-key paredit-mode-map (kbd "C-M-f")   'live-paredit-forward)
(define-key paredit-mode-map (kbd "M-q")     'live-paredit-reindent-defun)
(define-key paredit-mode-map (kbd "M-d")     'live-paredit-forward-kill-sexp)
(define-key paredit-mode-map (kbd "M-k")     'live-paredit-backward-kill)
(define-key paredit-mode-map (kbd "M-\\")    'live-paredit-delete-horizontal-space)
(define-key paredit-mode-map (kbd "C-M-i")   'paredit-forward-down)
(define-key paredit-mode-map (kbd "C-M-n")   'paredit-forward-up)
(define-key paredit-mode-map (kbd "C-M-p")   'paredit-backward-down)
(define-key paredit-mode-map (kbd "C-M-u")   'paredit-backward-up)
(define-key paredit-mode-map (kbd "M-T")     'transpose-sexps)
(define-key paredit-mode-map (kbd "C-M-k")   'live-paredit-copy-sexp-at-point)


;;; Snippets

(require 'yasnippet)


;;; Multiple Cursors

(require 'multiple-cursors)


;;; Clojure Mode

(require 'clojure-mode)


;;; Cider (Clojure REPL)

(require 'cider)


;;; Clojure Refactoring

(require 'clj-refactor)


;;; Emacs Live lisp conf

(add-to-list 'auto-mode-alist '("\\.el$" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defun live-lisp-describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
   This checks in turn:
     -- for a function name where point is
     -- for a variable name where point is
     -- for a surrounding function call"
          (interactive)
          (let (sym)
            ;; sigh, function-at-point is too clever.  we want only the first half.
            (cond ((setq sym (ignore-errors
                               (with-syntax-table emacs-lisp-mode-syntax-table
                                 (save-excursion
                                   (or (not (zerop (skip-syntax-backward "_w")))
                                       (eq (char-syntax (char-after (point))) ?w)
                                       (eq (char-syntax (char-after (point))) ?_)
                                       (forward-sexp -1))
                                   (skip-chars-forward "`'")
                                   (let ((obj (read (current-buffer))))
                                     (and (symbolp obj) (fboundp obj) obj))))))
                   (describe-function sym))
                  ((setq sym (variable-at-point)) (describe-variable sym)))))


(defun live-lisp-top-level-p ()
  "Returns true if point is not within a given form i.e. it's in
  toplevel 'whitespace'. Only works for lisp modes."
  (= 0 (car (syntax-ppss))))

(defun live-check-lisp-top-level-p ()
  "Returns true if point is not within a given form i.e. it's in
  toplevel 'whitespace'. Only works for lisp modes."
  (interactive)
  (if (live-lisp-top-level-p)
      (message "top level")
    (message "not top level")))

(defun live-whitespace-at-point-p ()
  "Returns true if the char at point is whitespace"
  (string-match "[ \n\t]" (buffer-substring (point) (+ 1 (point)))))
