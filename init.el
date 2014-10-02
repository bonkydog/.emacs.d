;;; -*- lexical-binding: t; -*-

;;;; Brian Jenkins (bonkdog)'s Emacs Configuration

;;; "Your father's lightsaber. This is the weapon of a Jedi
;;; Knight. Not as clumsy or random as a blaster. An elegant weapon,
;;; for a more civilized age."
;;;
;;; ―Obi-Wan Kenobi
;;;
;;;
;;; Many thanks to these Emacs Jedi, from whom I have learned (and,
;;; honestly, copied) so much.
;;;
;;; Aaron Bedra
;;; http://www.aaronbedra.com/emacs.d
;;;
;;; Bozhidar Batsov
;;; https://github.com/bbatsov/prelude
;;;
;;; Dimitri Fontaine
;;; https://github.com/dimitri/emacs-kicker
;;;
;;; Magnar Sveen
;;; https://github.com/magnars/.emacs.d
;;;
;;; Phil Hagelberg (technomancy)
;;; https://github.com/technomancy/emacs-starter-kit/tree/master
;;;
;;; Sam Aaron
;;; https://github.com/overtone/emacs-live

;;; Shortcut to edit this file.

(defun ei ()
  (interactive)
  (find-file user-init-file))


;;; Disable unneeded chrome.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq inhibit-splash-screen t
      initial-scratch-message nil)


;;; Set up load path.

(defun bk9y-set-up-load-path () ; This is itempotent.  Feel free to re-run it if you add libraries.
  ;; my code
  (add-to-list 'load-path (concat user-emacs-directory "src/"))

  ;; other peoples' "vendored" code.  (That is, checked into this project.)
  (add-to-list 'load-path (concat user-emacs-directory "vendor/")) ; others' code, checked in.

  ;; other people's code, "submoduled" into this project
  (dolist (dir (directory-files (concat user-emacs-directory "lib/") t))
    (if (and (file-directory-p dir)
	     (not (string-prefix-p "." (file-name-nondirectory dir))))
	(add-to-list 'load-path dir))))

(bk9y-set-up-load-path)


;;; Extend ELisp in useful ways.
(require 'cl)   ; Common Lisp-esqe extensions
(require 'dash) ; Clojure-esque data-structure tools.
(require 's)


;;; Start server for emacsclient command.

(server-start)


;;; Make sure ansi color character escapes are honored.
(require 'ansi-color)
(ansi-color-for-comint-mode-on)

(setq font-lock-maximum-decoration t
      color-theme-is-global t)

;;; Wrap lines at 72
(set-default 'fill-column 72)


;;; Save unsaved file-backed buffers on loss of focus.

(defun save-file-visiting-buffers ()
  (interactive)
  (save-some-buffers t nil))

(add-hook 'focus-out-hook 'save-file-visiting-buffers) 


;;; Revert buffers on gain of focus.

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers 1)


;;; Centralize backup and autosave files.

(setq bonkydog-backups-dir (concat user-emacs-directory "backups/")
      bonkydog-autosaves-dir (concat user-emacs-directory "autosaves/"))

(require 'backup-dir)

(setq file-precious-flag t)

(make-variable-buffer-local 'backup-inhibited)
(setq bkup-backup-directory-info
      `((t ,bonkydog-backups-dir ok-create full-path prepend-name)))

(setq auto-save-file-name-transforms `((".*" ,(concat bonkydog-autosaves-dir "\\1") t)))
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,bonkydog-backups-dir)))
(setq auto-save-list-file-name (concat bonkydog-autosaves-dir "autosave-list"))

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)


;;; Auto-recompile ELisp

(require 'packed)
(require 'auto-compile)

(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


;;; Dired configuration

(require 'dircolors)


;;; Navigate ELisp source without a tags file

(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)


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
