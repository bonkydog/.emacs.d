;;; -*- lexical-binding: t; -*-

;;; Paredit

(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

(use-package paredit)
(use-package thingatpt)

(use-package mic-paren)
(paren-activate)

(use-package rainbow-delimiters)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook    'enable-paredit-mode)

(dolist (x '(scheme emacs-lisp lisp clojure cider-repl))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

;;; Clojure Mode

(use-package clojure-mode)

;;; Cider (Clojure REPL)

(use-package cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-history-file (expand-file-name "cider-history.clj" bonkydog-tmp-dir))


;;; Clojure Refactoring
(use-package yasnippet)
(use-package multiple-cursors)
(use-package clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")))

(define-key clojure-mode-map (kbd "C-:") 'cljr-cycle-stringlike)
(define-key clojure-mode-map (kbd "C->") 'cljr-cycle-coll)
