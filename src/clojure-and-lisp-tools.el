;;; -*- lexical-binding: t; -*-

;;; Paredit

(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

(require 'paredit)
(require 'thingatpt)

(require 'mic-paren)
(paren-activate)

(require 'rainbow-delimiters)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook    'enable-paredit-mode)

(dolist (x '(scheme emacs-lisp lisp clojure cider-repl))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

;;; Clojure Mode

(require 'clojure-mode)

;;; Cider (Clojure REPL)

(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-prompt-save-file-on-load nil)
(setq cider-repl-history-file (expand-file-name "cider-history.clj" bonkydog-tmp-dir))


;;; Clojure Refactoring
(require 'yasnippet)
(require 'multiple-cursors)
(require 'clj-refactor)
