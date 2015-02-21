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
;;; Dmitry Gutov
;;; https://github.com/dgutov/dot-emacs
;;;
;;; Daniel Higginbotham
;;; https://github.com/flyingmachine/emacs.d
;;;
;;; Magnar Sveen
;;; https://github.com/magnars/.emacs.d
;;;
;;; Phil Hagelberg (technomancy)
;;; https://github.com/technomancy/emacs-starter-kit/tree/master
;;;
;;; Sam Aaron
;;; https://github.com/overtone/emacs-live
;;;
;;; Sebastian Wiesner
;;; https://github.com/lunaryorn/.emacs.d
;;;
;;; Johan Anderson
;;; https://github.com/rejeep/emacs
;;;
;;; John Wiegley
;;; https://github.com/jwiegley/dot-emacs
;;;

(setq message-log-max 16384)

(defconst emacs-start-time (current-time))

;;; Disable unneeded chrome as early as possible.

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;;; Shortcut to edit this file.

(defun ei ()
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "s-i") 'ei)

;;; Set up directory variables

(defconst bonkydog-root-dir user-emacs-directory)
(defconst bonkydog-backups-dir (expand-file-name  "backups" bonkydog-root-dir))
(defconst bonkydog-autosaves-dir (expand-file-name "autosaves" bonkydog-root-dir))
(defconst bonkydog-tmp-dir (file-name-as-directory (expand-file-name "tmp" bonkydog-root-dir)))


;;; Set up load path

(defun bonkydog-set-up-load-path () ; This is itempotent.  Feel free to re-run it if you add libraries.
  (interactive)
  ;; my code
  (add-to-list 'load-path (expand-file-name "src" bonkydog-root-dir))

  ;; other peoples' "vendored" code.  (That is, checked into this project.)
  (add-to-list 'load-path (expand-file-name "vendor" bonkydog-root-dir)) ; others' code, checked in.

  ;; other people's code, "submoduled" into this project
  (dolist (dir (directory-files (expand-file-name "lib" bonkydog-root-dir) t))
    (if (and (file-directory-p dir)
             (not (string-prefix-p "." (file-name-nondirectory dir))))
        (add-to-list 'load-path dir))))

(bonkydog-set-up-load-path)


;;; Set up use-package

(require 'use-package)
(setq use-package-verbose t)


;; Set up 'custom' system

(setq custom-file (expand-file-name "customizations.el" bonkydog-root-dir ))
(if (file-exists-p custom-file)
    (load custom-file))


(use-package solarized
  :init
  (progn
    (load-theme 'solarized-light)
    
    ;; make the fringe stand out from the background
    (setq solarized-distinct-fringe-background t)

    ;; make the modeline high contrast
    (setq solarized-high-contrast-mode-line t)))


(use-package key-chord
  :init
  (key-chord-mode t))

;;; Extend ELisp in useful ways.
(use-package cl-lib)   ; Common Lisp-esqe extensions
(use-package dash)     ; Clojure-esque data-structure tools
(use-package s)        ; string handling tools

;;; Auto-recompile ELisp

(use-package packed)
(use-package auto-compile
  :init
  (progn
    (auto-compile-on-load-mode 1)
    (auto-compile-on-save-mode 1)))


;;; Navigate ELisp source without a tags file

(use-package elisp-slime-nav
  :init
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))


;;; Start server for emacsclient command.

(server-start)


;;; Make sure ansi color character escapes are honored.
(use-package ansi-color
  :init
  (ansi-color-for-comint-mode-on))

(setq font-lock-maximum-decoration t
      color-theme-is-global t)


;;; Wrap lines at 72
(set-default 'fill-column 72)



;;; Centralize backup and autosave files.

(use-package backup-dir
  :init
  (progn

    (setq file-precious-flag t)

    (make-variable-buffer-local 'backup-inhibited)
    (setq bkup-backup-directory-info
          `((t ,bonkydog-backups-dir ok-create full-path prepend-name)))

    (setq auto-save-file-name-transforms `((".*" ,(expand-file-name  "\\1" bonkydog-autosaves-dir) t)))
    (setq backup-by-copying t)
    (setq backup-directory-alist `((".*" . ,bonkydog-backups-dir)))
    (setq auto-save-list-file-name (expand-file-name "autosave-list" bonkydog-autosaves-dir))

    (setq delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t)))





;;; Dired configuration

(use-package dircolors
  :init
  (progn
    ;; Mac's ls doesn't support the --dired flag.  Fall back to emacs's ls emulation.
    (when (eq system-type 'darwin)
      (use-package ls-lisp)
      (setq ls-lisp-use-insert-directory-program nil))))


(use-package paredit)
(use-package smartparens
  :disabled t
  :init
  (progn
    (require 'smartparens-config)
    (setq sp-base-key-bindings 'paredit)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings)
    (show-smartparens-global-mode +1)
    (smartparens-strict-mode t)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode)

(dolist (x '(scheme emacs-lisp lisp clojure cider-repl))
   ;(add-hook (intern (concat (symbol-name x) "-mode-hook")) 'smartparens-mode)
   (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'paredit-mode)
   (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))


(use-package thingatpt)

(use-package mic-paren)
(paren-activate)



(use-package yasnippet)
(use-package multiple-cursors)

;;; Clojure Mode

(use-package clojure-mode
  :commands clojure-mode
  :mode "\\.clj\\(s\\)\\'"
  :interpreter "clojure"
  :config
  (progn
    (use-package slamhound
      :config
      (progn
        (defun save-all-and-slamhound ()
          (interactive)
          (save-all-quietly)
          (slamhound)
          (save-all-quietly))
        
        (define-key clojure-mode-map (kbd "C-c l s") 'save-all-and-slamhound))))
  (use-package clj-refactor
    :config
    (add-hook 'clojure-mode-hook (lambda ()
                                   (clj-refactor-mode 1)
                                   (cljr-add-keybindings-with-prefix "s-r")))

    (define-key clojure-mode-map (kbd "C-:") 'clojure-toggle-keyword-string)
    (define-key clojure-mode-map (kbd "C->") 'cljr-cycle-coll)))

;;; Cider (Clojure REPL)

(use-package cider
  :commands (cider-connect cider-jack-in)
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (setq nrepl-hide-special-buffers t)
    (setq cider-prompt-save-file-on-load nil)
    (setq cider-repl-history-file (expand-file-name "cider-history.clj" bonkydog-tmp-dir))
    (defun clojure-test-clear-and-run-tests ()
  (interactive)
  (clojure-test-run-tests)
  (set-window-start (get-buffer-window (cider-current-repl-buffer) t) 0))

    (global-set-key (kbd "<f11>") 'clojure-test-clear-and-run-tests)

    (setq cider-prompt-save-file-on-load nil)

    (defun save-clojure-buffers ()
      (save-some-buffers t (lambda () (equal major-mode 'clojure-mode))))

    ;; If there’s an error compiling during a reset, sometimes user.clj doesn’t
    ;; get re-loaded.  Then we’re stranded.  So we manually reload dev.clj
    ;; every time before resetting.  Make sure any state vars in dev.clj are
    ;; defined using defonce to avoid losing state (e.g. the webserver).

    (defun reload-dev ()
      (interactive)
      (let ((dev-user-file-name (expand-file-name "src/cljs/dev.clj" (projectile-project-root))))
        (when (file-exists-p dev-user-file-name)
          (message "reloading dev.cljs")
          (cider-interactive-eval dev-user-file-name)
          t)))

    (defun clojure-test-clear-and-run-dev-t ()
      (interactive)
      (save-clojure-buffers)
      (cider-test-clear-highlights)
      (reload-dev)
      (cider-interactive-eval "(dev/t)"))

    (global-set-key (kbd "<f12>") 'clojure-test-clear-and-run-dev-t)

    (defun cider-refresh ()
      (interactive)
      (save-clojure-buffers)
      (reload-dev)
      (cider-interactive-eval "(dev/reset)"))

    (global-set-key (kbd "s-<return>") 'cider-refresh)


    (defun cider-insert-and-run-defun-in-repl ()
      (interactive)
      "Insert FORM in the REPL buffer."
      (let ((start-pos (point))
            (form (cider-defun-at-point)))
        (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
          (setq form (replace-match "" t t form)))
        (with-current-buffer (cider-current-repl-buffer)
          (end-of-buffer)
          (insert form)
          (indent-region start-pos (point))
          (cider-repl-return))))

    (define-key cider-mode-map
      (kbd "s-.")
      'cider-insert-and-run-defun-in-repl)))


;;; Fuzzy matching

(use-package flx)

;;; Ido mode

(use-package ido
  :init
  (progn
    (ido-mode t))
  (setq ido-enable-prefix nil
        ido-create-new-buffer 'always
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-everywhere 1)

  (icomplete-mode 1)

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (autoload 'ibuffer "ibuffer" "List buffers." t))

(use-package idomenu)
(use-package flx-ido
  :init
  (flx-ido-mode t))




(defvar live-symbol-names)
(defvar live-name-and-pos)

(defun live-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun live-ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (use-package imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              live-name-and-pos live-symbol-names position selected-symbol)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (live-ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " live-symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol live-name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (live-ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'live-symbol-names name)
              (add-to-list 'live-name-and-pos (cons name position))))))))

;;; Smart Meta-X

(use-package smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;; Company Mode
(use-package company)
(global-company-mode)

(use-package dired-x
  :init
  (progn
    (setq dired-omit-verbose nil)
    (add-hook 'dired-load-hook
              (lambda () (load "dired-x")))))

;;; Customize for MacOS

(when (eq system-type 'darwin)
  
  ;; Make cut and paste work with the OS X clipboard

  (defun live-copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun live-paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

  (when (not window-system)
    (setq interprogram-cut-function 'live-paste-to-osx)
    (setq interprogram-paste-function 'live-copy-from-osx))

  ;; Work around a bug on OS X where system-name is a fully qualified
  ;; domain name
  (setq system-name (car (split-string system-name "\\.")))

  ;; Ensure the exec-path honours the shell PATH
  (use-package exec-path-from-shell)
  (exec-path-from-shell-initialize)

  ;; Ignore .DS_Store files with ido mode
  (use-package ido)
  (add-to-list 'ido-ignore-files "\\.DS_Store")

  (setq dired-omit-mode t)
  (setq-default dired-omit-files-p t) ; this is buffer-local variable
  (setq dired-omit-files "\\.DS_Store")

  (setq default-input-method "MacOSX"))


;; Configure Emacs built-ins

;;; (Largely stolen from Emacs Live.)

;;use file path to ensure buffer name uniqueness
(use-package uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;store history of recently opened files
(use-package recentf)
(setq recentf-save-file (concat bonkydog-tmp-dir "recentf")
      recentf-max-saved-items 200)
(recentf-mode t)

;;When you visit a file, point goes to the last place where it was
;;when you previously visited. Save file is set to bonkydog-tmp-dir/places
(use-package saveplace)
(setq-default save-place t)
(setq save-place-file (concat bonkydog-tmp-dir "places"))

;;enable cua-mode for rectangular selections
(use-package cua-base)
(use-package cua-gmrk)
(use-package cua-rect)
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;;enable winner mode for C-c-(<left>|<right>) to navigate the history
;;of buffer changes i.e. undo a split screen
(when (fboundp 'winner-mode)
      (winner-mode 1))

(setq initial-major-mode 'lisp-interaction-mode
      redisplay-dont-pause t
      column-number-mode t
      echo-keystrokes 0.02
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      require-final-newline t
      truncate-partial-width-windows nil
      delete-by-moving-to-trash nil
      confirm-nonexistent-file-or-buffer nil
      query-replace-highlight t
      next-error-highlight t
      next-error-highlight-no-select t)

;;set all coding systems to utf-8
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

(set-default 'indent-tabs-mode nil)
(auto-compression-mode t)
(show-paren-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;;default to unified diffs
(setq diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;;remove all trailing whitespace and trailing blank lines before
;;saving the file
(defvar live-ignore-whitespace-modes '(markdown-mode))
(defun live-cleanup-whitespace ()
  (if (not (member major-mode live-ignore-whitespace-modes))
      (let ((whitespace-style '(trailing empty)) )
        (whitespace-cleanup))))

;; (add-hook 'before-save-hook 'live-cleanup-whitespace)

;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat bonkydog-tmp-dir "savehist"))
(savehist-mode t)


;; bookmarks
(use-package bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" bonkydog-tmp-dir)
      bookmark-save-flag 1)

;;disable backups of files edited with tramp
(add-to-list 'bkup-backup-directory-info
             (list tramp-file-name-regexp ""))
(setq tramp-bkup-backup-directory-info  nil)

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(use-package recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.

;; enable recent files mode.
(recentf-mode t)

                                        ; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
        (message "Aborting")))



(use-package win-switch
  :init
  (progn
    (setq win-switch-feedback-background-color "DeepPink3")
    (setq win-switch-feedback-foreground-color "black")
    (setq win-switch-window-threshold 1)
    (setq win-switch-idle-time 0.7)

    ;; disable majority of shortcuts
    (win-switch-set-keys '() 'up)
    (win-switch-set-keys '() 'down)
    (win-switch-set-keys '() 'left)
    (win-switch-set-keys '() 'right)
    (win-switch-set-keys '("o") 'next-window)
    (win-switch-set-keys '("p") 'previous-window)
    (win-switch-set-keys '() 'enlarge-vertically)
    (win-switch-set-keys '() 'shrink-vertically)
    (win-switch-set-keys '() 'shrink-horizontally)
    (win-switch-set-keys '() 'enlarge-horizontally)
    (win-switch-set-keys '() 'other-frame)
    (win-switch-set-keys '("C-g") 'exit)
    (win-switch-set-keys '() 'split-horizontally)
    (win-switch-set-keys '() 'split-vertically)
    (win-switch-set-keys '() 'delete-window)
    (win-switch-set-keys '("\M-\C-g") 'emergency-exit)))

;;; Projectile
(use-package epl)
(use-package pkg-info)
(use-package projectile
  :init
  (progn
    (projectile-global-mode)
    (global-set-key (kbd "s-N") 'projectile-find-file)
    (global-set-key (kbd "s-t") 'projectile-find-file)
    (global-set-key (kbd "s-F") 'ag-project)))

;;; Save unsaved file-backed buffers on loss of focus.
(use-package files)

(defun mute (f)
  (flet ((message (&rest ignored) nil))
    (eval f)))

(defun save-file-visiting-buffers ()
  (interactive)
  (mute '(save-some-buffers t nil)))

(add-hook 'focus-out-hook 'save-file-visiting-buffers)


;;; Revert buffers on gain of focus.

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers 1)

;; Load bindings config
(global-set-key (kbd "s-j") '(lambda () (interactive) (join-line t)))


;; set up sticky windows.  use C-x 9 to stick a window in place.

(use-package sticky-windows
  :bind
  (((kbd "C-x 0") . sticky-window-delete-window)
   ((kbd "C-x 1") . sticky-window-delete-other-windows)
   ((kbd "C-x 9") . sticky-window-keep-window-visible)))

;; use mark to jump around without transient mark mode making you crazy
;;(use-package mark-fix)


;; ;; git-gutter
;; (live-add-pack-lib "fringe-helper")
;; (use-package fringe-helper)

;; (live-add-pack-lib "git-gutter-fringe")
;; (use-package git-gutter-fringe)

;; (setq git-gutter-fr:side 'right-fringe)
;; (global-git-gutter-mode)

(defun cdt ()
  "change directory to tektite project"
  (interactive)
  (let ((path "~/workspace/clojure_lab/tektite-app"))
    (cd path)
    (dired path)))

(defun cdd ()
  "change directory to datomizer project"
  (interactive)
  (let ((path "~/workspace/goodguide/datomizer"))
    (cd path)
    (dired path)))

(defun cds ()
  "change directory to stack-spike project"
  (interactive)
  (let ((path "~/workspace/clojure_lab/stack_spike"))
    (cd path)
    (dired path)))


(use-package win-switch
  :bind
  (((kbd "s-}") . win-switch-next-window)
   ((kbd "s-{") . win-switch-previous-window)))


(windmove-default-keybindings 'super)

(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist `(("." . ,(expand-file-name  "undo" bonkydog-root-dir))))
    (global-set-key (kbd "s-_") 'undo-tree-visualize)))

(defun mark-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(use-package dired
  :init
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file)) ; not sure why this is mouse-2, but whatever.

; don't popwin my shell, dude.
;; (setq popwin:special-display-config
;;       (remove-if (lambda (x) (equal "*shell*" (car x)))
;;                  popwin:special-display-config))




(defun configure-text-minor-modes ()
  (auto-fill-mode 1)
  (auto-complete-mode -1)
  (company-mode -1))

(add-hook 'text-mode-hook 'configure-text-minor-modes)

(defun increase-left-margin-by-one (from to)
  (interactive "*r")
  (increase-left-margin from to 1))

(defun decrease-left-margin-by-one (from to)
  (interactive "*r")
  (decrease-left-margin from to 1))

(global-set-key (kbd "s-[") 'decrease-left-margin-by-one)
(global-set-key (kbd "s-]") 'increase-left-margin-by-one)

(defun toggle-comment-for-line-or-region (arg)
  "Make Emacs Alt-/ behave more like RubyMine: toggle comment on region or
toggle comment on line (and then move down to next line)."
  (interactive "*P")
  (let ((initial-mark-state (and mark-active transient-mark-mode)))
    (unless initial-mark-state
      (mark-line))
    (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (unless initial-mark-state
      (next-line))
    (font-lock-fontify-buffer)))

(global-set-key (kbd "M-/") 'toggle-comment-for-line-or-region)

(global-set-key (kbd "s-w") 'sticky-window-delete-window)

;; (global-set-key (kbd "s-D") 'dash-at-point)

(global-set-key (kbd "s-M-<down>") 'next-error)
(global-set-key (kbd "s-M-<up>") 'previous-error)

(use-package ag
  :commands (ag projectile-ag))

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(use-package popwin
  :init
  (progn
    (setq display-buffer-function 'popwin:display-buffer)

    (setq popwin:special-display-config
          '(("*Help*"  :height 30)
            ("*Completions*" :noselect t)
            ("*Messages*" :noselect t :height 30)
            ("*Apropos*" :noselect t :height 30)
            ("*compilation*" :noselect t)
            ("*Backtrace*" :height 30)
            ("*Messages*" :height 30)
            ("*Occur*" :noselect t)
            ("*Ido Completions*" :noselect t :height 30)
            ("*magit-commit*" :noselect t :height 40 :width 80 :stick t)
            ("*magit-diff*" :noselect t :height 40 :width 80)
            ("*magit-edit-log*" :noselect t :height 15 :width 80)
            ("\\*ansi-term\\*.*" :regexp t :height 30)
            ("*shell*" :height 30)
            (".*overtone.log" :regexp t :height 30)
            ("*gists*" :height 30)
            ("*sldb.*":regexp t :height 30)
            ("*cider-error*" :height 30 :stick t)
            ("*cider-doc*" :height 30 :stick t)
            ("*cider-src*" :height 30 :stick t)
            ("*cider-result*" :height 30 :stick t)
            ("*cider-macroexpansion*" :height 30 :stick t)
            ("*Kill Ring*" :height 30)
            ("*Compile-Log*" :height 30 :stick t)
            ("*git-gutter:diff*" :height 30 :stick t)))))



(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))


(use-package ace-jump-mode
  :init
  (key-chord-define-global "jk" 'ace-jump-mode)
  :bind
  (((kbd "C-o") . ace-jump-mode)
   ((kbd "M-o") . ace-jump-mode-pop-mark))
  :config
  (progn
    (message "configging ace-jump-mode!")
    (ace-jump-mode-enable-mark-sync)))



(global-set-key (kbd "C-c m") 'mc/mark-all-like-this-dwim)

(global-set-key (kbd "C-l") 'goto-line)


;; Magit

(use-package magit
  :commands magit-status)

;; easy-kill
(use-package easy-kill
  :init
  (progn
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark)))

(use-package nlinum
  :init
  (global-nlinum-mode t))

;; iy-goto-char
(use-package iy-go-to-char
  :bind (((kbd "M-m") . iy-go-up-to-char)
         ((kbd "M-M") . iy-go-to-char-backward)
         ((kbd "C-c ;") . iy-go-to-or-up-to-continue)
         ((kbd "C-c ,") . iy-go-to-or-up-to-continue-backward))
  :config (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))



;;; Save buffers, windows and frames

(use-package desktop   
  :init (desktop-save-mode)
  :config (progn
            (setq desktop-dirname (expand-file-name "desktop" bonkydog-root-dir))

            ;; Don't autosave desktops, it's too expensive.  Desktops aren't
            ;; that precious, and Emacs will save the desktop on exit anyway.
            (setq desktop-auto-save-timeout nil)

            (dolist (mode '(magit-mode git-commit-mode))
              (add-to-list 'desktop-modes-not-to-save mode))))






;;; Misc Keybindings

;;; Disable arrow keys for great awesome.

(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))

;; Use shell-like backspace C-h, rebind help to F1

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)



;;; Send message about load time
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;;; speed up find-dired
(require 'find-dired)
(setq find-program "find -E")
(setq find-ls-option '( "-print0 | xargs -0 ls -dilsb" . "-dilsb"))


(defun bonkydog-dired-project-files ()
  (interactive)
  (let ((default-directory (projectile-project-root))
        (find-program "git ls-files -c -o --exclude-standard -z")
        (find-ls-option '( " | xargs -0 ls -dislb" . "-dilsb")))
    (find-dired "." "")))

(global-set-key (kbd "s-D") 'bonkydog-dired-project-files)

