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


;;; Set up directory variables

(setq bonkydog-root-dir user-emacs-directory
      bonkydog-backups-dir (expand-file-name  "backups" bonkydog-root-dir)
      bonkydog-autosaves-dir (expand-file-name "autosaves" bonkydog-root-dir)
      bonkydog-tmp-dir (file-name-as-directory (expand-file-name "tmp" bonkydog-root-dir)))


;;; Set up load path

(defun bonkydog-set-up-load-path () ; This is itempotent.  Feel free to re-run it if you add libraries.
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

;; Set up 'custom' system
(setq custom-file (expand-file-name "customizations.el" bonkydog-root-dir ))
(if (file-exists-p custom-file)
  (load custom-file))


;;; Extend ELisp in useful ways.
(require 'cl-lib)   ; Common Lisp-esqe extensions
(require 'dash)     ; Clojure-esque data-structure tools
(require 's)        ; string handling tools


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

(require 'backup-dir)

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
  version-control t)


;;; Auto-recompile ELisp

(require 'packed)
(require 'auto-compile)

(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


;;; Navigate ELisp source without a tags file

(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)


;;; Dired configuration

(require 'dircolors)

;; Mac's ls doesn't support the --dired flag.  Fall back to emacs's ls emulation.
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))


(load "clojure-and-lisp-tools")

;;; Customize for MacOS

(when (eq system-type 'darwin)
  (load "macos-customizations")
  (setq default-input-method "MacOSX"))


;; Configure Emacs built-ins

(load "configure-built-ins")
(load "recentf-conf")
(require 'win-switch)
(load "win-switch-conf")
(load "window-number-conf")


;;;;=======================================================

;; save all files on loss of focus

(defun mute (f)
  (flet ((message (&rest ignored) nil))
    (funcall f)))

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(defun save-all-quietly ()
  (interactive)
  (mute 'save-all))

(add-hook 'focus-out-hook 'save-all-quietly)

;; revert all buffers on regaining focus
;; (a little scary -- use at your own risk!)
;; (from http://www.emacswiki.org/emacs/RevertBuffer)
;; possibly unnecessary with global autorevert mode
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun revert-all-buffers-quietly ()
  (interactive)
  (mute 'revert-all-buffers))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers 1)

;; configure find-file-in-project
;;(dolist (x '("*.coffee" "*.sass" "*.clj" "*.cljs")) (add-to-list 'ffip-patterns x :append))

;; Load bindings config
(load "bindings")

;; load white-background color theme
;;(load "gandalf")
;;(color-theme-gandalf)

;; set up sticky windows.  use C-x 9 to stick a window in place.

(require 'sticky-windows)
(global-set-key     [(control x) (?0)]        'sticky-window-delete-window)
(global-set-key     [(control x) (?1)]        'sticky-window-delete-other-windows)
(global-set-key     [(control x) (?9)]        'sticky-window-keep-window-visible)

;; use mark to jump around without transient mark mode making you crazy
;;(require 'mark-fix)

(global-linum-mode t)


;; ;; git-gutter
;; (live-add-pack-lib "fringe-helper")
;; (require 'fringe-helper)

;; (live-add-pack-lib "git-gutter-fringe")
;; (require 'git-gutter-fringe)

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

(global-set-key (kbd "s-i") 'ei)

(require 'win-switch)
(global-set-key (kbd "s-}") 'win-switch-next-window)
(global-set-key (kbd "s-{") 'win-switch-previous-window)


;; (defun clojure-test-clear-and-run-test ()
;;   (interactive)
;;   (with-current-buffer (cider-current-repl-buffer)
;;     (cider-clear-buffer))
;;   (clojure-test-run-test)
;;   (set-window-start (get-buffer-window (cider-current-repl-buffer) t) 0))

;; (global-set-key (kbd "<f10>") 'clojure-test-clear-and-run-test)

(defun clojure-test-clear-and-run-tests ()
  (interactive)
  (clojure-test-run-tests)
  (set-window-start (get-buffer-window (cider-current-repl-buffer) t) 0))

(global-set-key (kbd "<f11>") 'clojure-test-clear-and-run-tests)

(setq cider-prompt-save-file-on-load nil)

(defun save-clojure-buffers ()
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode))))

(defun clojure-test-clear-and-run-user-t ()
  (interactive)
  (save-clojure-buffers)
  (clojure-test-clear)
  (cider-interactive-eval "(user/t)"))

(global-set-key (kbd "<f12>") 'clojure-test-clear-and-run-user-t)


(defun cider-refresh ()
  (interactive)
  (save-clojure-buffers)
  (cider-interactive-eval "(user/reset)"))

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
  'cider-insert-and-run-defun-in-repl)


(windmove-default-keybindings 'super)

(global-set-key (kbd "s-_") 'undo-tree-visualize)


(defun mark-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(global-set-key (kbd "C-l") 'mark-line)
(global-set-key (kbd "C-t") 'find-file-in-project)


(require 'dired)
(define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-file) ; not sure why this is mouse-2, but whatever.

; don't popwin my shell, dude.
;; (setq popwin:special-display-config
;;       (remove-if (lambda (x) (equal "*shell*" (car x)))
;;                  popwin:special-display-config))



(require 'slamhound)
(defun save-all-and-slamhound ()
  (interactive)
  (save-all-quietly)
  (slamhound)
  (save-all-quietly))
(define-key clojure-mode-map (kbd "C-c l s") 'save-all-and-slamhound)

(defun configure-text-minor-modes ()
  (auto-fill-mode 1)
  (auto-complete-mode -1))

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

(require 'ag)

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "s-r") 'isearch-backward)

(cljr-add-keybindings-with-prefix "s-R")



;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)

;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)

(require 'solarized)
(load-theme 'solarized-light)


(require 'popwin)
