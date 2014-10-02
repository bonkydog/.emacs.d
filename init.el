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
