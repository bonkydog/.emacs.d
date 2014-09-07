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


;;; Load libraries from lib
(add-to-list 'load-path (concat user-emacs-directory "lib/"))

(defun bonkydog-add-lib (p)
  (add-to-list 'load-path (concat user-emacs-directory "lib/" p)))


;;; Start server for emacsclient command.
(server-start)

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


;;; Make sure ansi colour character escapes are honored.
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

(bonkydog-add-lib "packed")
(require 'packed)
(require 'auto-compile)

(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)


