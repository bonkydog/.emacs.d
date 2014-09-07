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

(setq bonkydog-backups-dir (concat user-emacs-directory "backups/")
      bonkydog-autosaves-dir (concat user-emacs-directory "autosaves/"))
  
;;; Disable unneeded chrome.

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-splash-screen t
      initial-scratch-message nil)


;;; Save unsaved file-backed buffers on loss of focus.

(defun save-file-visiting-buffers ()
  (interactive)
  (save-some-buffers t nil))

(add-hook 'focus-out-hook 'save-file-visiting-buffers) 


;;; Revert buffers on gain of focus.

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers 1)


;;; Centralize backup and autosave files.

(add-to-list 'load-path (concat user-emacs-directory "lib/"))
(load-file (concat user-emacs-directory "config/backup-dir-conf.el"))


