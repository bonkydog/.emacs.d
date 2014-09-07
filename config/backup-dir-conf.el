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

(provide 'backup-dir-conf)
