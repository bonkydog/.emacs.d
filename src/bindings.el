;; Place your bindings here.

;; For example:
;;(define-key global-map (kbd "C-+") 'text-scale-increase)
;;(define-key global-map (kbd "C--") 'text-scale-decrease)


(global-set-key (kbd "s-j") '(lambda () (interactive) (join-line t)))
(global-set-key (kbd "s-N") 'find-file-in-project)
