(require 'org)
(require 'babel)
(require 'ob-ipython)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (python . t)
   ;; other languages..
   )
 )

;; don't prompt me to confirm everytime I want to evaluate a block
(setq org-confirm-babel-evaluate nil)
;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(add-hook 'org-mode-hook 'yas-minor-mode)

(setq-default
 org-return-follows-link t
 org-image-actual-width '(800)
 org-highlight-latex-and-related '(latex script entities))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)

;; Allow highlighting in _src sections.
(setq org-src-fontify-natively t)

;; For now, keep all our org files in here.
(setq org-agenda-files '("/home/elliottb/org"))

(provide 'setup-org)
