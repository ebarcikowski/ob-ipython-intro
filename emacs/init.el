;; put everything else in sub packages.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/setup")
;; (require 'setup-theme)
(require 'setup-packages)
;; (require 'setup-ivy-counsel)
;; (require 'setup-helm)
;; I don't quite have ggtags figured out, but I'd better before I
;; get back into hard core C++ dev.
;; (require 'setup-helm-gtags)
;; (require 'setup-ggtags)
;; (require 'setup-general)
;; (require 'setup-c)
(require 'setup-python)
;; (require 'setup-cedet)
;; (require 'setup-editing)
;; (require 'setup-ein)
(require 'setup-org)
;; (require 'setup-browser)
;; (require 'setup-matlab)
;; (require 'setup-wp)

;; I don't know what this one is ;)
;; function-args
;; (require 'function-args)
;; (fa-config-default)

;; get custom stuff out of init.el for revisioning
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; A different option, actually disable custom, haven't tried this yet.
;; (setq custom-file (make-temp-file "emacs-custom"))
