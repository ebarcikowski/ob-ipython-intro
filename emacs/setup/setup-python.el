;; see: http://tkf.github.io/emacs-jedi/latest/


(setq elpy-rpc-backend "jedi")
(elpy-enable)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'autopair-mode)

(add-hook 'python-mode-hook
          (lambda ()
            ;; ('jedi:setup)
            ;; ('autopair-mode)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)))


;; try to only use python3 if I can help it.
(setq python-shell-interpreter "python3")
(setq elpy-rpc-python-command "python3")

;; nice to get jedi to complete when I want it to.
(eval-after-load 'python
  '(define-key python-mode-map (kbd "<C-tab>") 'jedi:complete))

;; trick to get the prompt to work with ipython.  Honestly,
;; I think just 'simple-prompt' is enough.
(when (executable-find "ipython")
  (setq
   ;; this may not be the best.  ipython is pretty sweet but python will
   ;; give you links to follow back to the source file.
   python-shell-interpreter "ipython3"
   python-shell-interpreter-args "--simple-prompt -i"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(add-hook 'inferior-python-mode-hook 'no-trailing-whitespace)
(add-hook 'inferior-python-mode-hook
          '(lambda ()
             (setq-local ml-interactive? t)))

;; Apparently a bug in emacs 25.1.  From
;; https://emacs.stackexchange.com/a/30970
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

;; I don't want python-mode to guess.  I want it to always be 4.

(provide 'setup-python)
