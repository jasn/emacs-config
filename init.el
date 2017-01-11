(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(require 'fill-column-indicator)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(setq python-shell-virtualenv-root "/home/jasn/venv/")


(defun basic-setup ()
  (setq show-trailing-whitespace t)
  (show-paren-mode)
  (column-number-mode))

(add-hook 'python-mode-hook
	  (lambda ()
	    (basic-setup)
	    (setq fci-rule-width 3)
	    (setq fci-rule-color "red")
	    (setq fci-rule-column 79)))

