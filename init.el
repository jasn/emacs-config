(require 'cl)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(let* ((packages
	'(auto-compile
	  fill-column-indicator
	  idle-require
	  helm
	  jedi
	  magit
	  markdown-mode
	  monokai-theme
	  zenburn-theme))
       (packages (remove-if 'package-installed-p packages)))
  (when packages
    (ignore-errors (package-refresh-contents)
		   (mapcar 'package-install packages))))

(require 'idle-require)

(dolist (feature
	 '(auto-compile
	   jedi))
  (idle-require feature))

(setq idle-require-idle-delay 5)
(idle-require-mode 1)

(setq custom-file (make-temp-file "") ;; Discard customizations.
      recentf-max-saved-items 100
      ring-bell-function 'ignore)


(setq-default fill-column 79
	      truncate-lines t
	      indent-tabs-mode nil
	      show-trailing-whitespace t)

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

(set-language-environment "UTF-8")

;; Disable things taking up screen space.
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)


;; Enable useful information minor modes.
(column-number-mode 1)
(show-paren-mode 1)
(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))


;; Python stuff
(require 'fill-column-indicator)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(setq python-shell-virtualenv-root "/home/jasn/venv/")

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq fci-rule-width 3)
	    (setq fci-rule-color "red")
	    (setq fci-rule-column 79)))

;; visual

(load-theme 'monokai t)
(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

;; Helm
(require 'helm)
(require 'helm-config)

(setq helm-split-window-in-side-p t
      helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-move-to-line-cycle-in-source t
      projectile-completion-system 'helm)

(helm-mode 1)

(define-key custom-bindings-map (kbd "C-c h")   'helm-command-prefix)
(define-key custom-bindings-map (kbd "M-x")     'helm-M-x)
(define-key custom-bindings-map (kbd "M-y")     'helm-show-kill-ring)
(define-key custom-bindings-map (kbd "C-x b")   'helm-mini)
(define-key custom-bindings-map (kbd "C-x C-f") 'helm-find-files)
(define-key custom-bindings-map (kbd "C-c h o") 'helm-occur)
(define-key custom-bindings-map (kbd "C-c h g") 'helm-google-suggest)
(define-key custom-bindings-map (kbd "M-i")     'helm-swoop)
(define-key custom-bindings-map (kbd "M-I")     'helm-multi-swoop-all)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")   'helm-select-action)

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)
