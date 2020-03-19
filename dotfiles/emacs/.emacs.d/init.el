(require 'package) ;the first thing to do is to install/update all the packages

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; packages list, change this to add new packages
(setq packages '(spacemacs-theme
		 evil
		 evil-nerd-commenter
		 undo-tree
		 powerline
		 all-the-icons
		 smex
		 company
		 centaur-tabs
		 highlight-indent-guides
		 dashboard
		 magit
		 evil-magit
		 rainbow-delimiters
		 neotree
		 ranger
		 projectile
		 auctex
		 company-auctex
		 company-math
		 elpy
		 flycheck
		 py-autopep8
		 paredit
		 haskell-mode))

;; iter over the packages list and install new packages, doing a refresh before
(setq already-refreshed nil) ; the refresh must be done only one time
(mapcar (lambda (p) (when (not (package-installed-p p))
		      (when (not already-refreshed)
			(package-refresh-contents)
			(setq already-refreshed 't))
		      (package-install p)))
     packages)

;; other config files are inside ~/.emacs.d/conf/, add this path to load-path
(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))
;; keep the custom variables (added by Custom) inside a different conf/custom.el
(load (expand-file-name "conf/custom.el" user-emacs-directory))

;; put backup files inside a specific directory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 100)

(add-hook 'prog-mode-hook #'hs-minor-mode) ; hs-minor-mode is necessary for evil to fold

(require 'appearance)


(require 'server)
(unless (server-running-p)
  (server-start))


(require 'evil)
(evil-mode 1) ; for emacs and vim commands at the same time


(require 'smex) 
(smex-initialize) 
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x is still available


(require 'centaur-tabs)
(centaur-tabs-mode t)
(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
(setq centaur-tabs-cycle-scope 'tabs) ; cycle through visible tabs (that is, the tabs in the current group)

(require 'powerline)
(powerline-center-evil-theme)


(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "TAB") 'company-complete)

(require 'undo-tree)
(global-undo-tree-mode 1)
(global-set-key (kbd "C-r") 'undo-tree-redo)


(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


(require 'projectile)
(projectile-mode +1)


(require 'evil-magit)


(require 'all-the-icons)


(require 'evil-nerd-commenter)
(evilnc-default-hotkeys)


(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        ))
(setq dashboard-set-navigator t)


(require 'paredit)
(mapcar (lambda (mode) (add-hook mode #'enable-paredit-mode))
	'(emacs-lisp-mode-hook
	  eval-expression-minibuffer-setup-hook
	  ielm-mode-hook
	  lisp-mode-hook
	  lisp-interaction-mode-hook
	  scheme-mode-hook
	  ))

(require 'ranger)
(ranger-override-dired-mode t)


;; Python editing configuration
(require 'elpy)
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; LaTeX editing configuration
(defun latex-mode-setup ()
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))

(add-hook 'TeX-mode-hook 'latex-mode-setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exwm vterm spacemacs-theme smex ranger rainbow-delimiters py-autopep8 projectile paredit neotree highlight-indent-guides haskell-mode flycheck evil-nerd-commenter evil-magit elpy dashboard company-math company-auctex centaur-tabs all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
