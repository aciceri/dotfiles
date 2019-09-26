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
(setq packages '(evil
		 powerline 
		 company
		 auctex
		 company-auctex
		 company-math))

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


(require 'appearance)


(require 'server)
(unless (server-running-p)
  (server-start))


(require 'evil)
(evil-mode 1) ; for emacs and vim commands at the same time


(require 'powerline)
(powerline-center-evil-theme)


(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "TAB") 'company-complete)

(defun latex-mode-setup ()
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))

(add-hook 'TeX-mode-hook 'latex-mode-setup)
