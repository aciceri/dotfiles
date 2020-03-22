(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq auto-save-file-name-transforms
  `((".*" "~/.emacs-saves/" t)))

(use-package darcula-theme)
(use-package exwm)
(require 'exwm-startup)
(exwm-startup)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package org-evil)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package vterm
  :hook (vterm-mode ))

