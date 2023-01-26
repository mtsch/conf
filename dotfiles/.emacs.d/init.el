(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq user-full-name "Matija Čufar"
      user-mail-address "matijacufar@gmail.com")

(require 'package)
(setq package-archives '(
  ("org" . "https://orgmode.org/elpa/")
  ("gnu" . "https://elpa.gnu.org/packages/")
  ("melpa" . "https://melpa.org/packages/")
))
(package-initialize)
(package-refresh-contents)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq backup-directory-alist '(("." . "~/.cache/emacs")))

;  (if (daemonp)
;      (add-hook 'after-make-frame-functions
;                (lambda (frame) (setq doom-modeline-icon t))))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(show-paren-mode t)
(global-hl-line-mode t)

(setq default-tab-width 4)
(setq c-basic-offset 4)
(setq c-indent-level 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline 't)

(setq mouse-autoselect-window t)
(setq focus-follows-mouse t)

(use-package all-the-icons)
(use-package doom-modeline
  :after all-the-icons
  :init
  (doom-modeline-mode 1)
  (column-number-mode 1)
  :config
  (setq doom-modeline-icon nil))

(use-package solarized-theme
  :init
  (setq solarized-scale-org-headlines nil)
  (setq solarized-distinct-fringe-background t)
  :config
  (load-theme 'solarized-light t))

(use-package heaven-and-hell
  :init
  (setq heaven-and-hell-themes
        '((light . solarized-light)
          (dark . solarized-dark)))
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))

(set-face-attribute 'default nil
                    :family "Hack Nerd Font Mono"
                    :height 120
                    :weight 'medium
                    :width 'normal)
(set-face-attribute 'fixed-pitch nil
                    :family "Hack Nerd Font Mono"
                    :height 120
                    :weight 'medium
                    :width 'normal)
(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 120)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(define-key global-map (kbd "<C-tab>") (lambda () (interactive) (other-window -1)))
(define-key global-map (kbd "<C-iso-lefttab>") (lambda () (interactive) (other-window 1)))
(windmove-default-keybindings)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(setq-default fill-column 92)
(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 92)
  :config
  (global-visual-fill-column-mode))

(use-package helm
  :diminish
  :preface
  (defun dwim-helm-find-files-up-one-level-maybe ()
    (interactive)
    (if (looking-back "/" 1)
        (call-interactively 'helm-find-files-up-one-level)
      (delete-backward-char 1)))
  (defun dwim-helm-find-files-navigate-forward (orig-fun &rest args)
    "Adjust how helm-execute-persistent actions behaves"
    (if (file-directory-p (helm-get-selection))
        (apply orig-fun args)
      (helm-maybe-exit-minibuffer)))

  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   (:map helm-map
         ("<tab>" . helm-select-action)
         ("<backspace>" . dwim-helm-find-files-up-one-level-maybe)
         ))

  :config
  (require 'helm-misc)
  (require 'helm-locate)
  (setq helm-quick-update t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-ff-keep-cached-candidates nil) ; fix for memory leak?
  (helm-autoresize-mode t)
  (helm-mode t))

(use-package evil
  :after undo-tree
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (evil-set-undo-system 'undo-tree))
(use-package evil-collection
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)
  (evil-collection-init))

(use-package which-key
  :init
  (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 1))

(use-package magit
  :bind ((:map magit-status-mode-map)
         ("C-<tab>" . nil)))
(use-package magit-todos
  :diminish
  :after magit
  :config
  (global-hl-todo-mode 1)
  (magit-todos-mode 1)
  (setq hl-todo-keyword-faces
        '(("TODO" . "#2AA198")
          ("FIXME" . "#DC322F"))))

(use-package vterm)

(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))) ; only run after saving
(use-package flyspell
  :hook markdown-mode)

(use-package eglot)

(defun m/org-setup ()
  (org-indent-mode)
  (auto-fill-mode nil)
  (visual-line-mode t)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . m/org-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation nil)
  (setq org-src-fontify-natively t))
  (setq org-replace-disputed-keys t)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("⦿" "⊚" "⊙" "⚪" "⚪" "⚪" "⚪" "⚪"))) ;

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun m/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/conf/dotfiles/.emacs.d/init.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'m/org-babel-tangle-config)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("jl" . "src julia"))

(use-package org-chef)

(use-package julia-mode)
(use-package julia-repl
  :ensure nil
  :load-path "~/.emacs.d/plugins/julia-repl"
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (setq julia-repl-switches "-O3 -t2")
  (setq julia-repl-executable-records
        '((master "julia-master")
          (stable "julia")
          (remote "julia-remote")
          (lts "julia-lts")))
  (julia-repl-set-terminal-backend 'vterm))

(use-package haskell-mode)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))
(use-package edit-indirect)

(use-package yaml-mode)

(use-package gdscript-mode)

(use-package reftex)
(use-package auctex-latexmk)
(use-package pdf-tools
  :config (progn (define-key pdf-view-mode-map (kbd "h")
                   'pdf-annot-add-highlight-markup-annotation)
                 (define-key pdf-view-mode-map (kbd "t")
                   'pdf-annot-add-text-annotation)
                 (define-key pdf-view-mode-map (kbd "d")
                   'pdf-annot-delete)
                 (define-key pdf-view-mode-map (kbd "s")
                   'pdf-annot-add-strikeout-markup-annotation))
)
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (require 'reftex)
  (setq reftex-plug-into-AUCTeX t)
  (require 'auctex-latexmk)
  (auctex-latexmk-setup)
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (reftex-mode t)
              (flyspell-mode t)
              (visual-line-mode t)
              )))
