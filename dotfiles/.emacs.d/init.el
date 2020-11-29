;; =================
;; Sources, packages
;; =================
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "~/.emacs.d/plugins/julia-repl")
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)
(setq auto-package-update-interval 1)
(auto-package-update-maybe)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages '(auto-package-update
                   bind-key
                   unicode-fonts
                   magit magit-todos hl-todo
                   evil evil-escape
                   helm
		   smart-mode-line
                   solarized-theme
                   fill-column-indicator
                   julia-mode vterm
                   ess
                   haskell-mode ghc dhall-mode
                   markdown-mode
                   flycheck
                   elpy pyvenv flycheck python-pytest py-isort
                   ))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

; gc cons to 100mb for performance
(setq gc-cons-threshold 100000000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" default))
 '(package-selected-packages
   '(unicode-fonts magit magit-todos hl-todo evil evil-numbers evil-escape evil-mc helm solarized-theme fill-column-indicator julia-mode ess haskell-mode ghc markdown-mode flycheck elpy pyvenv flycheck python-pytest py-isort)))

;; ========================
;; Appearance and usability
;; ========================
(setq default-tab-width 4)
(setq c-basic-offset 4)
(setq c-indent-level 4)
(setq mouse-autoselect-window t)
(setq focus-follows-mouse t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode 1)
(blink-cursor-mode 0)
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(setq user-full-name "mtsch"
      user-mail-address "matijacufar@gmail.com")

(setq-default
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t
 initial-scratch-message ""
 menu-prompting nil
 confirm-kill-emacs 'y-or-n-p
 line-number-mode t
 column-number-mode t

 ;; Scrolling
 mouse-wheel-mode t
 echo-keystrokes 0.1

 redisplay-dont-pause t
 scroll-margin 1
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1
 mouse-wheel-follow-mouse 't
 mouse-wheel-scroll-amount '(1 ((shift) . 1))

 undo-limit 10000

 ;; Force utf encoding
 buffer-file-coding-system        'utf-8-unix
 default-file-name-coding-system  'utf-8-unix
 default-keyboard-coding-system   'utf-8-unix
 default-process-coding-system    '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system   'utf-8-unix
 default-terminal-coding-system   'utf-8-unix
 )

(defalias 'yes-or-no-p 'y-or-n-p)

(load-theme 'solarized-light t)

;; =========
;; Powerline
;; =========
(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'automatic)

;; =====
;; Fonts
;; =====
(require 'unicode-fonts)
(unicode-fonts-setup)
(set-frame-font "-*-UbuntuMono Nerd Font-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
(set-fontset-font t 'unicode
                  "-*-UbuntuMono Nerd Font-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1"
                  nil
                  'prepend
                  )

(add-to-list 'default-frame-alist
             '(font . "-ADBO-Source Code Pro-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"
 ))
(set-face-attribute 'default t :font "-ADBO-Source Code Pro-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"
)

;; ==================
;; Evil mode settings
;; ==================
(require 'evil)
(evil-mode 1)
(add-to-list 'evil-insert-state-modes 'view-mode)

;; =====================
;; Long lines and spaces
;; =====================
(require 'fill-column-indicator)
(setq-default fci-rule-column 92)
(setq-default fill-column 92)
(setq fci-rule-width 1)
(add-hook 'after-change-major-mode-hook 'fci-mode)

;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Tabs
(standard-display-ascii ?\t "→   ")
(setq-default indent-tabs-mode nil)

;; Trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline 't)

;; Wrap lines when in text mode
(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)))

;; Regex indent

;; Highlight current line
(global-hl-line-mode t)

;; ====
;; Helm
;; ====
(require 'helm-config)
(require 'helm-R)
(require 'helm-misc)
(require 'helm-locate)
(setq helm-quick-update t)
(setq helm-bookmark-show-location t)
(setq helm-buffers-fuzzy-matching t)
(helm-mode t)

;; Keymaps
(require 'bind-key)
(bind-key* "<C-tab>" 'other-window)
(bind-key "M-y" 'helm-show-kill-ring)
(bind-key (kbd "C-x a r") 'align-regexp)

;; Change prefix
(bind-key "C-c h" 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; Buffers
(bind-key "M-x" 'helm-M-x)
(bind-key "C-x C-f" 'helm-find-files)
(bind-key "C-x b" 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; Use TAB in helm
(define-key helm-map (kbd "<tab>") 'helm-select-action)
(define-key helm-map (kbd "C-i") 'helm-select-action)
(define-key helm-map (kbd "C-z")  'helm-execute-persistent-action)

;; Helm shrinks
(helm-autoresize-mode t)

;; Make helm behave like ido
(defun dwim-helm-find-files-up-one-level-maybe ()
  (interactive)
  (if (looking-back "/" 1)
      (call-interactively 'helm-find-files-up-one-level)
    (delete-backward-char 1)))

(define-key helm-read-file-map (kbd "<backsqpace>")
  'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-read-file-map (kbd "DEL")
  'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-find-files-map (kbd "<backspace>")
  'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-find-files-map (kbd "DEL")
  'dwim-helm-find-files-up-one-level-maybe)

(defun dwim-helm-find-files-navigate-forward (orig-fun &rest args)
  "Adjust how helm-execute-persistent actions behaves"
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))

(define-key helm-map (kbd "<return>")
  'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "RET")
  'helm-maybe-exit-minibuffer)
(define-key helm-find-files-map (kbd "<return>")
  'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "<return>")
  'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "RET")
  'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "RET")
  'helm-execute-persistent-action)

(advice-add 'helm-execute-persistent-action
            :around #'dwim-helm-find-files-navigate-forward)

;; =====
;; Magit
;; =====
(require 'magit)
(require 'magit-todos)
(global-hl-todo-mode 1)
(magit-todos-mode 1)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#2AA198")
        ("FIXME"  . "#DC322F")))

;; =====
;; Julia
;; =====
(require 'julia-mode)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
(setq julia-repl-switches "-O3")
(setq julia-repl-executable-records
      '((master "julia")
        (stable "julia-stable")
        (lts "julia-lts")))
(julia-repl-set-terminal-backend 'vterm)

;; =======================
;; Emacs Speaks Statistics
;; =======================
(require 'ess-site)
(ess-disable-smart-S-assign nil)
(add-hook 'ess-mode-hook
          (lambda ()
            (ess-set-style 'RStudio 'quiet)))

;; =======
;; Haskell
;; =======
(require 'haskell-mode)

;; interactive
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; indentation
(add-hook 'haskell-mode-hook 'haskell-indent-mode)

;; compilation
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))
(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;; ghc-mod
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; ========
;; Markdown
;; ========
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ==========
;; Multi term
;; ==========
(setq multi-term-program "/bin/bash")

;; ======
;; PYTHON
;; ======
(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setenv "WORKON_HOME" "/home/m/.conda/envs")
(pyvenv-mode 1)
(eval-after-load "elpy"
  '(progn
     (define-key elpy-mode-map (kbd "M-d") 'elpy-goto-definition)
     (define-key elpy-mode-map (kbd "C-x t") 'python-pytest-popup)
     (define-key elpy-mode-map (kbd "C-x C-t") 'python-pytest-file-dwim)
     ;(add-hook 'before-save-hook 'py-isort-before-save)
     ))

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))

;; =====
;; EMAIL
;; =====
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'smtpmail)

; smtp
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("imap.gmail.com" 993 nil nil))
      smtpmail-default-smtp-server "imap.gmail.com"
      smtpmail-smtp-server "imap.gmail.com"
      smtpmail-smtp-service 993
      smtpmail-stream-type 'starttls
      smtpmail-debug-info t)

(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/email/main"))

(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent Items")
(setq mu4e-trash-folder  "/Trash")
(setq message-signature-file "~/.emacs.d/.signature") ; put your signature in this file

; get mail
(setq mu4e-get-mail-command "mbsync -a"
      mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval 120
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

(setq mu4e-maildir-shortcuts
      '( ("/INBOX" . ?i)
         ("/Sent Items" . ?s)
         ("/Trash" . ?t)
         ("/Drafts" . ?d)))

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "matijacufar@gmail.com"
    user-mail-address "matijacufar@gmail.com"
    user-full-name "Matija Čufar")

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (flyspell-mode)))

(provide 'init)
;;; init ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
