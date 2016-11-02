(setq mouse-autoselect-window t)
(setq focus-follows-mouse t)
;; =================
;; Sources, packages
;; =================
(add-to-list 'load-path "~/.emacs.d/plugins/")
(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar packages '(evil
                   evil-numbers
                   evil-escape
                   evil-mc
                   solarized-theme
                   multi-term
                   powerline
                   powerline-evil
                   helm
                   ;;flycheck

                   haskell-mode
                   ghc

                   tuareg
                   markdown-mode
                   julia-mode

                   sml-mode
                   ;;helm-R
                   ))

(dolist (p packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; user info
(setq user-full-name "mtsch"
      user-mail-address "matijacufar@gmail.com")

;; =======
;; General
;; =======
(tool-bar-mode 0)
(menu-bar-mode 0)
(show-paren-mode 1)
(blink-cursor-mode 0)
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(global-set-key [C-tab] 'other-window)

; (setq linum-format "%4d")

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

 undo-limit 1000000

 ;; Force utf encoding
 buffer-file-coding-system        'utf-8-unix
 default-file-name-coding-system  'utf-8-unix
 default-keyboard-coding-system   'utf-8-unix
 default-process-coding-system    '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system   'utf-8-unix
 default-terminal-coding-system   'utf-8-unix
 )

;; Y or N prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; Theme
(load-theme 'solarized-light t)

;; Font
(set-face-attribute 'default nil
                    :family "Ununtu-Mono"
                    :height 110
                    :weight 'normal)

;; ==================
;; Evil mode settings
;; ==================
(require 'evil)
(evil-mode 1)
(add-to-list 'evil-insert-state-modes 'view-mode)

;; Remap org-mode meta keys for convenience
(mapcar (lambda (state)
          (evil-declare-key state org-mode-map
            (kbd "M-l") 'org-metaright
            (kbd "M-h") 'org-metaleft
            (kbd "M-k") 'org-metaup
            (kbd "M-j") 'org-metadown
            (kbd "M-L") 'org-shiftmetaright
            (kbd "M-H") 'org-shiftmetaleft
            (kbd "M-K") 'org-shiftmetaup
            (kbd "M-J") 'org-shiftmetadown))
        '(normal insert))

;; Increment or decrement numbers
(require 'evil-numbers)
(define-key evil-normal-state-map
  (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map
  (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Multicursor
(require 'evil-mc)
(global-evil-mc-mode 1)

;; Evil-escape
(require 'evil-escape)
;(global-set-key (kbd "ESC") 'evil-escape)
(global-set-key (kbd "C-c :") 'evil-escape)

;; =====================
;; Long lines and spaces
;; =====================
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq fci-rule-width 1)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(set-fill-column 80)

;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Tabs
(standard-display-ascii ?\t "→   ")
(setq-default indent-tabs-mode nil)

;; Trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Wrap lines when in text mode
(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)))

;; Regex indent
(global-set-key (kbd "C-x a r") 'align-regexp)

;; Highlight current line
(global-hl-line-mode t)

;; =======================
;; Emacs Speaks Statistics
;; =======================
(require 'ess-site)
(ess-toggle-underscore nil)
(add-hook 'ess-mode-hook
          (lambda ()
            (ess-set-style 'RStudio 'quiet)))

; (require 'julia-mode)
;(add-hook 'ess-julia-mode-hook
;          (lambda()
;            (define-key
;              ess-julia-mode-map (kbd "TAB") 'julia-latexsub-or-indent)))

;; TeX input mode for julia
(add-hook 'ess-julia-mode-hook
          (lambda () (set-input-method "TeX")))

(add-hook 'ess-julia-post-run-hook
          (lambda () (set-input-method "TeX")))


;; =========
;; Powerline
;; =========
(require 'powerline)
(require 'powerline-evil)
(powerline-default-theme)

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
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Change prefix
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; Buffers
(global-set-key (kbd "C-x b") 'helm-mini)
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

;; interactive mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(package-selected-packages
   (quote
    (sml-mode tuareg solarized-theme powerline-evil multi-term markdown-mode hindent helm-R ghc flycheck-hdevtools evil-numbers evil-mc evil-escape))))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
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

;; =====
;; OCaml
;; =====

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string
                             "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Load merlin-mode
(require 'merlin)
;(require 'ocp-indent)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)
;; Indentation
(setq tuareg-indent-align-with-first-arg nil)

;; ====
;; AGDA
;; ====

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
