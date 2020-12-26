;;; magit-todos-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magit-todos" "../../../../../.emacs.d/elpa/magit-todos-20201101.2240/magit-todos.el"
;;;;;;  "7c98635a4c6f700b68cbbc460c2537ad")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/magit-todos-20201101.2240/magit-todos.el

(defvar magit-todos-mode nil "\
Non-nil if Magit-Todos mode is enabled.
See the `magit-todos-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-todos-mode'.")

(custom-autoload 'magit-todos-mode "magit-todos" nil)

(autoload 'magit-todos-mode "magit-todos" "\
Show list of to-do items in Magit status buffer for tracked files in repo.

If called interactively, enable Magit-Todos mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'magit-todos-list "magit-todos" "\
Show to-do list of the current Git repository in a buffer.
With prefix, prompt for repository.

\(fn &optional DIRECTORY)" t nil)

(autoload 'magit-todos-list-internal "magit-todos" "\
Open buffer showing to-do list of repository at DIRECTORY.

\(fn DIRECTORY)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "magit-todos"
;;;;;;  "../../../../../.emacs.d/elpa/magit-todos-20201101.2240/magit-todos.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../.emacs.d/elpa/magit-todos-20201101.2240/magit-todos.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-todos" '("magit-todos-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/magit-todos-20201101.2240/magit-todos-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/magit-todos-20201101.2240/magit-todos.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-todos-autoloads.el ends here
