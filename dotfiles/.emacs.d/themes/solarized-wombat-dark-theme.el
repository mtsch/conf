(require 'solarized)
(deftheme solarized-wombat-dark "The solarized-wombat-dark colour theme of Solarized colour theme flavor.")
(solarized-with-color-variables 'dark 'solarized-wombat-dark
  '((base03 . "#2a2929")
    (base02 . "#2f2f2d")
    (base01 . "#6a6965")
    (base00 . "#74736e")
    (base0 . "#8c8b85")
    (base1 . "#999791")
    (base2 . "#e8e5db")
    (base3 . "#f6f2e8")
    (yellow . "#e5c06d")
    (orange . "#ddaa6f")
    (red . "#ffb4ac")
    (magenta . "#e5786d")
    (violet . "#834c98")
    (blue . "#a4b5e6")
    (cyan . "#7ec98f")
    (green . "#8ac6f2")
    (yellow-d . "#4c4436")
    (yellow-l . "#f4e8cf")
    (orange-d . "#4a4136")
    (orange-l . "#f2e4cf")
    (red-d . "#4f4340")
    (red-l . "#f9e6db")
    (magenta-d . "#4c3935")
    (magenta-l . "#f6dace")
    (violet-d . "#3b313d")
    (violet-l . "#dfd0d8")
    (blue-d . "#40424a")
    (blue-l . "#e6e6e7")
    (cyan-d . "#3a463b")
    (cyan-l . "#deead5")
    (green-d . "#3d454c")
    (green-l . "#e2e9ea")
    (yellow-1bg . "#433d33")
    (orange-1bg . "#423b33")
    (red-1bg . "#463c3a")
    (magenta-1bg . "#443532")
    (blue-1bg . "#3a3c41")
    (cyan-1bg . "#363f36")
    (green-1bg . "#383e43")
    (violet-1bg . "#362f38")
    (yellow-1fg . "#ecce91")
    (orange-1fg . "#e6bf92")
    (red-1fg . "#fdc7bd")
    (magenta-1fg . "#ee9d90")
    (violet-1fg . "#a67cb0")
    (blue-1fg . "#bdc7e6")
    (cyan-1fg . "#a3d6a9")
    (green-1fg . "#aed3ef")
    (yellow-2bg . "#6f6143")
    (orange-2bg . "#6c5944")
    (red-2bg . "#785d59")
    (magenta-2bg . "#704842")
    (violet-2bg . "#4c3852")
    (blue-2bg . "#585d6e")
    (cyan-2bg . "#4b644f")
    (green-2bg . "#506372")
    (yellow-2fg . "#efd6a4")
    (orange-2fg . "#eacaa4")
    (red-2fg . "#fcd0c6")
    (magenta-2fg . "#f1afa2")
    (violet-2fg . "#b795bc")
    (blue-2fg . "#cad0e7")
    (cyan-2fg . "#b5dcb6")
    (green-2fg . "#bed9ed"))
  '((custom-theme-set-faces theme-name
			    `(default
			       ((,class
				 (:foreground ,(solarized-color-blend base03 base3 0.15 2)
					      :background ,base03))))
			    `(highlight
			      ((,class
				(:background ,violet))))
			    `(font-lock-builtin-face
			      ((,class
				(:foreground ,magenta))))
			    `(font-lock-constant-face
			      ((,class
				(:foreground ,blue))))
			    `(font-lock-comment-face
			      ((,class
				(:foreground ,base00))))
			    `(mode-line
			      ((,class
				(:foreground ,base2 :background ,(solarized-color-blend base03 base3 0.85 2)))))
			    `(mode-line-inactive
			      ((,class
				(:foreground ,base00 :background ,(solarized-color-blend base03 "black" 0.85 2)))))
			    `(mode-line-buffer-id
			      ((,class
				(:foreground ,base3 :weight bold))))
			    `(minibuffer-prompt
			      ((,class
				(:foreground ,base1))))
			    `(vertical-border
			      ((,class
				(:foreground ,base03)))))))
(provide-theme 'solarized-wombat-dark)
(provide 'solarized-wombat-dark-theme)
