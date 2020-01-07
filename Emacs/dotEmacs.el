
;; Emacs Notes
;; C-h k <the keybind> to find what a key does
;; C-h b to list all bindings (should've used this more when fighting binds...)
;; C-q = quoted-insert "insert the next character, whatever it is" e.g. useful for inserting a tab
;; describe-char with cursor over character will say where the font face came from (useful for theming)
;; Use ibuffer to select and kill many buffers. kill-some-buffers is also okay
;; Use ediff-revision to easily manipulate working edits
;; Use re-builder to create a regex by seeing the results of it in the current buffer (super awesome)
;; Hit C-f while in ido to disable all completion (for when you're fighting it)
;; Amazing multiline editing: C-f to isearch-forward, then C-a to see all results, then e to edit all lines
;; diff-buffer-with-file to see a diff of current (unsaved) modifications

;; Used to load separate configuration files I've created. Order matters so they're scattered a bit
(setq user-init-dir "~/.emacs.d/macoy")
(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; BAD THING
;;
;; This is the worst thing ever. Remap keys so I can use C-c and C-x how I damn well please.
;; Subsequently, read C-u as my C-x and C-y as my C-c (yes, it sucks and is confusing as hell.
;; I blame the developers.
;;
(keyboard-translate ?\C-u ?\C-x)
(keyboard-translate ?\C-x ?\C-u)
(keyboard-translate ?\C-y ?\C-c)
(keyboard-translate ?\C-c ?\C-y)

;; This is a stupid hack around an apparent bug in edebug.
;; If set, don't define C-x anywhere. If C-x is defined then edebug complains about it not being a prefix key
;; You'll need to restart in order for it to work
;; Update: After keyboard-translate, this is no longer an issue. Leaving here in case I go back on keyboard-translate
(setq macoy-edebug-prefix-hack nil)

;; Handle installing packages
(load-user-file "packages.el")

;; Simpleclip makes system clipboard and emacs kill ring separate
;; This is sane copy paste behavior
;; I keep this near the top because many of my utilities rely on simpleclip
(when (require 'simpleclip)
  (simpleclip-mode 1))

;; Settings which affect the core behavior of Emacs as well as interface-changing things like ido
;; This also has random utilities for managing buffers and files
;; This should remain early in load order
(load-user-file "core-settings.el")

;; Various customizations for Org mode
(load-user-file "org-customizations.el")

;; Manipulate JIRAs in Org-mode
;; These customizations are required to use org-jira with lower permissions
(load-user-file "org-jira-customizations.el")

;; Org-drill and associated customizations
(load-user-file "drill-customizations.el")

;; Make it easier to create and switch desktops
(load-user-file "desktop-management.el")

;; Utilities for getting around in files (quick jump, saving points, etc.)
(load-user-file "navigation.el")

;; Various different ways to search
(load-user-file "search.el")

;; Tools for auto-formatting code, default formatting settings, etc.
(load-user-file "code-formatting.el")

;; All things tags and autocompletion
(load-user-file "tags-and-autocompletion.el")

;; Modes and customizations of modes for different syntaxes
(load-user-file "syntaxes.el")

;; Support for other languages (not programming languages) (mostly Japanese)
(load-user-file "language.el")

;; Customizations related to source control stuff like DSVN, Magit, etc.
(load-user-file "source-control.el")

;; Various compile commands and build system management
(load-user-file "build-systems.el")

;; Cut/copy/paste and multiple cursors stuff
(load-user-file "clipboard.el")

;; Keybindings for various modes
;; Note that not all keybinds are defined in this file
(load-user-file "keybinds.el")

;; Visual customizations which are okay to occur before the theme has been set in custom-set-variables
(load-user-file "visual-early.el")

;; Make it easier to switch which tags and search index files are used
(load-user-file "index-management.el")

;; Stuff unique to certain machines (mine here for reference)
(when (or (string-equal (user-login-name) "macoy")
		  (string-equal (user-login-name) "mmadson"))
  (load-file "~/.emacs-this-machine-only.el"))

;;
;; Hand-written by Macoy end
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(ansi-term-color-vector
   [unspecified "#ffffff" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#4271ae" "#4d4d4c"] t)
 '(custom-enabled-themes '(base16-ashes))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(ediff-split-window-function 'split-window-horizontally)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(fci-rule-color "#383838")
 '(gnus-logo-colors '("#2fdbde" "#c0c0c0") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(large-file-warning-threshold 60000000)
 '(linum-format " %7i ")
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill))
 '(org-support-shift-select t)
 '(package-archives
   (quote
	(("gnu" . "http://elpa.gnu.org/packages/")
	 ("melpa" . "http://melpa.org/packages/"))))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "AutoGen" "obj140"))
 '(projectile-indexing-method 'native)
 '(tab-width 4)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
	 (40 . "#CC9393")
	 (60 . "#DFAF8F")
	 (80 . "#D0BF8F")
	 (100 . "#E0CF9F")
	 (120 . "#F0DFAF")
	 (140 . "#5F7F5F")
	 (160 . "#7F9F7F")
	 (180 . "#8FB28F")
	 (200 . "#9FC59F")
	 (220 . "#AFD8AF")
	 (240 . "#BFEBBF")
	 (260 . "#93E0E3")
	 (280 . "#6CA0A3")
	 (300 . "#7CB8BB")
	 (320 . "#8CD0D3")
	 (340 . "#94BFF3")
	 (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

;;
;; Macoy late visual customizations (put here to play nice with themes)
;;

;; Visual customization which should happen after theme has been set
(load-user-file "visual-late.el")

;;
;; Macoy Handwritten end
;;

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
