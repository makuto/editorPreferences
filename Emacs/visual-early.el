;; Macoy's primarily visual customizations (make sure to run visual_late.el too)
;; Note that there's still things in custom-set-variables and custom-set-faces which affect visuals (see .emacs)

;; Themes are generally safe
(setq custom-safe-themes t)

;; turn on highlighting current line
(global-hl-line-mode 1)

;; For theming: Make base16 not have distinct fringes, for a more minimal look
(require 'base16-theme)
(setq base16-distinct-fringe-background nil)

;; Hide toolbar (only needed on Linux?)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

;; Set cursor to I-beam
(modify-all-frames-parameters (list (cons 'cursor-type '(bar . 2))))
;; 4k monitor demands extra thicc cursor
(when (string-equal (user-login-name) "macoy")
  (modify-all-frames-parameters (list (cons 'cursor-type '(bar . 3))))
  )

;; Scrolling
;; https://www.emacswiki.org/emacs/SmoothScrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; Two lines at a time    
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;(setq scroll-step 1) ;; keyboard scroll one line at a time
;; (setq scroll-conservatively 10000)
;; (setq auto-window-vscroll nil)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;;
;; Powerline: nicer status bar
;;
(require 'powerline)
(setq powerline-default-separator 'butt)
(setq powerline-display-hud nil)
(setq powerline-display-buffer-size nil)
(setq powerline-display-mule-info nil)
(powerline-default-theme)
;; powerline-default-theme
;; powerline-center-theme
;; powerline-center-evil-theme
;; powerline-vim-theme
;; powerline-nano-theme

;; Instead of wrapping at character, wrap at word. This slightly improves readability
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode 1)

;; After wrapping, try to line up the text with the wrapped line column
(define-globalized-minor-mode my-global-adaptive-wrap-mode adaptive-wrap-prefix-mode
  (lambda () (adaptive-wrap-prefix-mode 1)))
(my-global-adaptive-wrap-mode 1)
(setq-default adaptive-wrap-extra-indent 1)

;; Show whitespace
;; Not enabled globally because it looks a bit too ugly for my tastes; I can toggle it when needed
;;(require 'whitespace)
;;(global-whitespace-mode 0)
(setq whitespace-style '(faces tabs tabs-mark spaces space-mark))
(setq whitespace-line-column 100)
(setq whitespace-newline nil)

;;
;; Auto Theming
;;

;; Follow the instructions at https://github.com/makuto/auto-base16-theme to get all this set up
(when (string-equal (user-login-name) "mmadson")
  (setq macoy-auto-theme-schemer2-bin "c:/Users/mmadson/go/bin/schemer2.exe")
  (setq macoy-auto-theme-script-dir "f:/gitRepos/auto-base16-theme")
  (setq macoy-auto-theme-output-file "c:/Users/mmadson/AppData/Roaming/.emacs.d/elpa/base16-theme-20180320.2254/base16-my-auto-theme.el")
  )
(when (string-equal (user-login-name) "macoy")
  (setq macoy-auto-theme-schemer2-bin "schemer2")
  (setq macoy-auto-theme-script-dir "~/Development/code/repositories/auto-base16-theme")  
  (setq macoy-auto-theme-output-file "~/.emacs.d/elpa/base16-theme-20180320.2254/base16-my-auto-theme.el")
  )

(defun macoy-generate-auto-theme ()
  "Create a base16 auto-theme using AutoBase16Theme.py based on the image selected."
  (interactive)

  (let ((default-directory macoy-auto-theme-script-dir))
	(compile
	 (format "%s -format img::colors -in \"%s\" -out colors.txt && python3 AutoBase16Theme.py emacs-base16-theme-template.el %s"
			 macoy-auto-theme-schemer2-bin (read-file-name "Image: ") macoy-auto-theme-output-file))
	)
  )

;; Reference Windows command:
;;c:/Users/mmadson/go/bin/schemer2.exe -format img::colors -in C:/Users/mmadson/Downloads/Wallpapers/32\ -\ fHFDkjY.jpg -out colors.txt && python3 AutoBase16Theme.py emacs-base16-theme-template.el c:/Users/mmadson/AppData/Roaming/.emacs.d/elpa/base16-theme-20180320.2254/base16-my-auto-theme.el
