;; Handle installing packages

;; Enable MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
;; TODO: Test if melpa still works
;; (package-initialize)

;; Install uninstalled packages
(let* ((package--builtins nil)
       (packages
        '(adaptive-wrap
		  ag
		  alect-themes
		  auto-complete
		  avy
		  base16-theme
		  better-defaults
		  clang-format
		  darktooth-theme
		  diminish
		  dired-narrow
		  dsvn
		  engine-mode
		  expand-region
		  flx-ido
		  ido-vertical-mode
		  ivy
		  ivy-xref
		  iy-go-to-char
		  keyfreq
		  magit
		  marmalade-demo
		  multiple-cursors
		  powerline
		  projectile
		  rainbow-mode
		  simpleclip
		  smex
		  smooth-scrolling
		  sublime-themes
		  swiper
		  web-beautify
		  web-mode
		  xah-find
		  yasnippet
		  zenburn-theme
		  )))
  (ignore-errors
    (let ((packages (remove-if 'package-installed-p packages)))
      (when packages
        (package-refresh-contents)
        (mapc 'package-install packages)))))
