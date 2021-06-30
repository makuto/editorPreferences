;; Handle installing packages

(require 'seq)

;; Hack to get Melpa to work on Windows
(when (eq system-type 'windows-nt)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

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

(defun macoy-package-install (package)
  (message "Installing %s" package)
  (package-install package))

;; Install uninstalled packages
(let* ((package--builtins nil)
       (packages
        '(adaptive-wrap
          ag
          auto-complete
          avy
          base16-theme
          better-defaults
          clang-format
          csharp-mode
          diminish
          dired-narrow
          dsvn
          engine-mode
          expand-region
          flx-ido
          glsl-mode
          htmlize
          ido-vertical-mode
          ido-completing-read+
          ivy
          ivy-xref
          ;;iy-go-to-char ;; Now provided by submodule
          keyfreq
          lua-mode
          markdown-mode
          magit
          multiple-cursors
          ;; org-jira
          org-mru-clock
          php-mode
          powerline
          projectile
          rainbow-mode
          simpleclip
          smart-tabs-mode
          smex
          smooth-scrolling
          web-beautify
          web-mode
          yasnippet
          zenburn-theme
          )))
  (ignore-errors
    (message "Checking for packages to install...")
    (let ((packages (seq-remove 'package-installed-p packages)))
      (when packages
        (message "Installing packages...")
        (package-refresh-contents)
        (mapc 'macoy-package-install packages)))))
