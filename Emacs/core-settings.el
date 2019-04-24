
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

;; Some settings from http://ergoemacs.org/emacs/emacs_make_modern.html

;; make cursor movement stop in between camelCase words. (don't)
(global-subword-mode 0)

;; No beeping on windows
(setq ring-bell-function 'ignore)

;; Always highlight matching parenthesis. This is a necessity when using multiple-cursors because
;;  if show-paren-mode is disabled, typing multiple closing parentheses takes a long time due to
;;  the pause to highlight after each one
(show-paren-mode 1)

;; make typing delete/overwrite selected text
(delete-selection-mode 1)

;; remember cursor position, for emacs 25.1 or later
(save-place-mode 1)

;; Make garbage collection happen less often (https://github.com/lewang/flx)
;; (setq gc-cons-threshold 20000000)
;; Another idea:
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; These are tricky: the higher, the more responsive flx ido is, but the slower it takes on minibuffer exit
(defun my-minibuffer-setup-hook ()
  ;; (setq gc-cons-threshold most-positive-fixnum))
  ;; (setq gc-cons-threshold 20000000))
  (setq gc-cons-threshold 80000000))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


;; stop creating those backup~ files
(setq make-backup-files nil)

;; stop creating those #auto-save# files
(setq auto-save-default nil)

;; Don't create lock files
(setq create-lockfiles nil)

;; Automatically revert buffers if file changes underneath (unless there are unsaved changes)
(global-auto-revert-mode 1)

(defun macoy-kill-transient-buffers ()
  "Auto kill buffers which aren't important to let hang around. You shouldn't run this while using things which use these buffers!"
  (interactive)
  ;; TODO: Make sure dependent buffers aren't broken when this happens!
  (setq macoy-buffers-to-kill (list
							   "*Backtrace*"
							   "*CTags-out*"
							   "*Calc Trail*"
							   "*Calculator*"
							   "*Codesearch*"
							   "*Codesearch-Index*"
							   "*Compile-Log*"
							   "*Completions*"
							   "*Diff*"
							   "*Ediff Registry*"
							   "*Gimme-checkout*"
							   "*Gimme-GetLatest*"
							   "*Help*"
							   "*Packages*"
							   "*ag search*"
							   "*compilation*"
							   "*log-edit-files*"
							   "*svn output*"
							   "*vc-change-log*"
							   "*vc-diff*"
							   "*xref*"
							   "*Macoy-Select-Search*"
							   "*Occur*"
							   ))
  (mapcar
   (lambda (buffer-to-kill)
	 (when (get-buffer buffer-to-kill)
		(kill-buffer buffer-to-kill))
	 )
   macoy-buffers-to-kill
   )
  )

;; Store recently opened files so we can easily reopen them
(recentf-mode 1)
;; Store more recent files
(setq recentf-max-saved-items 100)

;; Smex: Smart M-x completion
(when (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-M-x") 'execute-extended-command)
  )

;; Ido enable (this might be unnecessary as Emacs comes with it by default now)
(when (require 'ido)
  (ido-mode t)
  ;; Ido display vertically (closer to Sublime)
  (ido-vertical-mode 1)
  ;; Ido vertical use up and down to navigate options, left-right for history/directories
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  ;; Don't try searching other directories when there are no matches
  (setq ido-auto-merge-work-directories-length -1)

  ;; Ido flx settings: make ido have fuzzy sublime-like matching
  (when (require 'flx-ido)
	(ido-mode 1)
	(ido-everywhere 1)
	(flx-ido-mode 1)
	;; disable ido faces to see flx highlights.
	(setq ido-enable-flex-matching t)
	(setq ido-use-faces nil)
	)

  (setq ido-everywhere t)
  
  ;; Use ido for recentf file selection
  ;; From https://masteringemacs.org/article/find-files-faster-recent-files-package
  (defun ido-recentf-open ()
	"Use `ido-completing-read' to \\[find-file] a recent file"
	(interactive)
	(find-file (ido-completing-read "Find recent file: " recentf-list))
	)
  (global-set-key (kbd "C-S-t") 'ido-recentf-open)
  )

;; Projectile: this does "project" stuff like quick find https://github.com/bbatsov/projectile
;; C-p for ctrl-p-like find
(when (require 'projectile)
  ;; Ignore autogenerated files. Doesn't work. I used customize-group projectile for ignored dirs
  ;;  which can be found near the bottom of this file
  (setq projectile-globally-ignored-files
        (append '("AutoGen"
                  "*_ast.*"
				  "3rdparty/Node/*")
                projectile-globally-ignored-files))
  (projectile-mode 1)

  ;; Make projectile mode-line more minimal
  (when (string-equal (user-login-name) "macoy")
	(defun macoy-projectile-mode-line ()
	  (format " [%s]" (projectile-project-name))
	  )
	(setq projectile-mode-line-function 'macoy-projectile-mode-line)
	)

  (when (string-equal (user-login-name) "mmadson")
	;; Older version syntax
	(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
	)
  )
;;
;; File-related shortcuts
;;

(when (require 'simpleclip)
  (defun macoy-copy-buffer-filename-to-clipboard ()
	(interactive)
	(simpleclip-set-contents buffer-file-name)
	)
  )

;; Open file in explorer
(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))
