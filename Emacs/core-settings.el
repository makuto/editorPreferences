;; After having troubles with Windows 10 writing CRLF, I'm going to try this
;; Always prefer UTF-8
(prefer-coding-system 'utf-8)

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

;; Don't warn on large files (I have very big TAGS)
(setq large-file-warning-threshold 90000000)

;; stop creating those backup~ files
;; TODO: Use https://www.emacswiki.org/emacs/BackupEachSave etc. instead (the below code doesn't
;; save a backup every time you save the buffer)
(setq make-backup-files nil)
;; (setq make-backup-files t)
;; (when make-backup-files
;;   (setq
;;    ; Backup version-controlled files
;;    vc-make-backup-files t
;;    ; don't clobber symlinks
;;    backup-by-copying t
;;    backup-directory-alist
;;    ; don't litter my fs tree
;;     '(("." . "~/.macoy-emacs-backups/"))
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    ; use versioned backups
;;    version-control t))

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
       (kill-buffer buffer-to-kill)))
   macoy-buffers-to-kill))

(defun macoy-bury-buffer-anywhere (buffer-or-name)
  "Bury all windows showing BUFFER-OR-NAME.
BUFFER-OR-NAME may be a buffer or the name of an existing buffer
and defaults to the current buffer. For example, 
(macoy-bury-buffer-anywhere \"*Compile-Log*\") 
would dismiss the compile log, if it was visible"
  (let ((buffer (window-normalize-buffer buffer-or-name))
        ;; Handle the "inverted" meaning of the FRAME argument wrt other
        ;; `window-list-1' based function.
        (all-frames t))
    (dolist (window (window-list-1 nil nil all-frames))
      (when (eq (window-buffer window) buffer)
        (switch-to-prev-buffer window)))))

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
  
  ;; No really, do ido everywhere
  (when (require 'ido-completing-read+)
    (ido-ubiquitous-mode 1))

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
        (append '("*_ast.*")
                projectile-globally-ignored-files))
  (setq projectile-globally-ignored-directories
        (append '("AutoGen"
                  "3rdparty"
                  "obj140"
                  ".build"
                  ".cquery_cached_index")
                projectile-globally-ignored-files))
  (projectile-mode 1)

  ;; Make projectile mode-line more minimal
  ;; TODO: Make this work based on version!
  (defun macoy-projectile-mode-line ()
    (format " [%s]" (projectile-project-name)))
  (setq projectile-mode-line-function 'macoy-projectile-mode-line)
  ;; Older version syntax
  ;; (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  )
;;
;; File-related shortcuts
;;

(when (require 'simpleclip)
  (defun macoy-copy-buffer-filename-to-clipboard ()
    (interactive)
    (simpleclip-set-contents buffer-file-name))

  ;; TODO
  ;; (defun macoy-browse-copy-path ()
  ;;   (interactive)
  ;;   (simpleclip-set-contents (ido-find-file)))
  )

;; Open file in explorer
(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(global-set-key (kbd "<f10>") 'browse-file-directory)

;; Open marked path
(defun macoy-open-marked-path-file ()
  "Open the path in region."
  (interactive)
  (when (use-region-p)
    (find-file (buffer-substring (region-beginning) (region-end)))))

;;
;; Tabs and indentation
;;

;; Delete tabs instead of converting them to spaces
(setq backward-delete-char-untabify-method nil)
;; From https://dougie.io/emacs/indentation (with some modifications
;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun enable-tabs ()
  (interactive)
  ;; (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

;; Hooks to Enable Tabs
(add-hook 'c-mode-hook 'enable-tabs)
(add-hook 'c++-mode-hook 'enable-tabs)
(add-hook 'lua-mode-hook 'enable-tabs)

;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'disable-tabs)
(add-hook 'python-mode-hook 'disable-tabs)

;; Requires Guake
;; Thanks https://dougie.io/emacs/open-directory-in-terminal-app/
(defun open-terminal-in-workdir ()
  (interactive)
  (call-process-shell-command
   (concat "guake --show --new-tab=" default-directory) nil 0))

(global-set-key (kbd "<f12>") 'open-terminal-in-workdir)
