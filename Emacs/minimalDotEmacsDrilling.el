(keyboard-translate ?\C-u ?\C-x)
(keyboard-translate ?\C-x ?\C-u)
(keyboard-translate ?\C-y ?\C-c)
(keyboard-translate ?\C-c ?\C-y)
(setq macoy-edebug-prefix-hack nil)
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
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)


;; Always highlight matching parenthesis. This is a necessity when using multiple-cursors because
;;  if show-paren-mode is disabled, typing multiple closing parentheses takes a long time due to
;;  the pause to highlight after each one
(show-paren-mode 1)

;; Themes are generally safe
(setq custom-safe-themes t)

;; make typing delete/overwrite selected text
(delete-selection-mode 1)

;; turn on highlighting current line
(global-hl-line-mode 1)

;; remember cursor position, for emacs 25.1 or later
(save-place-mode 1)

;; Make garbage collection happen less often (https://github.com/lewang/flx)
(setq gc-cons-threshold 20000000)

;; stop creating those backup~ files
(setq make-backup-files nil)

;; stop creating those #auto-save# files
(setq auto-save-default nil)

;; Don't create lock files
(setq create-lockfiles nil)

;; save/restore opened files
(desktop-save-mode 1)
;; This is needed only for theming. Desktop frames hold on to color values for some reason. We don't
;;  care too much about losing our frame configurations so this is okay
(setq desktop-restore-frames nil)

;; Automatically revert buffers if file changes underneath (unless there are unsaved changes)
(global-auto-revert-mode 1)

;; Instead of wrapping at character, wrap at word. This slightly improves readability
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode 1)

;; Hide toolbar (only needed on Linux?)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Set cursor to I-beam
(modify-all-frames-parameters (list (cons 'cursor-type '(bar . 2))))

;; Smex: Smart M-x completion
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-M-x") 'execute-extended-command)

;; Ido enable (this might be unnecessary as Emacs comes with it by default now)
(require 'ido)
(ido-mode t)
;; Ido display vertically (closer to Sublime)
(ido-vertical-mode 1)
;; Ido vertical use up and down to navigate options, left-right for history/directories
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; Ido flx settings: make ido have fuzzy sublime-like matching
 (require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Powerline: nicer status bar
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

;; Multiple cursors
(require 'multiple-cursors)

;; Make sure to change this in my-keys-minor-mode-map too
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
;;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-<f3>") 'mc/mark-all-like-this)
;; Make <return> insert a newline; multiple-cursors-mode can still be disabled with C-g.
(define-key mc/keymap (kbd "<return>") nil)
;; Clear these so that expand-region can have them
(define-key mc/keymap (kbd "C-'") nil)
(define-key mc/keymap (kbd "C-\"") nil)
(define-key mc/keymap (kbd "C-SPC") 'mc-hide-unmatched-lines-mode)
;; Adds one cursor to each line in the current region.
(global-set-key (kbd "C-S-l") 'mc/edit-lines)
;; Note that in my-keys I define cut, copy, and paste overrides which work with simpleclip & mc

;; Isearch customizations
(defun macoy-isearch-yank-clipboard ()
  "Insert the contents of the clipboard into isearch. We do this because we don't use the yank stuff"
  (interactive)
  (isearch-yank-string (simpleclip-get-contents))
  )
(define-key isearch-mode-map (kbd "C-v") 'macoy-isearch-yank-clipboard)
;; Go to next/previous result with arrow keys
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
;; Used for expanding the search by words
(define-key isearch-mode-map (kbd "C-'") 'isearch-yank-word-or-char)

;; If marked, use swiper to search mark
(defun macoy-isearch-search-mark ()
  "If marked, use isearch to search mark. Otherwise, isearch normally"
  (interactive)
  (call-interactively 'isearch-forward)
  (when (use-region-p)
	;; (isearch-search)
	;; (call-interactively 'isearch-forward)
	(isearch-yank-string (buffer-substring (region-beginning) (region-end)))
	)
  )
(global-set-key (kbd "C-f") 'macoy-isearch-search-mark)

;;
;; Custom multiple cursors cut/copy/paste handling
;;

(setq macoy-multiple-cursors-buffers nil)
(setq macoy-mc-buffer-index 0)

(defun macoy-mc-copy ()
  (interactive)
  (if (use-region-p)
	  (push (buffer-substring (region-beginning) (region-end)) macoy-multiple-cursors-buffers)
	;; TODO: Copy whole line if no region
	(message "TODO: Copy whole line if no region selected")
	)
  )

(defun macoy-mc-cut ()
  (interactive)
  ;; TODO: Cut whole line if no region
  (unless (use-region-p)
	(message "TODO: Cut whole line if no region selected")
	)
  
  (when (use-region-p)
	(push (buffer-substring (region-beginning) (region-end)) macoy-multiple-cursors-buffers)
	(kill-region (region-beginning) (region-end)))
  )

(defun macoy-mc-paste ()
  (interactive)
  ;; Delete selected text before insert if necessary
  (when (use-region-p)
	(delete-region (region-beginning) (region-end))
	)
  ;; If no macoy-multiple-cursors-buffers the user probably did a simple copy so paste that
  (unless macoy-multiple-cursors-buffers
	(call-interactively 'simpleclip-paste)
	)
  (when macoy-multiple-cursors-buffers
	(insert (nth macoy-mc-buffer-index macoy-multiple-cursors-buffers))
	;; Set up next cursor buffer index
	;; Ensure we don't go out of range of the buffers
	;; Sublime's behavior is to just paste all buffers at all marks, so our solution is different here
	(setq macoy-mc-buffer-index (min
								 (+ macoy-mc-buffer-index 1)
								 (- (length macoy-multiple-cursors-buffers) 1)))
	)
  )

(defun macoy-multiple-cursors-copy()
  "Copy at multiple cursors using macoy-multiple-cursors-buffers"
  (interactive)
  (setq macoy-multiple-cursors-buffers nil)
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'macoy-mc-copy cursor))
  ;; Append happens in reverse
  (setq macoy-multiple-cursors-buffers (reverse macoy-multiple-cursors-buffers))
  ;; Adding newline isn't correct but emacs won't copy the newline. It is slightly more useful
  ;;  to paste things with the newlines when collapsing multiple selections
  (simpleclip-set-contents (string-join macoy-multiple-cursors-buffers "\n"))
  )

(defun macoy-multiple-cursors-cut()
  "Cut at multiple cursors using macoy-multiple-cursors-buffers"
  (interactive)
  (setq macoy-multiple-cursors-buffers nil)
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'macoy-mc-cut cursor))
  (setq macoy-multiple-cursors-buffers (reverse macoy-multiple-cursors-buffers))
  ;; Adding newline isn't correct but emacs won't copy the newline. It is slightly more useful
  ;;  to paste things with the newlines when collapsing multiple selections
  (simpleclip-set-contents (string-join macoy-multiple-cursors-buffers "\n"))
  )

(defun macoy-multiple-cursors-paste()
  "Paste at multiple cursors using macoy-multiple-cursors-buffers"
  (interactive)
  (setq macoy-mc-buffer-index 0)
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'macoy-mc-paste cursor))
  )

(defun macoy-test-multiple-cursors-print-list()
  "Print buffers"
  (interactive)
  (message "%d in buffer" (length macoy-multiple-cursors-buffers))
  (dolist (buffer macoy-multiple-cursors-buffers)
	(message "Buffer: %s" buffer))
  )

;;
;; Custom copy/cut/paste functions so one key can work for simpleclip and multiple-cursors
;; Make sure to add these to mc/cmds-to-run-once and restart Emacs

(defun macoyCopy ()
  (interactive)
  ;; Clear buffers here in case they aren't using multiple cursors
  ;; Then, if they paste in multiple-cursors-mode it will paste simpleclip
  (setq macoy-multiple-cursors-buffers nil)
  (if (bound-and-true-p multiple-cursors-mode)
	  (call-interactively 'macoy-multiple-cursors-copy) ;; Was kill-ring-save
	(call-interactively 'simpleclip-copy))
  )

(defun macoyCut ()
  (interactive)
  ;; Clear buffers here in case they aren't using multiple cursors
  ;; Then, if they paste in multiple-cursors-mode it will paste simpleclip
  (setq macoy-multiple-cursors-buffers nil)
  (if (bound-and-true-p multiple-cursors-mode)
	  (call-interactively 'macoy-multiple-cursors-cut) ;; Was kill-region
	(call-interactively 'simpleclip-cut))
  )

(defun macoyPaste ()
  (interactive)
  (if (bound-and-true-p multiple-cursors-mode)
	  (call-interactively 'macoy-multiple-cursors-paste) ;; Was yank
	(call-interactively 'simpleclip-paste))
  )

(defun macoy-tags-query-replace-marked (replacement)
  (interactive (list
				(read-string (format "Replace %s with: "
									 (buffer-substring (region-beginning) (region-end))))))
  
  (tags-query-replace (buffer-substring (region-beginning) (region-end)) replacement)
  )

(defun macoy-tags-query-replace ()
  (interactive)
  (if (use-region-p)
	  (call-interactively 'macoy-tags-query-replace-marked (buffer-substring (region-beginning) (region-end)))
	(call-interactively 'tags-query-replace)
	)
  )

(defun macoy-add-edit-newline-before ()
  "Create a new line before the current line and go to it"
  (interactive)
  (back-to-indentation)
  (newline)
  (call-interactively 'indent-for-tab-command)
  (previous-line)
  (back-to-indentation)
  )

(defun macoy-add-edit-newline-after ()
  "Create a new line after the current line and go to it"
  (interactive)
  (end-of-visual-line)
  (newline)
  (call-interactively 'indent-for-tab-command)
  )

(global-set-key (kbd "C-<return>") 'macoy-add-edit-newline-after)
(global-set-key (kbd "S-<return>") 'macoy-add-edit-newline-before)
(global-set-key (kbd "C-S-<return>") 'macoy-add-edit-newline-before)

;;
;;
;; Macoy's keybinds which require better override
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-d") 'mc/mark-next-like-this)
	;; Custom copy/paste functions for working with simpleclip and multiple-cursors
	(define-key map (kbd "C-y") 'macoyCopy) ;; Actually C-c after keyboard-translate
	(define-key map (kbd "C-v") 'macoyPaste)
	(unless macoy-edebug-prefix-hack
	 (define-key map (kbd "C-u") 'macoyCut)) ;; Actually C-x after keyboard-translate
	;; In case you need the dumb copy paste (or multiple cursors clipboard after exiting mc)
	(define-key map (kbd "C-S-c") 'kill-ring-save)
	(define-key map (kbd "C-S-v") 'yank)
	(define-key map (kbd "C-S-x") 'kill-region)
	(define-key map (kbd "M-j") 'pop-global-mark)
	;; Overrides c-indent-line-or-region (this should be in C mode only, plus <tab>)
	;;(define-key map (kbd "C-i") 'clang-format)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " M")

(my-keys-minor-mode 1)

;;
;; Macoy's Keybind overrides
;;
;; Many come from http://ergoemacs.org/emacs/emacs_make_modern.html

;; Make it possible to easily input raw tabs instead of having to do C-q <tab>
(defun macoy-insert-tab ()
  "Make it possible to easily input raw tabs instead of having to do C-q <tab>"
  (interactive)
  (insert "	")
  )

;; Backtab is the same as S-<tab>
(global-set-key (kbd "<backtab>") 'macoy-insert-tab)

;; make {copy, cut, paste, undo} have {C-c, C-x, C-v, C-z} keys
;;(cua-mode 1) (disabled in favor of simpleclip)

(global-set-key (kbd "C-z") 'undo)

;; Ctrl shift P like sublime for commands
(global-set-key (kbd "C-S-p") 'smex)

;; Save As. was nil
(global-set-key (kbd "C-S-s") 'write-file)

;; Close. was kill-region
(global-set-key (kbd "C-w") 'kill-buffer)

;; Select All. was move-beginning-of-line
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Open. was open-line
(global-set-key (kbd "C-o") 'ido-find-file)

;; Save. was isearch-forward
(global-set-key (kbd "C-s") 'save-buffer)

;; Save As. was nil
(global-set-key (kbd "C-S-s") 'write-file)

;; Find. was forward-char
;; Replaced by swiper above
;;(global-set-key (kbd "C-f") 'isearch-forward)

;; Switch buffers. Was backward-char
(global-set-key (kbd "C-b") 'ido-switch-buffer)

;; Switch windows via ctrl tab
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") 'previous-multiframe-window)

;; Revert buffer
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Kill line like Sublime
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; Go to first character of line, not beginning of line. Was move-beginning-of-line
(global-set-key (kbd "<home>") 'back-to-indentation)

;; Toggle comment lines (same keybind as Sublime). This also works for regions
(global-set-key (kbd "C-/") 'comment-line)

;; Simpleclip cut copy paste (not sure if this is essential due to customize-group settings)
;; These are also set in my-keys mode with macoyCopy functions for multiple-cursors support,
;; overriding these defaults
(global-set-key (kbd "C-y") 'simpleclip-copy) ;; Actually C-c after keyboard-translate
(unless macoy-edebug-prefix-hack
  (global-set-key (kbd "C-u") 'simpleclip-cut)) ;; Actually C-x after keyboard-translate
(global-set-key (kbd "C-v") 'simpleclip-paste)

;; point-to-register and jump-to-register (was reverse search)
(global-set-key (kbd "C-r") 'jump-to-register)
(global-set-key (kbd "C-S-r") 'point-to-register)
;; copy-to-register and insert-register
(global-set-key (kbd "M-r") 'insert-register)
(global-set-key (kbd "M-R") 'copy-to-register)

;; Move to beginning/end of function
(global-set-key (kbd "M-<up>") 'beginning-of-defun)
(global-set-key (kbd "M-<down>") 'end-of-defun)
(global-set-key (kbd "C-<prior>") 'beginning-of-defun)
(global-set-key (kbd "C-<next>") 'end-of-defun)

;; Window management
;; Split horizonal (was transpose-chars)
(global-set-key (kbd "C-t") 'split-window-horizontally)
(global-set-key (kbd "M-t") 'split-window-vertically)
(global-set-key (kbd "C-S-w") 'delete-window)

;; Dired customizations
(require 'dired)
;; Hide details by default (show with '(')
;; See http://ergoemacs.org/emacs/emacs_dired_tips.html
(defun macoy-dired-mode-setup ()
  "To be run as a hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'macoy-dired-mode-setup)
(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)

;; Compilation mode customizations
;; (define-key compilation-mode-map (kbd "n") 'compilation-next-error)
;; (define-key compilation-mode-map (kbd "p") 'compilation-previous-error)
(require 'org-plus-contrib)
(require 'cl)
(require 'org-drill)


;; Make bindings work with org-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<tab>") nil)
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "C") nil)
  ;; Don't mess with my arrow keys. I use them a lot
  (define-key org-mode-map (kbd "C-S-<down>")  nil)
  (define-key org-mode-map (kbd "C-S-<left>")  nil)
  (define-key org-mode-map (kbd "C-S-<right>") nil)
  (define-key org-mode-map (kbd "C-S-<up>")	 nil)
  (define-key org-mode-map (kbd "M-S-<down>")  nil)
  (define-key org-mode-map (kbd "M-S-<left>")  nil)
  (define-key org-mode-map (kbd "M-S-<right>") nil)
  (define-key org-mode-map (kbd "M-S-<up>")	 nil)
  (define-key org-mode-map (kbd "M-<down>")	 nil)
  (define-key org-mode-map (kbd "M-<left>")	 nil)
  (define-key org-mode-map (kbd "M-<right>")   nil)
  (define-key org-mode-map (kbd "M-<up>")	     nil)
  (define-key org-mode-map (kbd "S-<down>")	 nil)
  (define-key org-mode-map (kbd "S-<left>")    nil)
  (define-key org-mode-map (kbd "S-<right>")   nil)
  (define-key org-mode-map (kbd "S-<up>")      nil)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-ashes)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (org-plus-contrib diminish base16-theme smex powerline ido-vertical-mode flx-ido multiple-cursors simpleclip))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'my-keys-minor-mode)
(diminish 'yas-minor-mode)
(diminish 'auto-complete-mode)
(diminish 'visual-line-mode)
(diminish 'org-indent-mode)
;; The following might not do anything/are unnecessary
(diminish 'adaptive-wrap-prefix-mode)
(diminish 'wrap-region-mode)
(diminish 'auto-revert-mode)
