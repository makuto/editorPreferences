
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
;; Macoy's Keybind overrides
;;
;; Some come from http://ergoemacs.org/emacs/emacs_make_modern.html

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

(defun macoy-mark-whole-buffer ()
  "Save a register with start position in case the user wants to go back to where they were before
 they marked the buffer"
  (interactive)
  ;; From Macoy's navigation.el
  (macoy-save-place)
  (call-interactively 'mark-whole-buffer)
  )

;; Select All. was move-beginning-of-line
(global-set-key (kbd "C-a") 'macoy-mark-whole-buffer)

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

;; Open ibuffer (good for killing many buffers)
(global-set-key (kbd "M-w") 'ibuffer)

;; Switch desktops
(global-set-key (kbd "C-S-b") 'macoy-switch-desktop)

;; Create desktop
(global-set-key (kbd "M-d") 'macoy-save-desktop)

;; Switch windows via ctrl tab
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<tab>") 'previous-multiframe-window)

;; Find file in project (via projectile) was previous-line
(global-set-key (kbd "C-p") 'projectile-find-file)

;; Revert buffer
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Kill line like Sublime
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; Go to first character of line, not beginning of line. Was move-beginning-of-line
(global-set-key (kbd "<home>") 'back-to-indentation)

;; Toggle comment lines (same keybind as Sublime). This also works for regions
(global-set-key (kbd "C-/") 'comment-line)

(defun macoy-kill-subword ()
  "Temporarily enable subword mode to kill camelCase subword"
  (interactive)
  (subword-mode 1)
  (call-interactively 'kill-word)
  (subword-mode 0)
  )

(defun macoy-kill-subword-backward ()
  "Temporarily enable subword mode to kill camelCase subword"
  (interactive)
  (subword-mode 1)
  (call-interactively 'backward-kill-word)
  (subword-mode 0)
  )

(global-set-key (kbd "C-S-<delete>") 'macoy-kill-subword)
(global-set-key (kbd "C-S-<backspace>") 'macoy-kill-subword-backward)

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

;; Occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)

;; Move to beginning/end of function
(global-set-key (kbd "M-<up>") 'beginning-of-defun)
(global-set-key (kbd "M-<down>") 'end-of-defun)
(global-set-key (kbd "C-<prior>") 'beginning-of-defun)
(global-set-key (kbd "C-<next>") 'end-of-defun)

(when (require 'expand-region)
  ;; I don't like it creating temporary binds (what if I want to type those symbols?)
  (setq expand-region-fast-keys-enabled nil)
  (global-set-key (kbd "C-'") 'er/expand-region)
  (global-set-key (kbd "C-\"") 'er/contract-region)
  (global-set-key (kbd "M-'") 'er/mark-defun)
  (global-set-key (kbd "M-\"") 'er/mark-paragraph)
  )

;; Window management
;; Split horizonal (was transpose-chars)
(global-set-key (kbd "C-t") 'split-window-horizontally)
(global-set-key (kbd "M-t") 'split-window-vertically)
(global-set-key (kbd "C-S-w") 'delete-window)

;; Go back (unfortunately no forward yet)
(global-set-key (kbd "M-j") 'pop-global-mark)

;; Replace all of a tag in all files
(global-set-key (kbd "M-a") 'tags-query-replace)

;; Dired customizations
(when (require 'dired)
  ;; Hide details by default (show with '(')
  ;; See http://ergoemacs.org/emacs/emacs_dired_tips.html
  (defun macoy-dired-mode-setup ()
	"To be run as a hook for `dired-mode'."
	(dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'macoy-dired-mode-setup)
  ;; Reuse buffer (from http://ergoemacs.org/emacs/emacs_dired_tips.html)
  ;; Was dired-find-file
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "S-<return>") 'dired-find-file)
  ;; Was dired-up-directory
  (define-key dired-mode-map (kbd "<backspace>") (lambda () (interactive) (find-alternate-file "..")))
  )

;; Compilation mode customizations
(define-key compilation-mode-map (kbd "n") 'compilation-next-error)
(define-key compilation-mode-map (kbd "p") 'compilation-previous-error)

;; Re Builder customizations
(when (require 're-builder)
  (define-key reb-mode-map (kbd "C-<up>") 'reb-prev-match)
  (define-key reb-mode-map (kbd "C-<down>") 'reb-next-match)
  )

;;
;; Make bindings work with org-mode
;;

(defun macoy-org-insert-heading-respect-content-before ()
  "The same as `org-insert-heading-respect-content' only do it before current heading"
  (interactive)
  (call-interactively 'org-previous-visible-heading)
  (call-interactively 'org-insert-heading-respect-content)
  )

;; Note that org keybinds are kept in org-customizations.el

;; Make bindings work with magit
(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-<tab>") nil))


;;
;; Multiple cursors
;;
(when (require 'multiple-cursors)
  ;; Make sure to change this in my-keys-minor-mode-map too
  (global-set-key (kbd "C-d") 'mc/mark-next-like-this)
  ;;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "M-<f3>") 'mc/mark-all-like-this)
  ;; Adds one cursor to each line in the current region.
  (global-set-key (kbd "C-S-l") 'mc/edit-lines)

  (define-key mc/keymap (kbd "C-S-d") 'mc/skip-to-next-like-this)
  ;; Make <return> insert a newline; multiple-cursors-mode can still be disabled with C-g.
  (define-key mc/keymap (kbd "<return>") nil)
  ;; Clear these so that expand-region can have them
  (define-key mc/keymap (kbd "C-'") nil)
  (define-key mc/keymap (kbd "C-\"") nil)
  (define-key mc/keymap (kbd "C-SPC") 'mc-hide-unmatched-lines-mode)

  ;; Ignore wrapping when doing motions in multiple-cursors
  (define-key mc/keymap (kbd "<end>") 'end-of-line)
  (define-key mc/keymap (kbd "<down>") 'next-logical-line)
  (define-key mc/keymap (kbd "<up>") 'previous-logical-line)
  
  ;; Note that in my-keys I define cut, copy, and paste overrides which work with simpleclip & mc
  )
;;
;;
;; Macoy's keybinds which require better override
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-d") 'mc/mark-next-like-this)
	(define-key map (kbd "C-M-a") 'macoy-clang-format-region-or-buffer)
	;; Custom copy/paste functions for working with simpleclip and multiple-cursors
	(define-key map (kbd "C-y") 'macoyCopy) ;; Actually C-c after keyboard-translate
	(define-key map (kbd "C-v") 'macoyPaste)
	(unless macoy-edebug-prefix-hack
	 (define-key map (kbd "C-u") 'macoyCut)) ;; Actually C-x after keyboard-translate
	;; In case you need the dumb copy paste (or multiple cursors clipboard after exiting mc)
	(define-key map (kbd "C-S-c") 'kill-ring-save)
	(define-key map (kbd "C-S-v") 'yank)
	(define-key map (kbd "C-S-x") 'kill-region)
	(define-key map (kbd "M-a") 'macoy-tags-query-replace)
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
