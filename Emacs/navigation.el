
;; Custom place saving (to get back to where you where after a jump or something)

;; Store what we last did for the toggle
(setq macoy-place-last-operation "load")

;; Quick save place and jump back to it
(defun macoy-save-place ()
  "Store the current position in a register"
  (interactive)
  (point-to-register ?=)
  (setq macoy-place-last-operation "save")
  (message "Point saved")
  )

(defun macoy-load-place ()
  "Go to the last position saved in the macoy register"
  (interactive)
  (jump-to-register ?=)
  (setq macoy-place-last-operation "load")
  (message "Point loaded")
  )

(defun macoy-save-place-then-call (function-to-call)
  "Save the place then call function-to-call"
  (interactive)
  (call-interactively 'macoy-save-place)
  (call-interactively function-to-call)
  )

(defun macoy-save-or-load-place ()
  "Either save or load the place, depending on what was done previously"
  (interactive)
  (if (string-equal macoy-place-last-operation "load")
	  (call-interactively 'macoy-save-place)
	(call-interactively 'macoy-load-place)
	)
  )
(global-set-key (kbd "C-=") 'macoy-save-or-load-place)
(global-set-key (kbd "C-+") 'macoy-save-place)

;; Quick jumping. Ace-jump-mode didn't work for me
;; Hit keybind, then type first letter of word. Type letter for correct word to jump to
(when (require 'avy)
  (global-set-key (kbd "C-S-g") (lambda () (interactive) (macoy-save-place-then-call
														  'avy-goto-line)))
  (global-set-key (kbd "C-S-j") (lambda () (interactive) (macoy-save-place-then-call
														  'avy-goto-char)))
  (global-set-key (kbd "C-j") (lambda () (interactive) (macoy-save-place-then-call
														'avy-goto-word-or-subword-1)))

  (defun macoy-quick-jump-copy-paste ()
	"Use Avy to jump to a position, select something, then jump back and paste it"
	(interactive)
	(setq paste-point (point))
	(setq copied-str "")
	(save-excursion
	  (call-interactively 'avy-goto-word-or-subword-1)
	  ;; TODO: Push a mode which lets the user select until they hit enter
	  (setq copy-start (point))
	  (right-word)
	  (setq copied-str (buffer-substring copy-start (point)))
	  )
	(goto-char paste-point)
	(insert copied-str)
	)

  (global-set-key (kbd "M-c") 'macoy-quick-jump-copy-paste)

  (setq macoy-avy-workman nil)
  (defun macoy-toggle-avy-keys ()
	"Toggle avy prompts for layout"
	(interactive)
	(setq macoy-avy-workman (not macoy-avy-workman))
	(if macoy-avy-workman
		(setq avy-keys '(?a ?s ?h ?t ?n ?e ?o ?i ?g))
	  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))
  )

;; Go to char. This is like avy quick jump but instead just goes to the next one, not any onscreen
(when (require 'iy-go-to-char)
   (define-key iy-go-to-char-keymap (kbd "C-g") 'iy-go-to-char-done)
  (defun macoy-iy-go-to-char-regular-mode ()
	(interactive)
	(global-set-key (kbd "<left>") 'left-char)
	(global-set-key (kbd "<right>") 'right-char)
	(global-set-key (kbd "C-n") 'iy-go-to-char)
	(global-set-key (kbd "C-S-n") 'iy-go-to-char-backward))
  (macoy-iy-go-to-char-regular-mode)
  ;;
  ;; Extreme keymappings
  ;; Keys which shouldn't be for the general user because they're confusing
  (defun macoy-iy-go-to-char-extreme-mode ()
	(interactive)
	(global-set-key (kbd "<left>") 'iy-go-to-char-backward)
	(global-set-key (kbd "<right>") 'iy-go-to-char)
	(defun macoy-iy-go-to-char-left-done ()
	  (interactive)
	  (iy-go-to-char-done)
	  (left-char))
	(defun macoy-iy-go-to-char-right-done ()
	  (interactive)
	  (iy-go-to-char-done)
	  (right-char))
	(define-key iy-go-to-char-keymap (kbd "<left>") 'macoy-iy-go-to-char-left-done)
	(define-key iy-go-to-char-keymap (kbd "<right>") 'macoy-iy-go-to-char-right-done)
	(global-set-key (kbd "C-n") 'right-char)
	(global-set-key (kbd "C-S-n") 'left-char))
  
  ;; (when (string-equal (user-login-name) "mmadson")
	;; (macoy-iy-go-to-char-extreme-mode))
  )
