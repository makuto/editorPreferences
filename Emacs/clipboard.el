;;
;; Custom multiple cursors cut/copy/paste handling
;;
(when (and (require 'simpleclip) (require 'multiple-cursors))
  ;; To replicate Sublime behavior, if copy or cut is executed, do the operation for the whole line.
  ;; When pasting, paste that line above the current line, regardless of point position
  (setq macoy-clipboard-no-selection-line-paste nil)
  (setq macoy-multiple-cursors-buffers nil)
  (setq macoy-mc-buffer-index 0)

  ;;
  ;; Internal, per-cursor cxp functions for multiple-cursors
  ;;

  (defun macoy-mc-copy ()
	(interactive)
	(if (use-region-p)
		(progn (push (buffer-substring (region-beginning) (region-end)) macoy-multiple-cursors-buffers)
			   (setq macoy-clipboard-no-selection-line-paste nil))
	  ;; Nothing marked? Copy whole line
	  (progn
		(push (buffer-substring (line-beginning-position) (line-end-position)) macoy-multiple-cursors-buffers)
		(setq macoy-clipboard-no-selection-line-paste t))))

  (defun macoy-mc-cut ()
	(interactive)
	(unless (use-region-p)
	  (push (buffer-substring (line-beginning-position) (line-end-position)) macoy-multiple-cursors-buffers)
	  ;; (kill-region (line-beginning-position) (line-end-position))
	  (kill-whole-line)
	  (setq macoy-clipboard-no-selection-line-paste t))

	(when (use-region-p)
	  (push (buffer-substring (region-beginning) (region-end)) macoy-multiple-cursors-buffers)
	  (kill-region (region-beginning) (region-end))
	  (setq macoy-clipboard-no-selection-line-paste nil)))

  ;; Respect macoy-clipboard-no-selection-line-paste
  (defun macoy-simpleclip-paste ()
	(interactive)
	(if macoy-clipboard-no-selection-line-paste
		(save-excursion		  
		  (progn
			(back-to-indentation)
			(newline)
			(call-interactively 'indent-for-tab-command)
			(previous-line)
			(call-interactively 'simpleclip-paste)
			(call-interactively 'indent-for-tab-command)))
	  (call-interactively 'simpleclip-paste)))

  (defun macoy-mc-paste ()
	(interactive)
	;; Delete selected text before insert if necessary
	(when (use-region-p)
	  (delete-region (region-beginning) (region-end)))
	;; If no macoy-multiple-cursors-buffers the user probably did a simple copy so paste that
	(unless macoy-multiple-cursors-buffers
	  (call-interactively 'macoy-simpleclip-paste))
	(when macoy-multiple-cursors-buffers
	  (save-excursion
		(if macoy-clipboard-no-selection-line-paste
			(progn
			  (back-to-indentation)
			  (newline)
			  (call-interactively 'indent-for-tab-command)
			  (previous-line)
			  (insert (nth macoy-mc-buffer-index macoy-multiple-cursors-buffers))
			  (call-interactively 'indent-for-tab-command))
		  (insert (nth macoy-mc-buffer-index macoy-multiple-cursors-buffers))))
	  ;; Set up next cursor buffer index
	  ;; Ensure we don't go out of range of the buffers
	  ;; Sublime's behavior is to just paste all buffers at all marks, so our solution is different here
	  (setq macoy-mc-buffer-index (min
								   (+ macoy-mc-buffer-index 1)
								   (- (length macoy-multiple-cursors-buffers) 1)))))

  ;;
  ;; Cxp functions at all cursors (uses internal functions)
  ;;

  ;; For versions newer than 25.3 or something :(
  (defun string-join (sl delim)
	(mapconcat 'identity sl delim))

  (defun macoy-multiple-cursors-copy()
	"Copy at multiple cursors using `macoy-multiple-cursors-buffers'"
	(interactive)
	(setq macoy-multiple-cursors-buffers nil)
	(mc/for-each-cursor-ordered
	 (mc/execute-command-for-fake-cursor 'macoy-mc-copy cursor))
	;; Append happens in reverse
	(setq macoy-multiple-cursors-buffers (reverse macoy-multiple-cursors-buffers))
	;; Adding newline isn't correct but emacs won't copy the newline. It is slightly more useful
	;;  to paste things with the newlines when collapsing multiple selections
	(simpleclip-set-contents (string-join macoy-multiple-cursors-buffers "\n")))

  (defun macoy-multiple-cursors-cut()
	"Cut at multiple cursors using `macoy-multiple-cursors-buffers'"
	(interactive)
	(setq macoy-multiple-cursors-buffers nil)
	(mc/for-each-cursor-ordered
	 (mc/execute-command-for-fake-cursor 'macoy-mc-cut cursor))
	(setq macoy-multiple-cursors-buffers (reverse macoy-multiple-cursors-buffers))
	;; Adding newline isn't correct but emacs won't copy the newline. It is slightly more useful
	;;  to paste things with the newlines when collapsing multiple selections
	(simpleclip-set-contents (string-join macoy-multiple-cursors-buffers "\n")))

  (defun macoy-multiple-cursors-paste()
	"Paste at multiple cursors using `macoy-multiple-cursors-buffers'"
	(interactive)
	(setq macoy-mc-buffer-index 0)
	(mc/for-each-cursor-ordered
	 (mc/execute-command-for-fake-cursor 'macoy-mc-paste cursor)))

  (defun macoy-test-multiple-cursors-print-list()
	"Print buffers"
	(interactive)
	(message "%d in buffer" (length macoy-multiple-cursors-buffers))
	(dolist (buffer macoy-multiple-cursors-buffers)
	  (message "Buffer: %s" buffer)))

  ;;
  ;; Internal, non-mc cx functions (to duplicate no region line copy behavior)
  ;;

  (defun macoy-nonmc-copy ()
	(interactive)
	(if (use-region-p)
		(progn (simpleclip-set-contents (buffer-substring (region-beginning) (region-end)))
			   (setq macoy-clipboard-no-selection-line-paste nil))
	  ;; Nothing marked? Copy whole line
	  (progn (simpleclip-set-contents
			  (buffer-substring (line-beginning-position)
								(line-end-position)))
			 (setq macoy-clipboard-no-selection-line-paste t))))

  (defun macoy-nonmc-cut ()
	(interactive)
	(unless (use-region-p)
	  (simpleclip-set-contents (buffer-substring (line-beginning-position) (line-end-position)))
	  ;; (kill-region (line-beginning-position) (line-end-position))
	  (kill-whole-line)
	  (setq macoy-clipboard-no-selection-line-paste t))
	(when (use-region-p)
	  (simpleclip-cut (region-beginning) (region-end))
	  (setq macoy-clipboard-no-selection-line-paste nil)))

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
	  (call-interactively 'macoy-nonmc-copy))
	(when macoy-clipboard-no-selection-line-paste
	  (setq macoy-clipboard-no-selection-line-paste (simpleclip-get-contents))))

  (defun macoyCut ()
	(interactive)
	;; Clear buffers here in case they aren't using multiple cursors
	;; Then, if they paste in multiple-cursors-mode it will paste simpleclip
	(setq macoy-multiple-cursors-buffers nil)
	(if (bound-and-true-p multiple-cursors-mode)
		(call-interactively 'macoy-multiple-cursors-cut) ;; Was kill-region
	  (call-interactively 'macoy-nonmc-cut))
	(when macoy-clipboard-no-selection-line-paste
	  (setq macoy-clipboard-no-selection-line-paste (simpleclip-get-contents))))

  (defun macoyPaste ()
	(interactive)
	;; Something was put in the clipboard that wasn't a "No Selection" copy/cut (e.g. from an
	;; external program). Clear it so that it behaves normally
	(when (and macoy-clipboard-no-selection-line-paste
			   (not (string-equal (simpleclip-get-contents)
								  macoy-clipboard-no-selection-line-paste)))
	  (setq macoy-clipboard-no-selection-line-paste nil))

	(if (bound-and-true-p multiple-cursors-mode)
		(call-interactively 'macoy-multiple-cursors-paste) ;; Was yank
	  (call-interactively 'macoy-simpleclip-paste)))
  )
