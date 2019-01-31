;;
;; Custom multiple cursors cut/copy/paste handling
;;
(when (and (require 'simpleclip) (require 'multiple-cursors))

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
	(simpleclip-set-contents (string-join macoy-multiple-cursors-buffers "\n"))
	)

  (defun macoy-multiple-cursors-cut()
	"Cut at multiple cursors using `macoy-multiple-cursors-buffers'"
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
	"Paste at multiple cursors using `macoy-multiple-cursors-buffers'"
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
  )
