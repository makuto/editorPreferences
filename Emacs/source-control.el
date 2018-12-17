
;;
;; Source control
;;

(defun tortoise-svn-check-for-modifications-src ()
  "Open the TortoiseSVN Check for Modifications window"
  (interactive)
  (message "Checking for modifications")
  (start-process "CheckForModifications" nil
				 "tortoiseproc" "/command:repostatus" "/path:F:\\CJUNCTIONS\\src"))

(defun tortoise-svn-show-log-src ()
  "Open the TortoiseSVN Log window"
  (interactive)
  (message "SVN Log")
  (start-process "SVNLog" nil
				 "tortoiseproc" "/command:log" "/path:F:\\CJUNCTIONS\\src"))

(defun tortoise-svn-update-src ()
  "Open the TortoiseSVN Update window"
  (interactive)
  (message "SVN Update")
  (start-process "SVNUpdate" nil
				 "tortoiseproc" "/command:update" "/path:F:\\CJUNCTIONS\\src"))

(defun tortoise-svn-create-patch-src ()
  "Open the TortoiseSVN Create Patch window"
  (interactive)
  (message "SVN Create Patch")
  (start-process "SVNCreatePatch" nil
				 "tortoiseproc" "/command:createpatch" "/noview" "/path:F:\\CJUNCTIONS\\src"))

;; dsvn SVN frontend
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

(defun macoy-svn-status ()
  "Run svn-status on the current projectile-root"
  (interactive)
  (if (projectile-project-p)
	  (svn-status (projectile-project-root))
	(call-interactively 'svn-status)
	)
  )

(global-set-key (kbd "<f6>") 'macoy-svn-status)

(setq macoy-commit-message-backup "~/Macoy_Emacs_CommitMessage_Backup.txt")
;; SVN and Magit commit message finished
(defun macoy-commit-message-done ()
  "Save a copy of the commit message then run log-edit-done (dsvn) or with-editor-finish (magit)"
  (interactive)
  ;; Save a backup of the message in case something goes wrong
  (mark-whole-buffer)
  (write-region (region-beginning) (region-end) macoy-commit-message-backup t)

  ;; magit
  (when (string-equal (buffer-name) "COMMIT_EDITMSG")
	(call-interactively 'with-editor-finish)
	)
  
  ;; dsvn
  (when (derived-mode-p 'log-edit-mode)
	(call-interactively 'log-edit-done)
	)
  )
