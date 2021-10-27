
;;
;; Source control
;;
(setq tortoise-svn-repository-path "D:/src")

(defun macoy-unix-path-to-windows-path (str)
  (replace-regexp-in-string "/" "\\\\" str))

(defun macoy-tortoise-svn-make-path (str)
  (format "/path:%s" (macoy-unix-path-to-windows-path str)))

(defun tortoise-svn-check-for-modifications-src ()
  "Open the TortoiseSVN Check for Modifications window"
  (interactive)
  (message "Checking for modifications")
  (start-process "CheckForModifications" nil
				 "tortoiseproc" "/command:repostatus"
                 (macoy-tortoise-svn-make-path tortoise-svn-repository-path)))

(defun tortoise-svn-show-log-src ()
  "Open the TortoliseSVN Log window"
  (interactive)
  (message "SVN Log")
  (start-process "SVNLog" nil
				 "tortoiseproc" "/command:log"
                 (macoy-tortoise-svn-make-path tortoise-svn-repository-path)))

(defun tortoise-svn-update-src ()
  "Open the TortoiseSVN Update window"
  (interactive)
  (message "SVN Update")
  (start-process "SVNUpdate" nil
				 "tortoiseproc" "/command:update"
                 (macoy-tortoise-svn-make-path tortoise-svn-repository-path)))

(defun tortoise-svn-create-patch-src ()
  "Open the TortoiseSVN Create Patch window"
  (interactive)
  (message "SVN Create Patch")
  (start-process "SVNCreatePatch" nil
				 "tortoiseproc" "/command:createpatch" "/noview"
                 (macoy-tortoise-svn-make-path tortoise-svn-repository-path)))

;; dsvn SVN frontend
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

(defun macoy-svn-status ()
  "Run svn-status on the current projectile-root"
  (interactive)
  (if (projectile-project-p)
	  (svn-status (projectile-project-root))
	(call-interactively 'svn-status)))

(global-set-key (kbd "<f6>") 'magit)

(global-set-key (kbd "S-<f6>") 'vc-print-root-log)

(setq macoy-commit-message-backup "~/Macoy_Emacs_CommitMessage_Backup.txt")
;; SVN and Magit commit message finished
(defun macoy-commit-message-done ()
  "Save a copy of the commit message then run log-edit-done (dsvn) or with-editor-finish (magit)"
  (interactive)
  ;; Save a backup of the message in case something goes wrong
  (mark-whole-buffer)
  (write-region (region-beginning) (region-end) macoy-commit-message-backup t)

  ;; magit
  (when (or (string-equal (buffer-name) "COMMIT_EDITMSG")
            (string-equal (buffer-name) "MERGE_MSG"))
	(call-interactively 'with-editor-finish))

  ;; dsvn
  (when (derived-mode-p 'log-edit-mode)
	(call-interactively 'log-edit-done))

  (when (derived-mode-p 'p4-form-mode)
	(call-interactively 'p4-form-commit)))

(defun macoy-dsvn-diff-marked ()
  (interactive)
  (svn-diff-file t))
