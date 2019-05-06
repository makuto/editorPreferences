
;;
;; JIRA
;;
;; Then start syncing with M-x org-jira-get-issues.
;; Goes to ~/.org-jira by default
(when (require 'org-jira)
  (setq jiralib-url "http://jira:8080")
  
  ; Debug commands
  ;; (setq org-jira-verbosity 'debug)
  ;; (setq request-log-level 'debug)
  ;;(setq request-log-level 'warn)
;; (setq request-message-level 'debug)
  ;; (setq request-message-level 'warn)
  
  ;; (setq org-jira-working-dir "~/.org-jira")
  ;; Not sure why, but org-jira has trouble making this dir on its own
  (files--ensure-directory org-jira-working-dir)

;; Overload this function to work with lower permissions (i.e. not set reporter)
(defun org-jira-update-issue-details (issue-id filename &rest rest)
  "Update the details of issue ISSUE-ID in FILENAME.  REST will contain optional input."
  (ensure-on-issue-id-with-filename issue-id filename
    ;; Set up a bunch of values from the org content
    (let* ((org-issue-components (org-jira-get-issue-val-from-org 'components))
           (org-issue-description (s-trim (org-jira-get-issue-val-from-org 'description)))
           (org-issue-priority (org-jira-get-issue-val-from-org 'priority))
           (org-issue-type (org-jira-get-issue-val-from-org 'type))
           (org-issue-assignee (cl-getf rest :assignee (org-jira-get-issue-val-from-org 'assignee)))
           ;; (org-issue-reporter (cl-getf rest :reporter (org-jira-get-issue-val-from-org 'reporter)))
           (project (replace-regexp-in-string "-[0-9]+" "" issue-id))
           (project-components (jiralib-get-components project)))

      ;; Lets fire off a worklog update async with the main issue
      ;; update, why not?  This is better to fire first, because it
      ;; doesn't auto-refresh any areas, while the end of the main
      ;; update does a callback that reloads the worklog entries (so,
      ;; we hope that wont occur until after this successfully syncs
      ;; up).  Only do this sync if the user defcustom defines it as such.
      (when org-jira-worklog-sync-p
        (org-jira-update-worklogs-from-org-clocks))

      ;; Send the update to jira
      (let ((update-fields
             (list (cons
                    'components
                    (or (org-jira-build-components-list
                         project-components
                         org-issue-components) []))
                   (cons 'priority (org-jira-get-id-name-alist org-issue-priority
                                                       (jiralib-get-priorities)))
                   (cons 'description org-issue-description)
                   (cons 'assignee (jiralib-get-user org-issue-assignee))
                   ;; (cons 'reporter (jiralib-get-user org-issue-reporter))
                   (cons 'summary (org-jira-strip-priority-tags (org-jira-get-issue-val-from-org 'summary)))
                   (cons 'issuetype (org-jira-get-id-name-alist org-issue-type
                                                        (jiralib-get-issue-types))))))

        ;; If we enable duedate sync and we have a deadline present
        (when (and org-jira-deadline-duedate-sync-p
                   (org-jira-get-issue-val-from-org 'deadline))
          (setq update-fields
                (append update-fields
                        (list (cons 'duedate (org-jira-get-issue-val-from-org 'deadline))))))

        ;; TODO: We need some way to handle things like assignee setting
        ;; and refreshing the proper issue in the proper buffer/filename.
        (jiralib-update-issue
         issue-id
         update-fields
         ;; This callback occurs on success
         (org-jira-with-callback
           (message (format "Issue '%s' updated!" issue-id))
           (jiralib-get-issue
            issue-id
            (org-jira-with-callback
              (org-jira-log "Update get issue for refresh callback hit.")
              (-> cb-data list org-jira-get-issues))))
         ))
      )))
)
