;; Macoy's hacky setup for using Org-drill

;; Org drill
(load-user-file "org-learn.el")
(load-user-file "org-drill.el")

(setq macoy-org-drill-file "~/Dropbox/Org/Japanese/macoy-drill.org")

(defun macoy-org-drill-convert-subtree-to-twosided ()
  (interactive)

  ;; Convert every item to whatever the subtree requested
  (while (outline-next-heading)
	;; Standard vocab card; convert to org-drill double-sided format
	(org-insert-heading)
	(org-set-property "DRILL_CARD_TYPE" "twosided")
	(insert "w :drill:")
	(outline-next-heading)
	(org-demote-subtree)
	(skip-chars-forward "\* ")
	(insert "a\n")
	(next-line)
	(org-insert-heading)
	(insert "b\n")
	)
  )

;; I used org-drill-copy-entry-to-other-buffer as a reference
(defun macoy-org-drill-add-subtree ()
  "Copies org subtree at point to `macoy-org-drill-file` and converts
 it to the appropriate org-drill format based on the subtree's tags."
  (interactive)
  (let ((subtree-tags (org-get-local-tags)))
	(org-copy-subtree)
	
	(switch-to-buffer "*Macoy-Drill-Convert*")
	(with-current-buffer "*Macoy-Drill-Convert*"
	  (org-mode)
	  ;; Override the level because we don't care about levels in drill,
	  ;; but want to see them in non-drill notes
	  (org-paste-subtree 1)
	  (macoy-org-drill-convert-subtree-to-twosided)
	  (append-to-file nil nil macoy-org-drill-file)
	  )
	
	
	;; (org-toggle-tag "drill" 'on)
	
	;; ;; Convert every item to whatever the subtree requested
	;; (while (outline-next-heading)
	;;   ;; Standard vocab card; convert to org-drill double-sided format
	;;   (when (member "vocab" subtree-tags)
	;; 	;; (org-toggle-tag "vocab" 'on)
	;; 	(org-insert-subheading
	;; 	)
	;;   )
	;; )
	
	)
  )

;; (defun macoy-test-org-element ()
;;   (interactive)
;;   ;; ;; (message "%s" (org-element-parse-buffer))
;;   ;; (message "%s" (org-element-interpret-data
;;   ;; 	 (org-element-map (org-element-parse-buffer) 'item
;;   ;; 	   (lambda (item)
;;   ;; 		 ;; (when (member "vocab" (org-element-property :tags item))
;;   ;; 		 ;;   item
;;   ;; 		 ;;   )
;;   ;; 		 (message "whahhahta")
;;   ;; 		 item
;;   ;; 		 )
;;   ;; 	   ))
;;   ;; 	 )
;;   (org-copy-subtree)
;;   (message "%s" (org-element-map
;; 					(with-temp-buffer
;; 					  (org-paste-subtree 0)
;; 					  (org-element-parse-buffer))
;; 					'(headline section)
;; 				  'identity))
;;   (message "interpreted: %s" (org-element-interpret-data (org-element-map
;; 															 (with-temp-buffer
;; 															   (insert (and kill-ring (current-kill 0)))
;; 															   (org-element-parse-buffer))
;; 															 '(headline section)
;; 														   'identity)))
;;   (message "clipboard: %s" (setq tree (and kill-ring (current-kill 0))))
  
;;   ;; This is the correct way it looks like, although org-paste-subtree 0 should be used instead of the kill-ring stuff I think
;;   ;; http://ergoemacs.org/emacs/elisp_parse_org_mode.html
;;   ;; How do I edit??
;;   (message "just parse: %s" (with-temp-buffer
;; 							  (insert (and kill-ring (current-kill 0)))
;; 							  (org-element-interpret-data (org-element-parse-buffer))))
;;   )
