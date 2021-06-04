;; Turns "D:/srcTip" into "/d/srcTip"
(defun macoy-windows-path-to-gitbash-path (path)
  (format "/%s" (replace-regexp-in-string ":/" "/" path)))

;; Index management: determine which index files to use for things like tags, codesearch, etc.
(defun macoy-set-index-directories (&optional code-dir data-dir)
  "Change which directories Codesearch, tags, and other utilities create their indexes for.
Note that this does not perform any indexing."
  (interactive)
  (setq macoy-active-code-dir
        (if code-dir
            code-dir
          ;; Strip the trailing '/'
          (substring (read-directory-name
                      (format "Code index directory (was %s): " macoy-active-code-dir)) 0 -1)))
  (setq macoy-active-data-dir
        (if data-dir
            data-dir
          ;; Strip the trailing '/'
          (substring (read-directory-name
                      (format "Data index directory (was %s): " macoy-active-data-dir)) 0 -1)))

  ;; Follow up by updating things which rely on this statically
  (setq tortoise-svn-repository-path macoy-active-code-dir)
  (setq macoy-active-code-dir-gitbash (macoy-windows-path-to-gitbash-path macoy-active-code-dir))
  (setq macoy-active-code-dir-escaped (macoy-unix-path-to-windows-path macoy-active-code-dir))
  (setq macoy-active-data-dir-escaped (macoy-unix-path-to-windows-path macoy-active-data-dir))
  ;; Narrow the search a bit
  (setq macoy-active-data-search-dir (format "%s/data" macoy-active-data-dir))
  (setq codesearch-dir-to-index macoy-active-code-dir)
  ;; TODO Make this an add-to-list conditionally on exclude
  (setq codesearch-cindex-args
        (list "-reset" "-exclude" (format "%s/csearchIgnorePatterns" macoy-active-code-dir)))
  (setq macoy-codesearch-search-data-dir macoy-active-data-search-dir)
  (message "Set to %s (code) and %s (data). No indexing performed"
           macoy-active-code-dir macoy-active-data-dir))


;;
;; Switching between established branches
;;

(setq macoy-index-branch-list (list
							   '("My example branch" (list "D:/MyCodeBranch" "D:/MyDataBranch"))
                               '("My example branch 2" (list "D:/MyCodeBranch2" "D:/MyDataBranch2"))
							   ))

(defun macoy-index-branch-select ()
  "Select collections of index directories using Ido"
  (interactive)
;; Use Ido to pick the build system
  (let ((index-branch-ido-list nil) (selected-index-branch nil))
	;; Build a list of only the names of build systems
	(dolist (index-branch macoy-index-branch-list index-branch-ido-list)
	  (add-to-list 'index-branch-ido-list (car index-branch)))
	;; Let the user select the build system using Ido
	(setq selected-index-branch (ido-completing-read "Branch set: " index-branch-ido-list))
	(dolist (index-branch macoy-index-branch-list)
	  (when (string-equal selected-index-branch (car index-branch))
        ;; There's probably a more idiomatic way to do this
		(macoy-set-index-directories
                 (nth 1 (nth 1 index-branch))
                 (nth 2 (nth 1 index-branch)))))))

(defun macoy-print-active-branch ()
  (interactive)
  (message (concat
            "Code:         %s\n"
            "Data:         %s\n"
            "Build system: %s\n")
           macoy-active-code-dir macoy-active-data-dir (nth 0 macoy-build-system-default) ))
