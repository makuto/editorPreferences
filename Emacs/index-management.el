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
  (setq macoy-active-data-dir-escaped (macoy-unix-path-to-windows-path macoy-active-data-dir))
  ;; Narrow the search a bit
  (setq macoy-active-data-search-dir (format "%s/data" macoy-active-data-dir))
  (setq codesearch-dir-to-index macoy-active-code-dir)
  ;; TODO Make this an add-to-list conditionally on exclude
  (setq codesearch-cindex-args
        (list "-reset" "-exclude" (format "%s/csearchIgnorePatterns" macoy-active-code-dir)))
  (setq macoy-codesearch-search-data-dir macoy-active-data-search-dir))
