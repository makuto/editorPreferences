
;;
;; Macoy-select-search: given a search string, select from many different types of search methods
;;

(setq macoy-select-search-buf-name "*Macoy-Select-Search*")
(setq macoy-select-search-search "")

;; Copied from ag/dwim-at-point in ag.el
(defun macoy-dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

;; Copied from ag/read-from-minibuffer in ag.el
(defun macoy-read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (macoy-dwim-at-point))
         (final-prompt
          (if suggested
              (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt)))
         ;; Ask the user for input, but add `suggested' to the history
         ;; so they can use M-n if they want to modify it.
         (user-input (read-from-minibuffer
                      final-prompt
                      nil nil nil nil suggested)))
    ;; Return the input provided by the user, or use `suggested' if
    ;; the input was empty.
    (if (> (length user-input) 0)
        user-input
      suggested)))

(defun macoy-select-do-search (search-function &rest after-search-args)
  "Using `search-function', execute the search"
  (interactive)
  (kill-buffer macoy-select-search-buf-name)
  (apply search-function macoy-select-search-search after-search-args))

;; TODO: Make generic system (e.g. refresh cache selector)
(setq macoy-select-search-key-descriptions "Select search:
\ta - Ag (in same directory as file, no filter)
\tb - Ag (browse for directory, no filter)
\tf - Ag (browse for directory, with project filter)
\tc - Codesearch (with filter)
\td - Codesearch Data (with filter)
\ts - Swiper search all buffers
\ti - Internet search (DuckDuckGo)")

;; TODO: Add ag no filter and ag filter
(defvar macoy-select-search-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ag with default directory
    (define-key map (kbd "a") (lambda () (interactive) (macoy-select-do-search
                                                        'ag default-directory)))
    ;; Ag with browse directory
    (define-key map (kbd "b") (lambda () (interactive) (macoy-select-do-search
                                                        'ag (read-directory-name "Directory: "))))
    ;; Ag with project filter
    (define-key map (kbd "f") (lambda () (interactive) (macoy-select-do-search
                                                        'macoy-ag-with-project-filter (read-directory-name "Directory: "))))
    ;; Codesearch source with filter (directory defined by `codesearch-dir-to-index', aka use ~/.csearchindex)
    (define-key map (kbd "c") (lambda () (interactive) (macoy-select-do-search
                                                        'macoy-codesearch-search-src)))
    ;; Codesearch data
    (define-key map (kbd "d") (lambda () (interactive) (macoy-select-do-search
                                                        'macoy-codesearch-search-with-filter-directory macoy-codesearch-search-data-dir)))
    ;; Swiper all
    (define-key map (kbd "s") (lambda () (interactive) (macoy-select-do-search
                                                        'swiper-all)))
    ;; Internet search
    (define-key map (kbd "i") (lambda () (interactive) (macoy-select-do-search
                                                        'engine/search-duckduckgo)))

    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer macoy-select-search-buf-name)))
    map)
  "macoy-select-search-minor-mode keymap.")

(define-minor-mode macoy-select-search-minor-mode
  "A minor mode for selecting the search function which should be used."
  :init-value nil
  :lighter " MacoySelectSearch")

(defun macoy-select-search (string)
  (interactive (list (macoy-read-from-minibuffer "Search string")))
  ;; Only search if search string
  (when string
    ;; Hang on to the search string
    (setq macoy-select-search-search string)

    ;; Create a buffer `macoy-select-search-buf-name'
    (get-buffer-create macoy-select-search-buf-name)

    ;; Switch to the `macoy-select-search-buf-name' buffer
    ;; (switch-to-buffer-other-window macoy-select-search-buf-name)
    (switch-to-buffer macoy-select-search-buf-name)

    (insert macoy-select-search-key-descriptions)
    
    ;; Set its major mode to `special-mode'
    (special-mode) ; a read-only major mode
    ;; As you are now in the `macoy-select-search-buf-name' buffer, you can do the below:
    ;; - hit `q' to quit the current window *without* killing `macoy-select-search-buf-name'
    ;; - hit `C-u q' to quit the current window *and* kill `macoy-select-search-buf-name'
    (macoy-select-search-minor-mode 1)))

(global-set-key (kbd "C-e") 'macoy-select-search)

;; Searching in files (Ag)
;; Make ag.el reuse the same *ag* buffer for all your searches:
(setq ag-reuse-buffers 't)
(setq ag-arguments '("--smart-case" "--stats"))

(defun macoy-ag-with-project-filter (&optional pattern directory)
  "Conditionally filter ag results based on whether I know I should filter certain files (i.e. I'm in a project)"
  (interactive)
  (if (projectile-project-p)
      (setq ag-arguments '("--smart-case" "--stats" "-G"
                           "(\\.txt|\\.org|\\.cpp|\\.c|\\.h|\\.inl|\\.html|\\.css|\\.lua|\\.js|\\.py|\\.cdm|\\.el)"
                           "--ignore" ".*AutoGen.*"))
    (setq ag-arguments '("--smart-case" "--stats")))
  (if (and pattern directory)
      (ag pattern directory)
    (call-interactively 'ag))
  ;; Reset the arguments so we don't unintentionally have a filter
  (setq ag-arguments '("--smart-case" "--stats")))

;; (global-set-key (kbd "C-M-f") 'macoy-ag-with-project-filter)
;; (global-set-key (kbd "C-M-F") 'ag)

;; Codesearch: Use a pregenerated index to search files. Requires Codesearch

;; Whether or not files have already been filtered
;; Get this fork for required feature https://github.com/junkblocker/codesearch
;; If you want to use Google's version, set codesearch-filter-supported to nil
;;  and set macoy-codesearch-ignore-lines-pattern
(setq codesearch-filter-supported t)
;;
;; Always rescan all folders when indexing
(if codesearch-filter-supported
    (setq codesearch-cindex-args (list "-reset" "-exclude" "f:/CJUNCTIONS/src/csearchIgnorePatterns"))
  ;; Use if junkblocker codesearch fork is not used
  (setq codesearch-cindex-args (list "-reset")))


;; User-specific settings for codesearch
(when (string-equal (user-login-name) "mmadson")
  (setq codesearch-csearch-exe "c:/Users/mmadson/go/bin/csearch.exe")
  (setq codesearch-cindex-exe "c:/Users/mmadson/go/bin/cindex.exe")
  (setq codesearch-dir-to-index "f:/CJUNCTIONS/src")
  (setq codesearch-temp-file "c:/.temp-codesearch.txt"))
  ;; (setq macoy-codesearch-search-data-dir "D:/Magic/data"))

(when (string-equal (user-login-name) "macoy")
  (setq codesearch-csearch-exe "csearch")
  (setq codesearch-cindex-exe "cindex")
  (setq codesearch-dir-to-index "/home/macoy/Development/code/repositories")
  (setq codesearch-temp-file "~/.temp-codesearch.txt")
  (setq macoy-codesearch-search-data-dir nil))

;; Make it possible to index multiple directories at the same time
(defun macoy-codesearch-name-function (name-of-mode)
  (format "*Codesearch index* %s" compilation-environment))

;; TODO: Rename compilation buffer and add a finished message?
(defun macoy-codesearch-index-directory (directory)
  "Create a .csearchindex for the specified directory"
  (interactive)
  (message "Running Codesearch index on %s" directory)
  (set (make-local-variable 'compilation-environment)
       (list (format "%s%s%s" "CSEARCHINDEX=" directory "/.csearchindex")))
  (let ((compilation-buffer-name-function 'macoy-codesearch-name-function))
    (compile (string-join
              (append (list codesearch-cindex-exe)
                      codesearch-cindex-args
                      (list directory)) " "))))

(defun macoy-codesearch-index-default ()
  (interactive)
  (message "Running Codesearch index on %s" codesearch-dir-to-index)
  ;; Remove the old codesearch because for some reason it doesn't actually update
  ;; This is okay too because having an existing index doesn't speed things up all that much
  ;; Commented because this is no longer needed due to using -reset
  ;; (when (file-exists-p codesearch-index-file)
  ;;    (delete-file codesearch-index-file))
  ;; We won't use this way for now so that we can run two indexes at the same time
  (macoy-codesearch-index-directory codesearch-dir-to-index))
  ;; (let ((codesearch-proc (apply 'start-process
  ;;                               (append (list "CodesearchIndex" "*Codesearch-Index*" codesearch-cindex-exe)
  ;;                                       codesearch-cindex-args
  ;;                                       (list codesearch-dir-to-index)))))
  ;;   (set-process-sentinel codesearch-proc
  ;;                         (lambda (codesearch-proc _string)
  ;;                           (message "Codesearch finished building index")))))

;; TODO: There is no way to undo after doing this; you have to repeat the search
(defun macoy-filter-buffer ()
  "Disable readonly and filter lines using keep-lines"
  (interactive)
  ;; Make sure we get all lines by going to the start of the buffer
  (goto-char (point-min))
  (read-only-mode 0)
  (call-interactively 'keep-lines)
  ;; Start with the cursor on the first result
  (compilation-next-error 1))

;; Refer to ag.el for customization
(define-compilation-mode macoy-codesearch-mode "Codesearch"
  "Codesearch results compilation mode"
  ;; This is so you can delete results with C-S-k. This doesn't break n and p which is cool
  (read-only-mode 0))

(define-key macoy-codesearch-mode-map (kbd "p") #'compilation-previous-error)
(define-key macoy-codesearch-mode-map (kbd "n") #'compilation-next-error)
(define-key macoy-codesearch-mode-map (kbd "f") 'macoy-filter-buffer)

;; The filter which will apply to codesearch results. Things matching this regex will be removed.
;; This is useful for e.g. filtering autogenerated code files
;; Note that this will also remove code lines which match this pattern, so make the regex robust to that
;; Only used if codesearch-filter-supported = nil
(setq macoy-codesearch-ignore-lines-pattern "_ast\.|_autogen")

(defun macoy-codesearch-search-with-filter-directory (pattern directory)
  (interactive (list (macoy-read-from-minibuffer "Search string")))
  ;; Use the compile command so we have nice clickable links
  ;; Note that without regexp-quote, this does support regexes. I don't want them in my case
  ;; Args explanation: -n (Line numbers) -i (ignore case)
  ;; Output Codesearch results to a temporary file, filter out lines, then clean up the temp file
  (if directory
      (set (make-local-variable 'compilation-environment) (list (format "%s%s%s" "CSEARCHINDEX=" directory "/.csearchindex")))
    (set (make-local-variable 'compilation-environment) nil))
  (if codesearch-filter-supported
      (compilation-start (format "%s -n -i \"%s\""
                                 codesearch-csearch-exe
                                 ;; (regexp-quote pattern) ;; This doesn't work because more escape(\) chars are added
                                 pattern)
                         #'macoy-codesearch-mode
                         `(lambda (mode-name) , "*Codesearch*"))
    ;; This temp-file thing sucks but seems necessary for Windows
    (compilation-start (format "%s -n -i \"%s\" > %s && grep -i -E -v \"%s\" %s && rm %s"
                               codesearch-csearch-exe
                               ;; (regexp-quote pattern) ;; This doesn't work because more escape(\) chars are added
                               pattern
                               codesearch-temp-file
                               macoy-codesearch-ignore-lines-pattern
                               codesearch-temp-file
                               codesearch-temp-file)
                       #'macoy-codesearch-mode
                       `(lambda (mode-name) , "*Codesearch*"))))

(defun macoy-codesearch-search-src (pattern)
  (interactive (list (macoy-read-from-minibuffer "Search string")))
  (macoy-codesearch-search-with-filter-directory pattern codesearch-dir-to-index))

(global-set-key (kbd "C-S-f") 'macoy-codesearch-search-src)

;; Isearch customizations
(defun macoy-isearch-yank-clipboard ()
  "Insert the contents of the clipboard into isearch. We do this because we don't use the yank stuff"
  (interactive)
  (isearch-yank-string (simpleclip-get-contents)))

(define-key isearch-mode-map (kbd "C-v") 'macoy-isearch-yank-clipboard)
;; Go to next/previous result with arrow keys
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
;; Used for expanding the search by words
(define-key isearch-mode-map (kbd "C-'") 'isearch-yank-word-or-char)

;; Make it easier to just see all results when there are too many to scroll
(defun macoy-isearch-occur ()
  (interactive)
  ;; Get all results
  (call-interactively 'isearch-occur)
  ;; End the search
  (isearch-done)
  ;; Select the results list buffer
  (switch-to-buffer-other-window "*Occur*"))
(define-key isearch-mode-map (kbd "C-a") 'macoy-isearch-occur)

;; We're going to use smooth scrolling only for isearch so there's a good margin between a search
;; result which would've ended up at the very bottom of the window without smooth scrolling
(require 'smooth-scrolling)
(defun macoy-isearch-end-hook ()
  (smooth-scrolling-mode 0))
(setq isearch-mode-end-hook 'macoy-isearch-end-hook)

;; If marked, use swiper to search mark
(defun macoy-isearch-search-mark ()
  "If marked, use isearch to search mark. Otherwise, isearch normally"
  (interactive)
  ;; Make sure we can see several lines below a result near the bottom
  ;; This is then disabled in macoy-isearch-end-hook
  (smooth-scrolling-mode 1)
  
  (call-interactively 'isearch-forward)
  (when (use-region-p)
    ;; (isearch-search)
    ;; (call-interactively 'isearch-forward)
    (isearch-yank-string (buffer-substring (region-beginning) (region-end)))
    ;; Clear the mark for two reasons:
    ;; 1. Occur will only search the region, but whenever I use occur I expect
    ;;    it to show me all results in the entire file
    ;; 2. I don't like how it looks visually when the mark is expanding while I'm going
    ;;    through results.
    ;; I never use isearch to change my region, so this seemed like an acceptable tradeoff
    (setq mark-active nil)))

(global-set-key (kbd "C-f") 'macoy-isearch-search-mark)

;; Swiper customizations
;; (when (require 'swiper)
;;   ;; If marked, use swiper to search mark
;;   (defun macoy-swiper-search-mark ()
;;     "If marked, use swiper to search mark. Otherwise, open swiper normally"
;;     (interactive)
;;     ;; This isn't sufficient for my purposes; it's nice to search e.g. thing->thing
;;     ;;(swiper (thing-at-point 'symbol))
;;     (if (use-region-p)
;;         (swiper (buffer-substring (region-beginning) (region-end)))
;;       (swiper)))

;;   ;; If marked, use swiper to search mark
;;   (defun macoy-swiper-all-search-mark ()
;;     "If marked, use swiper-all to search mark. Otherwise, open swiper-all normally"
;;     (interactive)
;;     ;; This isn't sufficient for my purposes; it's nice to search e.g. thing->thing
;;     ;;(swiper (thing-at-point 'symbol))
;;     (if (use-region-p)
;;         (swiper-all (buffer-substring (region-beginning) (region-end)))
;;       (swiper-all)))

;;   ;; Use swiper for search instead of isearch (use e.g. space to fuzzy search)
;;   ;; (global-set-key (kbd "C-f") 'macoy-swiper-search-mark)
;;   (global-set-key (kbd "M-f") 'macoy-swiper-all-search-mark))

;; Quickly search the web
;; See https://github.com/hrs/engine-mode for more browsers
(when (require 'engine-mode)
  ;; (engine-mode t)

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (global-set-key (kbd "M-l") 'engine/search-duckduckgo))
