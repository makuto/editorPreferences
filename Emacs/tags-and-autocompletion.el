;; Imenu Ido interface (browse symbols in file)
(load-user-file "idomenu.el")

;; Templates/Snippets
(yas-global-mode 1)

;; Don't prompt me to load tags
(setq tags-revert-without-query 1)

;; Refresh and load tags
;; TODO: Use projectile refresh ctags instead
(if (eq system-type 'gnu/linux)
	(setq ctags-path "ctags")
  (setq ctags-path "C:/programsMacoy/ctags58/ctags.exe")
  )

(defun generateTags ()
  "Create tags file"
  ;; Doesn't do anything for start-process
  ;;(let ((default-directory "F:/CJUNCTIONS/src/")))
  (message "Running CTags")
  (let ((ctagsProc (start-process "CTags" "*CTags-out*" ctags-path "-e" "-f"
				 ;; Output location
				 (concat (projectile-project-root) "TAGS")

				 ;; Additional arguments
				 "--verbose" "--recurse=yes" "--languages=C,C++,Python"
				 
				 ;; Annoyingly there doesn't seem to be wildcard matching for folders (at least
				 ;;  not on Windows)
				 "--exclude=/home/macoy/Development/code/3rdParty/repositories/blender/doc"

				 ;; Includes
				 (projectile-project-root) ;; HOME_ONLY
				 ;; "F:/CJUNCTIONS/src/Core"
				 )))
	(set-process-sentinel ctagsProc
						  (lambda (ctagsProc _string)
							(call-interactively 'macoy-ido-find-tag-refresh))))
)

(defun loadTagsFromParent ()
  (let ((my-tags-file (locate-dominating-file default-directory "TAGS")))
    (when my-tags-file
      (message "Loading tags file: %s" my-tags-file)
      (visit-tags-table my-tags-file)))
  )

;; Use Ivy to select xref results
(require 'ivy-xref)
(setq xref-show-xrefs-function 'ivy-xref-show-xrefs)

;; This isn't really necessary because attempting a goto definition will automatically do this
;;(global-set-key (kbd "C-<f5>") (lambda () (interactive) (loadTagsFromParent)))
(global-set-key (kbd "C-<f5>")
				(lambda ()
				  (interactive)
				  ;; Note that these are both subprocesses so they will run at the same time
				  (generateTags)
				  (macoy-codesearch-index-default)))

;; Tags keybinding
(global-set-key (kbd "<f12>") 'xref-find-definitions)
(global-set-key (kbd "M-g") 'xref-find-definitions-other-window)
(global-set-key (kbd "S-<f12>") 'xref-find-apropos)
(global-set-key (kbd "C-S-d") 'macoy-ido-find-tag)

;; Auto-complete
;; This will at least work for local completions
(global-auto-complete-mode)
;; Don't start auto-completion until three characters have been typed
;; Performance becomes problematic with as many tags as I have so this is necessary
;; See https://github.com/auto-complete/auto-complete/blob/master/doc/manual.md
(setq ac-auto-start 3)

;; Custom fuzzy completion stuff
(defun macoy-ido-example ()
  "Test ido custom"
  (interactive)
  (setq mylist (list "red" "blue" "yellow" "clear" "i-dont-know"))
  (ido-completing-read "What, ... is your favorite color? " mylist))

;; Fuzzy find tag like Sublime's C-S-r
;; Also used for auto-completion
;; From https://www.emacswiki.org/emacs/InteractivelyDoThings#CompleteFindTagUsingIdo
(setq macoy-tag-names (list "empty (run macoy-ido-find-tag-refresh"))
(defun macoy-ido-find-tag-refresh ()
  "Refresh ido tag list"
  (interactive)
  (message "Refreshing tags table")
  ;; tags-completion-table() early-outs if the table has already been created
  ;; This is problematic if TAGS has changed
  ;; Clearing it here ensures the table won't get out of sync
  (setq tags-completion-table nil)
  (tags-completion-table)
  
  (message "Refreshing ido tags list")
  ;; Reset to remove "empty" value as well as avoid duplicates
  (setq macoy-tag-names nil)
  (mapcar (lambda (x)
			(push (prin1-to-string x t) macoy-tag-names))
		  tags-completion-table)
  (message "Refreshing ido tags list done"))

(defun macoy-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (xref-find-definitions (ido-completing-read "Tag: " macoy-tag-names)))

;; This doesn't actually help that much
(defun macoy-ido-find-tag-default-text (start-string)
  "Find a tag using ido"
  (interactive "sTag: ")
  (xref-find-definitions (ido-completing-read "Tag: " macoy-tag-names nil nil start-string)))


;; For reference (see https://github.com/auto-complete/auto-complete/blob/master/doc/manual.md)
;; (defun mysource2-candidates ()
;;   '("Foo" "Bar" "Baz" "macoyTest2" "what" "zoooo"))

;; (defvar ac-source-mysource2
;;   '((candidates . mysource2-candidates)))

(defvar ac-source-macoy-ido-tags
  '(;;(init . macoy-ido-find-tag-refresh) ;; Commented because it runs every time (unnecessary)
	(candidates . macoy-tag-names)
    (cache)))

;; Autocomplete from precompiled tags list (normal tags source is too slow)
;; Make sure auto-complete knows about yasnippets
;; From https://github.com/joaotavora/yasnippet/issues/336
(require 'auto-complete-config)
(setq-default ac-sources '(
                           ac-source-yasnippet
                           ac-source-words-in-same-mode-buffers
						   ac-source-macoy-ido-tags
                           ))

;; Alternate find file in project thing using tags
;; If projectile isn't doing the trick, use tags instead
;; From https://www.emacswiki.org/emacs/InteractivelyDoThings#CompleteFindTagUsingIdo
(defun macoy-ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
	(let ((enable-recursive-minibuffers t))
	  (visit-tags-table-buffer))
	(find-file
	 (expand-file-name
	  (ido-completing-read
	   "Project file: " (tags-table-files) nil t)))))

;; cquery language server
;; See https://github.com/cquery-project/cquery/wiki
;; Run `lsp` to enable it in a buffer
(when (require 'cquery)
  (setq cquery-executable "f:/gitRepos/cquery/build/Release/cquery.exe")
  (setq cquery-project-roots '("f:/CJUNCTIONS/src" ))
  )

;; Find references via tags-search. This is my find-references replacement
(defun macoy-tags-search ()
  "Pick tag with `macoy-ido-find-tag' then run `tags-search' (or search marked)"
  (interactive)
  (if (use-region-p)
	  (tags-search (buffer-substring (region-beginning) (region-end)))
	(tags-search (ido-completing-read "Tag: " macoy-tag-names))
	))

;; Hippie Expand/DAbbrev settings
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers))
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-SPC") 'hippie-expand)
  
;; Find references
(global-set-key (kbd "C-\\") 'macoy-tags-search)
(global-set-key (kbd "C-|") 'tags-loop-continue)


(defun macoy-tags-query-replace-marked (replacement)
  (interactive (list
				(read-string (format "Replace %s with: "
									 (buffer-substring (region-beginning) (region-end))))))
  
  (tags-query-replace (buffer-substring (region-beginning) (region-end)) replacement)
  )

(defun macoy-tags-query-replace ()
  (interactive)
  (if (use-region-p)
	  (call-interactively 'macoy-tags-query-replace-marked (buffer-substring (region-beginning) (region-end)))
	(call-interactively 'tags-query-replace)
	)
  )
