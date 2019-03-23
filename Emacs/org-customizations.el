
(when (require 'simpleclip)
  ;; (defun macoy-org-create-copy-link-from-point (link-text)
  ;; 	"Create a link from the file and line at point and copy it to the clipboard"
  ;; 	(interactive "sLink text: ")
  ;; 	(simpleclip-set-contents (format "[[%s::%d][%s]]" buffer-file-name (line-number-at-pos) link-text))
  ;; 	)

  (defun macoy-org-copy-file-line-link-to-clipboard ()
	(interactive)
	(simpleclip-set-contents (format "[[file:%s::%d][" buffer-file-name (line-number-at-pos)))
	)
  )

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<tab>") nil)
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "C") nil)
  ;; Don't mess with my arrow keys. I use them a lot
  (define-key org-mode-map (kbd "C-S-<down>")  nil)
  (define-key org-mode-map (kbd "C-S-<left>")  nil)
  (define-key org-mode-map (kbd "C-S-<right>") nil)
  (define-key org-mode-map (kbd "C-S-<up>")    nil)
  (define-key org-mode-map (kbd "M-S-<down>")  nil)
  (define-key org-mode-map (kbd "M-S-<left>")  nil)
  (define-key org-mode-map (kbd "M-S-<right>") nil)
  (define-key org-mode-map (kbd "M-S-<up>")    nil)
  (define-key org-mode-map (kbd "M-<down>")    nil)
  (define-key org-mode-map (kbd "M-<left>")    nil)
  (define-key org-mode-map (kbd "M-<right>")   nil)
  (define-key org-mode-map (kbd "M-<up>")      nil)
  (define-key org-mode-map (kbd "S-<down>")    nil)
  (define-key org-mode-map (kbd "S-<left>")    nil)
  (define-key org-mode-map (kbd "S-<right>")   nil)
  (define-key org-mode-map (kbd "S-<up>")      nil)
  (define-key org-mode-map (kbd "C-e")         nil)

  ;; Expand region is useful in Org too
  (define-key org-mode-map (kbd "C-'") nil)
  ;; I already have a convention where C-Enter and C-S-Enter open up new lines; let's make org follow that
  (define-key org-mode-map (kbd "C-S-<return>") 'macoy-org-insert-heading-respect-content-before)
  )

;; Org: indent nested things
(setq org-startup-indented t)

;; Use ido for refile (doesn't work)
;; (setq org-refile-targets '((nil :maxlevel . 6)
                           ;; (org-agenda-files :maxlevel . 6)))
;; (setq org-refile-use-outline-path nil)
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)

;; My org files
(when (string-equal (user-login-name) "macoy")
  (setq org-agenda-files (list "~/Dropbox/Org/1_Calendar.org"
							   "~/Dropbox/Org/0_Dump.org"))
  )

(when (string-equal (user-login-name) "mmadson")
  (setq org-agenda-files (list "C:/Users/mmadson/Dropbox/Org/1_Calendar.org"
							   "C:/Users/mmadson/Dropbox/Org/0_Dump.org"))
  )

;; From https://orgmode.org/manual/Languages.html#Languages
;; Because I was too lazy to find the actual internal list
(setq macoy-org-code-block-languages (list
									  "asymptote"
									  "lua"
									  "awk"
									  "matlab"
									  "C"
									  "mscgen"
									  "C++"
									  "ocaml"
									  "clojure"
									  "octave"
									  "css"
									  "org"
									  "D"
									  "oz"
									  "ditaa"
									  "perl"
									  "calc"
									  "plantuml"
									  "processing"
									  "fortran"
									  "python"
									  "gnuplot"
									  "R"
									  "screen"
									  "ruby"
									  "dot"
									  "sass"
									  "haskell"
									  "scheme"
									  "java"
									  "sed"
									  "js"
									  "sh"
									  "latex"
									  "sql"
									  "ledger"
									  "sqlite"
									  "lilypond"
									  "vala"
									  "lisp"
									  ))

(defun macoy-org-insert-code-block ()
  (interactive)
  (insert (format "#+BEGIN_SRC %s\n\n#+END_SRC"
				  (ido-completing-read "Language: " macoy-org-code-block-languages)))
  (forward-line -1)
  )

(when (require 'simpleclip)
  (defun macoy-org-copy-code-block ()
	"Intended to be executed from a source file while region is active. Create a link and org code
 block and set the clipboard to it"
	(interactive)
	(when (use-region-p)
	  (simpleclip-set-contents (format "[[file:%s::%d][%s:%d]]\n#+BEGIN_SRC %s\n%s\n#+END_SRC"
									   (buffer-file-name)
									   (line-number-at-pos (region-beginning))
									   (buffer-name)
									   (line-number-at-pos (region-beginning))
									   (ido-completing-read "Language: " macoy-org-code-block-languages)
									   (buffer-substring (region-beginning) (region-end)))
							   )
	  )
	)
  )
