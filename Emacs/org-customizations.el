
(when (require 'simpleclip)
  (defun macoy-create-copy-org-link-from-point (link-text)
	"Create a link from the file and line at point and copy it to the clipboard"
	(interactive "sLink text: ")
	(simpleclip-set-contents (format "[[%s::%d][%s]]" buffer-file-name (line-number-at-pos) link-text))
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

;; My org files
(when (string-equal (user-login-name) "macoy")
  (setq org-agenda-files (list "~/Dropbox/Org/1_Calendar.org"
							   "~/Dropbox/Org/0_Dump.org"))
  )

(when (string-equal (user-login-name) "mmadson")
  (setq org-agenda-files (list "C:/Users/mmadson/Dropbox/Org/1_Calendar.org"
							   "C:/Users/mmadson/Dropbox/Org/0_Dump.org"))
  )
