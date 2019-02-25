;; This should be executed after custom-set-variables

;;
;; Macoy's custom theme overrides
;; These give emacs a more minimal, less contrast-y appearance
;; I put it down here so it happens after custom-set-variables sets the theme

;; Set the border color to the fringe to have less contrast-y line (generally; will vary per theme)
;; Commented versions are for when base16-distinct-fringe-background wasn't nil
;; (set-face-background 'vertical-border (face-background 'fringe))
;; (set-face-foreground 'vertical-border (face-background 'vertical-border))
(set-face-foreground 'vertical-border (face-foreground 'font-lock-comment-delimiter-face))

;; Make the fringe indicators a little more subdued. This might be too much if I start
;; using the fringe for anything more than wrapping indicators, but for now it is fine
;; We'll use the comment colors because comments are usually high enough contrast to read
;;  but still more subdued than regular text (and regular fringe foreground)
;; See base16-theme.el for faces and colors and stuff
(set-face-foreground 'fringe (face-foreground 'font-lock-comment-face))

;; Make fringe same color as background. We still want fringe for wrap indicators
;; If you change your theme you should run this code again
;; Note that the vertical border uses the fringe color before we set it to default
;; Commented because above we set base16-distinct-fringe-background to nil
;; (set-face-background 'fringe (face-background 'default))

(when (require 'isearch)
  ;; This is a little weird but I like the color and it should be okay
  (set-face-foreground 'lazy-highlight (face-foreground 'region))
  (set-face-background 'lazy-highlight (face-background 'region))
  )

;; Make avy faces beautiful
;; Note that we swap foreground and background to emulate the inverse-video setting (there's probably
;;; a cleaner way of doing this, but oh well)
(when (require 'avy)
  (set-face-foreground 'avy-lead-face (face-background 'match))
  (set-face-background 'avy-lead-face (face-foreground 'match))
  (set-face-foreground 'avy-lead-face-0 (face-background 'match))
  (set-face-background 'avy-lead-face-0 (face-foreground 'match))
  (set-face-foreground 'avy-lead-face-1 (face-background 'match))
  (set-face-background 'avy-lead-face-1 (face-foreground 'match))
  (set-face-foreground 'avy-lead-face-2 (face-background 'match))
  (set-face-background 'avy-lead-face-2 (face-foreground 'match))
  )

;; Make dsvn marked face match theme
(when (require 'dsvn)
  (set-face-foreground 'svn-mark-face (face-foreground 'region))
  (set-face-background 'svn-mark-face (face-background 'region))
  )

;; TODO: Colors to differentiate between groups
(when (require 're-builder)
  (set-face-background 'reb-match-0 (face-background 'region))
  (set-face-background 'reb-match-1 (face-background 'region))
  (set-face-background 'reb-match-2 (face-background 'region))
  (set-face-background 'reb-match-3 (face-background 'region))
  )

;; Make sure diff-mode colors are theme-appropriate:
;; - No stark white text for context
;; - Make refine* backgrounds much more muted and darker. This is for style and higher contrast
;;   between foreground and background of highlighted text
(when (require 'diff-mode)
  (set-face-foreground 'diff-context (face-foreground 'font-lock-comment-face))
  (set-face-attribute 'diff-refine-added nil :background "#2c4a27")
  (set-face-attribute 'diff-refine-removed nil :background "#4a2727")
  )

(when (require 'ediff)
  ;; Added
  (set-face-attribute 'ediff-current-diff-A nil :background "#4a2727")
  (set-face-attribute 'ediff-fine-diff-A nil :background "#381e1e")
  ;; Removed
  (set-face-attribute 'ediff-current-diff-B nil :background "#21381e")
  (set-face-attribute 'ediff-fine-diff-B nil :background "#2c4a27")
  )

;; Make magit's diff look similar/the same as diff-mode
(when (require 'magit)
  (set-face-attribute 'magit-diff-added-highlight nil
					  :foreground (face-foreground 'diff-added)
					  :background "#2c4a27")
  (set-face-attribute 'magit-diff-removed-highlight nil
					  :foreground (face-foreground 'diff-added)
					  :background "#4a2727")
  (set-face-attribute 'magit-diff-added nil
					  :foreground (face-foreground 'diff-added)
					  :background "#21381e")
  (set-face-attribute 'magit-diff-removed nil
					  :foreground (face-foreground 'diff-added)
					  :background "#381e1e")
  )

;; Org faces customization
(when (require 'org-faces)
  (set-face-foreground 'org-agenda-clocking (face-foreground 'region))
  (set-face-background 'org-agenda-clocking (face-background 'region))
  ;; This is used for the date output in org-schedule
  (set-face-foreground 'secondary-selection (face-foreground 'region))
  (set-face-background 'secondary-selection (face-background 'region))

  ;; I'm not sure why I had to do this, but make 100% sure links are underlined
  (set-face-underline 'org-link t)
  )

;; Hide these modes completely (for a more minimal look)
;; Diminish things down here so that we're sure we catch all modes
(when (require 'diminish)
  (diminish 'abbrev-mode)
  (diminish 'my-keys-minor-mode)
  (diminish 'yas-minor-mode)
  (diminish 'auto-complete-mode)
  (diminish 'visual-line-mode)
  (diminish 'org-indent-mode)
  ;; The following might not do anything/are unnecessary
  (diminish 'adaptive-wrap-prefix-mode)
  (diminish 'wrap-region-mode)
  (diminish 'auto-revert-mode)
  ;; TODO: This needs to be in a hook somewhere for org mode. It doesn't work currently
  (diminish 'org-indent-mode)
  )
