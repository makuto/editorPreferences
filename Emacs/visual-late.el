;; This should be executed after custom-set-variables

;;
;; Macoy's custom theme overrides
;; These give emacs a more minimal, less contrast-y appearance
;; I put it down here so it happens after custom-set-variables sets the theme

;; Whole-window transparency
;; The first number is transparency while active
;; The second number is transparency while inactive
(defun macoy-normal-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 70)))
(defun macoy-no-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

;; Note that names need to be unique (they should be anyways)
(setq macoy-transparency-list (list
                               ;; '("Jam (current directory)" build-universal-jam)
                               '(70 60)
                               '(80 70)
                               '(85 70)
                               '(90 70)
                               '(100 100)))

(setq macoy-transparency-index 2)

(defun macoy-cycle-transparency (&optional index)
  (interactive)
  (if index
      (setq macoy-transparency-index index)
    (setq macoy-transparency-index (+ macoy-transparency-index 1)))
  ;; Loop around
  (unless (< macoy-transparency-index (safe-length macoy-transparency-list))
    (setq macoy-transparency-index 0))
  (let ((transparency-settings (nth macoy-transparency-index macoy-transparency-list)))
    (set-frame-parameter (selected-frame) 'alpha transparency-settings)
    (message "Transparency now %s" transparency-settings)))

;; Set default transparency (-1 because 1 will be added due to the "cycle" thing)
(macoy-cycle-transparency (- macoy-transparency-index 1))
(global-set-key (kbd "<f9>") 'macoy-cycle-transparency)

;; Add a slight border to give us some breathing room on the edges
(set-frame-parameter (selected-frame) 'internal-border-width 10)
;; Uncomment to disable the edge border
;; (set-frame-parameter (selected-frame) 'internal-border-width 0)

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
;; (set-face-foreground 'fringe (face-foreground 'font-lock-comment-delimiter-face)) ;; for extra subdued

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

;; TODO: Colors to differentiate between groups; foregrounds
(when (require 're-builder)
  (set-face-background 'reb-match-0 (face-background 'region))
  (set-face-background 'reb-match-1 "#c15759")
  (set-face-background 'reb-match-2 "#fbbd5c")
  (set-face-background 'reb-match-3 "#ff605d")
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

(when (require 'smerge-mode)
  ;; (set-face-attribute 'smerge-refined-added nil :background "#2c4a27")
  (set-face-attribute 'smerge-refined-added nil :background "#182916")
  (set-face-attribute 'smerge-refined-removed nil :background "#2c1717")
  (set-face-attribute 'smerge-refined-changed nil :background "#2c4a27")
  (set-face-attribute 'smerge-markers nil :background (face-background 'match))
  (set-face-attribute 'smerge-base nil :background "#482300")
  (set-face-attribute 'smerge-upper nil :background "#4f2929")
  (set-face-attribute 'smerge-lower nil :background "#233c20"))

(when (require 'web-mode)
  ;; TODO: Customize colors (see http://web-mode.org/ "Syntax highlighting")
  ;; (set-face-foreground 'web-mode-html-attr-custom-face (face-foreground 'font-lock-variable-name-face))
  ;; (set-face-foreground 'web-mode-html-attr-name-face (face-foreground 'font-lock-variable-name-face))
  (set-face-foreground 'web-mode-html-attr-custom-face (face-foreground 'default))
  (set-face-foreground 'web-mode-html-attr-name-face (face-foreground 'default))
  (set-face-foreground 'web-mode-html-tag-bracket-face (face-foreground 'default))
  (set-face-foreground 'web-mode-html-tag-face (face-foreground 'font-lock-function-name-face)))

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

  (set-face-foreground 'magit-diff-context (face-foreground 'font-lock-comment-face))
  (set-face-foreground 'magit-diff-context-highlight (face-foreground 'font-lock-comment-face))
  )

;; Get rid of the strange light gray background
(when (require 'vc-annotate)
  ;; (setq 'vc-annotate-background "#55")
  ;; (set-face-attribute 'vc-annotate-face-7CB8BB nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-8CD0D3 nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-8FB28F nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-93E0E3 nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-94BFF3 nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-9FC59F nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-AFD8AF nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-BC8383 nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-BFEBBF nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-CC9393 nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-D0BF8F nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-DC8CC3 nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-DFAF8F nil :background nil)
  ;; (set-face-attribute 'vc-annotate-face-F0DFAF nil :background nil)
  )

;; Org faces customization
(when (require 'org-faces)
  (set-face-foreground 'org-agenda-clocking (face-foreground 'region))
  (set-face-background 'org-agenda-clocking (face-background 'region))
  ;; This is used for the date output in org-schedule
  (set-face-foreground 'secondary-selection (face-foreground 'region))
  (set-face-background 'secondary-selection (face-background 'region))
  ;; This is normally font-lock-comment-face, which is not very readable
  (set-face-foreground 'org-level-4 (face-foreground 'font-lock-doc-face))
  (set-face-foreground 'outline-4 (face-foreground 'font-lock-doc-face))
  ;; I'm not sure why I had to do this, but make 100% sure links are underlined
  (set-face-underline 'org-link t)
  ;; Don't use the default block face for code
  (setq org-src-block-faces '(("C" 'default)
                              ("python" 'default)
                              ("C++" 'default)
                              ("emacs-lisp" 'default)
                              ("lisp" 'default)
                              ("sh" 'default)
                              ("cakelisp" 'default)))
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

;; Hide search term highlight in Occur (I think)
(setq list-matching-lines-face nil)

(set-face-foreground 'escape-glyph (face-foreground 'font-lock-warning-face))

;; Bad whitespace display
(setq-default show-trailing-whitespace t)
;; Ensure whitespace isn't shown in e.g. ido vertical (the ido-specific hooks didn't do the trick)
(add-hook 'minibuffer-inactive-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'compilation-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))

(set-face-foreground 'trailing-whitespace (face-foreground 'font-lock-comment-delimiter-face))
(set-face-background 'trailing-whitespace (face-foreground 'font-lock-comment-delimiter-face))
