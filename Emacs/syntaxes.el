
;;
;; Web tech
;;

;; This is for better syntax highlighting when editing templated web files (e.g. files with Nunjucks)
;; Only enabled at work because I only need web mode for template stuff
(when (require 'web-mode)
  ;; TODO: Customize colors (see http://web-mode.org/ "Syntax highlighting")
  
  ;; I like to manually enable rainbow-mode if I want to see colors (this might not work...)
  (setq web-mode-enable-css-colorization nil)

  ;; (set-face-foreground 'web-mode-html-attr-custom-face (face-foreground 'font-lock-variable-name-face))
  ;; (set-face-foreground 'web-mode-html-attr-name-face (face-foreground 'font-lock-variable-name-face))
  (set-face-foreground 'web-mode-html-attr-custom-face (face-foreground 'default))
  (set-face-foreground 'web-mode-html-attr-name-face (face-foreground 'default))
  (set-face-foreground 'web-mode-html-tag-bracket-face (face-foreground 'default))
  (set-face-foreground 'web-mode-html-tag-face (face-foreground 'font-lock-function-name-face))

  ;; Associate web files with web-mode
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

  ;; I use Nunjucks which is in the Django family
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")
          ("django" . "\\.js\\.")
          ("django" . "\\.css\\.")))
  )

;; Doesn't work because these args aren't the right command
;; (when (require 'web-beautify)
;;   ;; Override this function so I can customize args. Why did they make it defconst...
;;   (defconst macoy-web-beautify-args '("-f" "-t" "-"))
;;   (defun web-beautify-get-shell-command (program)
;;  "Join PROGRAM with the constant js-beautify args."
;;  (mapconcat 'identity (append (list program) macoy-web-beautify-args) " "))
;;   )

;; Jam
(load-user-file "jam-mode.el")

;;
;; Custom syntax definitions
;;

;; Data files
(define-generic-mode 'macoy-key-value-def-mode
  ;; Comments
  '("#")
  ;; Keywords
  '(
    "End" "Name"
    )
  ;; Font locks
  '(
    ;; Numerical constants
    ("\\_<[\-0-9e\.]+\\_>" . font-lock-constant-face)
    ;; Generic "Key Value" match (I'm proud of this :) )
    ;; I use variable here because it's setting a variable on the struct with match's name
    ;; (plus it looks different in a way that I like)
    ("^[[:blank:]]*+[[:alnum:]]+[[:blank:]]+*+" . font-lock-variable-name-face)
    ;; Functions. The 1 here means only highlight the first group
    ("\\([[:alnum:]]*\\)\(" 1 font-lock-function-name-face)
    )

  ;; Files to use this mode
  '(".ExampleExtension\\'")
  
  ;; Function list
  nil
  )

(when (require 'lua-mode)
  (setq lua-indent-level 4))

(when (require 'smart-tabs-mode
               (smart-tabs-insinuate 'c 'c++)))


;; Highlight function calls
;; (font-lock-remove-keywords 'c-mode
;;                         '(("\\(\\w+\\)\\s-*\("
;;                            (1 font-lock-string-face)))
;;                         )

;; (font-lock-remove-keywords 'c++-mode
;;                         '(("\\(\\w+\\)\\s-*\("
;;                            (1 font-lock-string-face)))
;;                         )
