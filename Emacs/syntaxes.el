
;;
;; Web tech
;;

;; This is for better syntax highlighting when editing templated web files (e.g. files with Nunjucks)
;; Only enabled at work because I don't use templates at home
(when (string-equal (user-login-name) "mmadson")
  ;; TODO: Customize colors (see http://web-mode.org/ "Syntax highlighting")
  (require 'web-mode)
  ;; I like to manually enable rainbow-mode if I want to see colors (this might not work...)
  (setq web-mode-enable-css-colorization nil)

  ;; Associate web files with web-mode
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

  ;; I use Nunjucks which is in the Django family
  (setq web-mode-engines-alist
      '(("django" . "\\.html\\'")
        ("django" . "\\.js\\.")
		("django" . "\\.css\\.")
		)
	  )
  )

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
