(when (require 'gud)
  (defvar macoy-gud-minor-mode-map
    (let ((map (make-sparse-keymap)))
	  (define-key map (kbd "S-<f5>") 'gud-run)
      (define-key map (kbd "C-<f5>") 'gud-run)
      (define-key map (kbd "<f5>") 'gud-cont)
      (define-key map (kbd "<f9>") 'gud-break)
      ;; Do not enter functions
	  (define-key map (kbd "<f10>") 'gud-next)
      ;; Do enter functions
      (define-key map (kbd "<f11>") 'gud-step)
      ;; Step out of function
      (define-key map (kbd "S-<f11>") 'gud-finish)
      ;; Print at mark
      (define-key map (kbd "C-SPC") 'gud-print)
      (define-key map (kbd "C-S-SPC") 'gud-watch)
      map)
    "macoy-gud-minor-mode keymap.")

  (define-minor-mode macoy-gud-minor-mode
    "A minor mode for Visual-Studio-like gud keybindings."
    :init-value nil
    :lighter " Macoy-GUD")
  )
