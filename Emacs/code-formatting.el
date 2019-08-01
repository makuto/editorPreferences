
;; C indentation settings
;; bsd AKA Allman https://www.emacswiki.org/emacs/IndentingC
(setq-default c-default-style "bsd"
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)

;; Doesn't quite work
;; (defun infer-indentation-style ()
;;   ;; if our source file uses tabs, we use tabs, if spaces spaces, and if        
;;   ;; neither, we use the current indent-tabs-mode
;;   (let ((space-count (how-many "^  " (point-min) (point-max)))
;;         (tab-count (how-many "^\t" (point-min) (point-max))))
;;     (if (> space-count tab-count)
;;          ;; ((message "Indent using space")
;;           (setq indent-tabs-mode nil))
;;     (if (> tab-count space-count)
;;          ;; ((message "Indent using tab")
;;           (setq indent-tabs-mode t))))

;; (add-hook 'c-mode-common-hook
;;           (lambda () (setq indent-tabs-mode nil)
;;             (infer-indentation-style)))

;; (add-hook 'lisp-mode-hook
;;           (lambda () (setq indent-tabs-mode nil)
;;              (infer-indentation-style)))

;; Clang format
;; Looks for .clang-format in project dir
(when (require 'clang-format)

  (defun macoy-clang-format-region-or-buffer ()
    "Format the region if one is selected, otherwise format the buffer"
    (interactive)
    (save-excursion
      (if (use-region-p)
          (call-interactively 'clang-format-region)
        (call-interactively 'clang-format-buffer))))

  (defun macoy-clang-format-paragraph ()
    "Format the block/paragraph"
    (interactive)
    (save-excursion
      (unless (use-region-p)
        (mark-paragraph))
      (when (use-region-p)
        (call-interactively 'clang-format-region))))

  (defun macoy-clang-format-function ()
    "Format the function"
    (interactive)
    (save-excursion
      (unless (use-region-p)
        (mark-defun))
      (when (use-region-p)
        (call-interactively 'clang-format-region))))

  (global-set-key (kbd "C-M-a") 'macoy-clang-format-region-or-buffer)
  (global-set-key (kbd "C-.") 'macoy-clang-format-paragraph)
  (global-set-key (kbd "C->") 'macoy-clang-format-function)

  ;; Not sure if this actually does anything
  ;; https://www.reddit.com/r/emacs/comments/7uq9w1/replace_emacs_c_autoformatting_with_clangformat/
  ;; (fset 'c-indent-region 'clang-format-region)
  )
