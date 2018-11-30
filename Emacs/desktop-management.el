
;; save/restore opened files
(desktop-save-mode 1)
;; This is needed only for theming. Desktop frames hold on to color values for some reason. We don't
;;  care too much about losing our frame configurations so this is okay
(setq desktop-restore-frames nil)

;;
;; Faster desktop creation/switching
;;
(setq macoy-desktop-dir "~/.emacs.d/macoy-desktops/")
(unless (file-exists-p macoy-desktop-dir)
  (make-directory macoy-desktop-dir)
  )

(defun macoy-save-desktop (new-desktop-name)
  "Save a desktop to the desktop registry for easy switching"
  (interactive "sNew desktop name: ")
  (let ((new-desktop (concat macoy-desktop-dir new-desktop-name)))
	(make-directory new-desktop)
	(desktop-save new-desktop)
	(message "Created desktop at %s" new-desktop)
	))

(defun macoy-switch-desktop ()
	"Use ido to list desktops to switch to"
  (interactive)
  (desktop-change-dir
   (concat macoy-desktop-dir
		   (ido-completing-read "Switch Desktop: "
								(remove "."
										(remove ".."
												(directory-files macoy-desktop-dir)))))))
