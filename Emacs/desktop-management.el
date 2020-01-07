
;; save/restore opened files
(desktop-save-mode 1)

;; This is needed only for theming. Desktop frames hold on to color values for some reason. We don't
;;  care too much about losing our frame configurations so this is okay
(setq desktop-restore-frames nil)

;; Lazy load buffers instead of loading them all at once (which takes too long)
(setq desktop-restore-eager 4)

;; Used to know whether or not to show the last selected desktop at the top of the desktop list
(setq macoy-has-ever-selected-desktop-this-session nil)

(defcustom macoy-selected-desktop nil
  "The last selected desktop. This is used to load the most recent desktop when starting Emacs")

;;
;; Faster desktop creation/switching
;;
(setq macoy-desktop-dir "~/.emacs.d/macoy-desktops/")
(unless (file-exists-p macoy-desktop-dir)
  (make-directory macoy-desktop-dir))

(defun macoy-save-desktop (new-desktop-name)
  "Save a desktop to the desktop registry for easy switching"
  (interactive "sNew desktop name: ")
  (let ((new-desktop (concat macoy-desktop-dir new-desktop-name)))
    (make-directory new-desktop)
    (desktop-save new-desktop)
    (customize-save-variable 'macoy-selected-desktop new-desktop)
    (message "Created desktop at %s" new-desktop)
    (setq macoy-has-ever-selected-desktop-this-session t)))

(defun macoy-get-desktop-list ()
  (let ((desktop-list (remove "."
                              (remove ".."
                                      (directory-files macoy-desktop-dir)))))
    ;; If we've never picked a desktop this session, put the last used desktop at the top of the
    ;; list. This is a workaround while I don't know how to make my desktop switch happen after
    ;; the regular emacs behavior picks the desktop during startup.
    (if (and (not macoy-has-ever-selected-desktop-this-session)
             macoy-selected-desktop)
        (progn
          (setq desktop-list (remove (file-name-nondirectory macoy-selected-desktop) desktop-list))
          (add-to-list 'desktop-list (file-name-nondirectory macoy-selected-desktop)))
      desktop-list)))

(defun macoy-switch-desktop ()
  "Use ido to list desktops to switch to"
  (interactive)
  (let ((selected-desktop
         (concat macoy-desktop-dir
                 (ido-completing-read "Switch Desktop: "
                                      (macoy-get-desktop-list)))))
    (message "%s" selected-desktop)
    (customize-save-variable 'macoy-selected-desktop selected-desktop)
    (desktop-change-dir selected-desktop)
    (setq macoy-has-ever-selected-desktop-this-session t)))

(defun macoy-reload-last-desktop ()
  "Load the last desktop which was saved or switched to via the macoy-desktop system"
  (interactive)
  (desktop-change-dir macoy-selected-desktop)
  (message "Restored desktop %s" macoy-selected-desktop)
  (setq macoy-has-ever-selected-desktop-this-session t))

(global-set-key (kbd "M-b") 'macoy-reload-last-desktop)
