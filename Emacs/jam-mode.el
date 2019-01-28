
;; *****************************************************************************
;;
;;  jam-mode.el
;;  Font-lock support for Jam files
;;
;;  Copyright (C) 2003, 2004, Rob Walker <address@hidden>
;;    http://www.tenfoot.org.uk/emacs/
;;  12 May 2004 - 0.3 - Fix keyword quoting, XEmacs support
;;  22 Mar 2003 - 0.2 - Autoload
;;  04 Mar 2003 - 0.1 - Added imenu support and basic indentation
;;
;;  Copyright (C) 2000, Eric Scouten
;;  Started Sat, 05 Aug 2000
;;
;; *****************************************************************************
;;
;;  This is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published
;; by
;;  the Free Software Foundation; either version 2, or (at your
;; option)
;;  any later version.
;;
;;  jam-mode.el is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with GNU Emacs; see the file COPYING.  If not, write to the
;;  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;  Boston, MA 02111-1307, USA.
;;
;; *****************************************************************************
;;
;;  To add font-lock support for Jam files, simply add the line
;;  (require 'jam-mode) to your .emacs file. Make sure generic-mode.el
;;  is visible in your load-path as well.
;;  
;; *****************************************************************************


;; Generic-mode is a meta-mode which can be used to define small modes
;; which provide basic comment and font-lock support. Jam-mode depends
;; on
;; this mode.

;; generic.el for GNU emacs, generic-mode.el for XEmacs
(if (string-match "XEmacs\\|Lucid" emacs-version)
    (require 'generic-mode)
  (require 'generic))

(defun jam-mode-quote-keywords (keywords)
  "Returns a list of expressions that match each element in KEYWORDS. 
For generic-mode, each element is quoted. For generic, each element is
unchanged."
  (if (featurep 'generic-mode)
      (mapcar 'regexp-quote keywords)
    keywords))

;;;###autoload
(define-generic-mode 'jam-mode

										; Jam comments always start with '#'
  (list ?# )

										; Jam keywords (defined later)
  nil

										; Extra stuff to colorize
  (list

										; Jam keywords
   (generic-make-keywords-list
	(list "actions" "bind" "case" "default" "else" "existing" "for" "if"
		  "ignore" "in" "include" "local" "on" "piecemeal" "quietly" "rule"
		  "switch"
		  "together" "updated")
	'font-lock-keyword-face)

										; Jam built-in variables
   (generic-make-keywords-list
	(list
	 "JAMDATE" "JAMSHELL" "JAMUNAME" "JAMVERSION" "MAC" "NT" "OS" "OS2"
	 "OSPLAT" "OSVER" "UNIX" "VMS")
	'font-lock-constant-face)

										; Jam built-in targets
   (generic-make-keywords-list
	(list
	 "ALWAYS" "DEPENDS" "ECHO" "INCLUDES" "LEAVES" "LOCATE" "NOCARE"
	 "NOTFILE" "NOUPDATE" "SEARCH" "TEMPORARY")
	'font-lock-builtin-face)

										; Jam built-in targets (warnings)
   (generic-make-keywords-list
	(list
	 "EXIT")
	'font-lock-warning-face)

										; Jambase rules
   (generic-make-keywords-list
	(jam-mode-quote-keywords 
     (list
      "Archive" "As" "Bulk" "Cc" "CcMv" "C++" "Chgrp" "Chmod"
	  "Chown" "Clean" "CreLib"
      "Depends" "File" "Fortran" "GenFile" "GenFile1" "HardLink"
      "HdrRule" "Install" "InstallBin" "InstallFile"
	  "InstallInto" "InstallLib" "InstallMan"
      "InstallShell" "Lex" "Library" "LibraryFromObjects" "Link"
	  "LinkLibraries"
      "Main" "MainFromObjects" "MakeLocate" "MkDir" "MkDir1"
	  "Object" "ObjectC++Flags"
      "ObjectCcFlags" "ObjectHdrs" "Objects" "Ranlib" "RmTemps"
	  "Setuid" "SubDir"
      "SubDirC++Flags" "SubDirCcFlags" "SubDirHdrs" "SubInclude"
	  "Shell" "Undefines"
      "UserObject" "Yacc" "Yacc1" "BULK" "FILE" "HDRRULE"
	  "INSTALL" "INSTALLBIN" "INSTALLLIB"
      "INSTALLMAN" "LIBRARY" "LIBS" "LINK" "MAIN" "SETUID"
	  "SHELL" "UNDEFINES"
      "addDirName" "makeCommon" "makeDirName" "makeGrist"
	  "makeGristedName" "makeRelPath"
      "makeString" "makeSubDir" "makeSuffixed" "unmakeDir"))
	'font-lock-function-name-face)
   
										; Jambase built-in targets
   (generic-make-keywords-list
	(list
	 "all" "clean" "dirs" "exe" "files" "first" "install" "lib" "obj"
	 "shell" "uninstall")
	'font-lock-builtin-face)

										; Jambase built-in variables
   (generic-make-keywords-list
	(jam-mode-quote-keywords
     (list
      "ALL_LOCATE_TARGET" "AR" "ARFLAGS" "AS" "ASFLAGS" "AWK"
	  "BCCROOT" "BINDIR" "CC" "CCFLAGS"
      "C++" "C++FLAGS" "CHMOD" "CP" "CRELIB" "CW" "CWGUSI"
	  "CWMAC" "CWMSL" "DOT" "DOTDOT"
      "EXEMODE" "FILEMODE" "FORTRAN" "FORTRANFLAGS" "GROUP"
	  "HDRGRIST" "HDRPATTERN" "HDRRULE"
      "HDRS" "HDRSCAN" "HDRSEARCH" "INSTALL" "JAMFILE" "JAMRULES"
	  "LEX" "LIBDIR" "LINK"
      "LINKFLAGS" "LINKLIBS" "LOCATE_SOURCE" "LOCATE_TARGET" "LN"
	  "MACINC" "MANDIR" "MKDIR"
      "MODE" "MSLIB" "MSLINK" "MSIMPLIB" "MSRC" "MSVC" "MSVCNT"
	  "MV" "NEEDLIBS" "NOARSCAN"
      "OSFULL" "OPTIM" "OWNER" "RANLIB" "RCP" "RELOCATE" "RM"
	  "RSH" "RUNVMS" "SEARCH_SOURCE"
      "SED" "SHELLHEADER" "SHELLMODE" "SLASH" "SLASHINC"
	  "SOURCE_GRIST" "STDHDRS" "STDLIBPATH"
      "SUBDIR" "SUBDIRASFLAGS" "SUBDIRC++FLAGS" "SUBDIRCCFLAGS"
	  "SUBDIRHDRS" "SUBDIR_TOKENS"
      "SUFEXE" "SUFLIB" "SUFOBJ" "UNDEFFLAG" "UNDEFS" "WATCOM"
	  "YACC" "YACCFLAGS" "YACCFILES"))
	'font-lock-function-name-face)

										; Jam variable references $(foo)
   '("$(\\([^ :\\[()\t\r\n]+\\)[)\\[:]" 1 font-lock-variable-name-face))

										; Apply this mode to all files called Jamfile, Jamrules or Jambase
  (list "\\(Jamfile\\|Jamrules\\|Jambase\\)\\'")

										; Attach setup function so we can modify syntax table.
  (list 'jam-mode-setup-function)

										; Brief description
  "Generic mode for Jam rules files")

(defun jam-mode-setup-function ()
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?. "w")
  (modify-syntax-entry ?/ "w")
  (modify-syntax-entry ?+ "w")
  (modify-syntax-entry ?# "<")
  (modify-syntax-entry ?\n ">")
  (setq imenu-generic-expression 
        '(("Rules" "^rule\\s-+\\([A-Za-z0-9_]+\\)" 1)
          ("Actions" "^actions\\s-+\\([A-Za-z0-9_]+\\)" 1)))
  (imenu-add-to-menubar "Jam")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'jam-indent-line)
  (run-hooks 'jam-mode-hook)
  )

(defvar jam-mode-hook nil)

(defvar jam-indent-size 2
  "Amount to indent by in jam-mode")

(defvar jam-case-align-to-colon t
  "Whether to align case statements to the colons")

(defun jam-indent-line (&optional whole-exp)
  "Indent current line"
  (interactive)
  (let ((indent (jam-indent-level))
		(pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (zerop (- indent (current-column)))
		nil
      (delete-region beg (point))
      (indent-to indent))
    (if (> (- (point-max) pos) (point))
		(goto-char (- (point-max) pos)))
    ))

(defun jam-goto-block-start ()
  "Goto the start of the block containing point (or beginning of buffer if not in a block)"
  (let ((l 1))
    (while (and (not (bobp)) (> l 0))
      (skip-chars-backward "^{}")
      (unless (bobp)
        (backward-char)
        (setq l (cond 
                 ((eq (char-after) ?{) (1- l))
                 ((eq (char-after) ?}) (1+ l))
                 )))
      )
    (bobp))
  )

(defun jam-indent-level ()
  (save-excursion
    (let ((p (point))
          ind 
          (is-block-start nil)
          (is-block-end nil)
          (is-case nil)
          (is-switch nil)
          switch-ind)
      ;; see what's on this line
      (beginning-of-line)
      (setq is-block-end (looking-at "^[^{\n]*}\\s-*$")) 
      (setq is-block-start (looking-at ".*{\\s-*$"))
      (setq is-case (looking-at "\\s-*case.*:"))

      ;; goto start of current block (0 if at top level)
      (if (jam-goto-block-start)
          (setq ind 0)
        (setq ind (+ (current-indentation) jam-indent-size)))
      
      ;; increase indent in switch statements (not cases)
      (setq is-switch (re-search-backward "^\\s-*switch" (- (point)
															100) t))
      (when (and is-switch (not (or is-block-end is-case)))
        (goto-char p)
        (setq ind (if (and jam-case-align-to-colon
                           (re-search-backward
							"^\\s-*case.*?\\(:\\)"))
                      (+ (- (match-beginning 1) (match-beginning 0))
                         jam-indent-size)
                    (+ ind jam-indent-size)))
        )
      
      ;; indentation of this line is jam-indent-size more than that of the
      ;; previous block 
      (cond (is-block-start  ind)
            (is-block-end    (- ind jam-indent-size))
            (is-case         ind)
            (t               ind)
            )
      )))

(provide 'jam-mode)

;; jam-mode.el ends here
