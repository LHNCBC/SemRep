;; -*- Mode: Emacs-Lisp -*-
;; [PM] 2003-01-23 (SICStus 3.10.1b1)

;; Load this file from your .emacs file to set up Emacs to use the
;; SICstus Prolog mode etc.

;; This file should be loaded from the place it was put when
;; installing SICStus. This ensures that it can find the SICStus
;; executables and other supporting files when it is loaded.
;; Example UNIX:
;; (load-file "/usr/local/lib/sicstus-3.10.0/emacs/sicstus_emacs_init")
;; Example Windows::
;; (load-file "C:/Program Files/SICStus Prolog 3.10.0/emacs/sicstus_emacs_init")
;;
;; You can also manually do M-x load-file and then point emacs at the
;; sicstus_emacs_init.el file.

;; The below is strictly optional and only for Windows XP
;;
;; A disadvantage with the above is that you must modify your .emacs
;; every time you upgrade SICStus. On Windows XP you can figure out
;; this location from the registry:
;;
;; Something like the following (remove the comments and put the
;; function and the call into your .emacs file.)  If you need more
;; instructions you should simply use load-files directly as above.

;;  (defun find-and-load-sicstus-init-file (major minor)
;;    (let* ((reg-output (shell-command-to-string 
;;                        (format "REG QUERY HKLM\\Software\\SICS\\SICStus%d.%d /v SP_PATH" major minor)))
;;           (sp-path (and reg-output
;;                         (string-match "^\\s-+SP_PATH\\s-+REG_SZ\\s-+\\(.+\\)"
;;                                       reg-output)
;;                         (match-beginning 1)
;;                         (substring reg-output 
;;                                    (match-beginning 1)
;;                                    (match-end 1))))
;;           (sicstus-emacs-init-file (and sp-path
;;                                         (expand-file-name "emacs/sicstus_emacs_init.el"
;;                                                           sp-path))))
;;      (if (and sicstus-emacs-init-file
;;               (file-readable-p sicstus-emacs-init-file))
;;          (load-file sicstus-emacs-init-file)
;;        (message "Warning: Could not find the SICStus Emacs files"))))
;;
;;  ;; Find and load the SICStus support files
;;  ;; Major=3, minor=10 for SICStus 3.10.x.
;;  ;; You could use REG to figure out the most recent version (3.11.x, 4.x.y, ...)
;;  (find-and-load-sicstus-init-file 3 10)



;; *** "No user serviceable parts inside" ****
;; There should be no need to modify anything in this file.  If you
;; want to customize things, put it in your .emacs file.


;; [PM] 4.4.0 KEEP IN SYNC! These properties must be set in both
;; sicstus-support.el.in _and_ sicstus_emacs_init.el so they are set
;; _before_ prolog-mode is autoloaded.
;;
;; [PM] 4.3.3 Mark some buffer local variables as being safe local
;; variables (so Emacs does not prompt if you want to change the
;; default indent with a local variable in the mode line).
(put 'prolog-indent-width 'safe-local-variable 'integerp)
(put 'prolog-paren-indent 'safe-local-variable 'integerp)
;; [PM] 4.3.3 -*- ....; Module:foo; ... -*- is used by prolog-insert-module-modeline()
(put 'Module 'safe-local-variable 'symbolp)
(put 'module 'safe-local-variable 'symbolp) ; i.e. (lowecase 'Module), older Emacsen lowercased the property in -*- Prop:value -*-



(defun sicstus-init-setup-info (info-dir)
  (if (and info-dir
           (file-accessible-directory-p info-dir))
      ;; Info directories is a mess in XEmacs and even more so in
      ;; Emacs. Do the best we can.
      ;; Currently this will not work if Info has already been
      ;; initialized. This should not be a problem as long as this
      ;; file is loaded from the users .emacs file.
      (cond ((string-match "XEmacs\\|Lucid" emacs-version) ; XEmacs
             (if (boundp 'Info-directory-list)
                 (setq Info-directory-list (append Info-directory-list (list info-dir)))))
            (t                          ; FSF (GNU) Emacs
             (setq Info-default-directory-list (append Info-default-directory-list (list info-dir)))

             ;; Emacs (to 20.7 at least) ignores
             ;; Info-default-directory-list if INFOPATH is set in the
             ;; environment. As a workaround we add SICstus info dirs
             ;; to the environment variable.
             (if (getenv "INFOPATH")
                 (setenv "INFOPATH" (concat (getenv "INFOPATH") path-separator info-dir)))
             
             ))))


(defun sicstus-init-emacs (this-file-name)
  ;; [PD] 4.0.0beta1 cygwin32 is the system-type in XEmacs for Cygwin.
  ;;                 In GNU emacs it's simply cygwin.
  (let* ((is_win32 (memq system-type '(windows-nt ms-dos cygwin32 cygwin)))
	 (is_cygwin (memq system-type '(cygwin32 cygwin)))
         (sicstus-emacs-directory
          (and this-file-name           ; location of this file 
               (expand-file-name (file-name-directory this-file-name))))
         (sicstus-info-dir (and
                            sicstus-emacs-directory
                            (expand-file-name "../doc/info" sicstus-emacs-directory)))


         ;; How to traverse the file system to reach the SICStus bin
         ;; directory from the emacs directory
         (sicstus-bin-from-emacs-directory
          (if is_win32
              "../bin"                  ; Win32 from ./emacs/ to ./bin
            "../../../bin"              ; UNIX from ./lib/sicstus-3.10.0/emacs/ to  ./bin
            ))

         (sicstus-exe-suffix
          (if is_win32
              ".exe"                    ; Win32
            ""                          ; UNIX 
            ))

         (sicstus-bin-directory         ; location of sicstus, spld, splfr etc.
          (let ((raw
                 (and sicstus-emacs-directory
                      (expand-file-name (concat sicstus-emacs-directory
                                                sicstus-bin-from-emacs-directory)))))
	    ;; [PD] 4.0.0beta1 Don't replace / with \\ if we're in Cygwin.
            (if (and is_win32 (not is_cygwin))
                ;; [PM] 3.11.1 The better replace-in-string is not
                ;; available in all emacsen
                (progn
                  (while (string-match "/" raw)
                    (setq raw (replace-match "\\" t t raw)))
                  raw)
              raw)))
       
         (sicstus-program-basename
          (if is_win32
              "sicstus"

            ;; UNIX extract version specific name, e.g., sicstus-3.10.0 (+ possible beta suffix)
            (let ((sicstus-version-suffix (and (string-match "/sicstus-\\([0-9]+-[0-9]+-[0-9]+[^/]*\\)/emacs/.*$"
                                                             ;; ..../lib/sicstus-3.10.0/emacs/
                                                             sicstus-emacs-directory
                                                             )
                                               (match-beginning 1)
                                               (match-end 1)
                                               (substring sicstus-emacs-directory 
                                                          (match-beginning 1)
                                                          (match-end 1)))))
            
              (if sicstus-version-suffix
                  (concat "sicstus" "-" sicstus-version-suffix)
                "sicstus"))))
        
    
         (sicstus-exe-path (and sicstus-bin-directory
                                (expand-file-name 
                                 (concat sicstus-program-basename sicstus-exe-suffix)
                                 sicstus-bin-directory))))

    ;; make sure the SICStus emacs files are found before anything else
    (if sicstus-emacs-directory
        (setq load-path
              (cons sicstus-emacs-directory
                    load-path)))
    ;; Ensure M-X run-prolog and opening prolog files loads the prolog mode.
    (autoload 'run-prolog "prolog" "Start a (SICStus) Prolog sub-process." t)
    (autoload 'prolog-mode "prolog" "Major mode for editing (SICStus) Prolog programs." t)

    ;; Tell the prolog mode that we should use the SICStus extensions
    (setq prolog-system 'sicstus)

    ;; Tell Emacs that .pro and .pl are prolog files (By default Emacs
    ;; assumes .pl is Perl)
    (setq auto-mode-alist (append '(
                                    ("\\.pro$" . prolog-mode) ; [PM] 4.0
                                    ("\\.pl$" . prolog-mode)
                                    )
                                  auto-mode-alist))

    ;; Tell the run-prolog where to find the sicstus executable
    (if (and sicstus-exe-path
	     ;; [PD] 4.0.0beta1
	     ;; By setting inhibit-sicstus-executable-check to t you can skip
	     ;; the executable check. This is for SICStus developers who wants
	     ;; to be able to have a correct setup without having built SICStus.
	     (or (and (boundp 'inhibit-sicstus-executable-check)
		      inhibit-sicstus-executable-check)
		 (file-executable-p sicstus-exe-path)))
        (setq sicstus-program-name sicstus-exe-path))

    ;; Ensure the SICStus Prolog mode can find the on-line documentation
    (sicstus-init-setup-info sicstus-info-dir)

    ;; Put the SICStus exe folder on the PATH for processes (such as
    ;; M-x shell) started from Emacs.
    ;; Among other things this helps if using SICStus from Java

    (if (not (and (boundp 'inhibit-sicstus-bin-directory-on-PATH)

                  ;; By setting
                  ;; inhibit-sicstus-bin-directory-on-PATH to 't you
                  ;; can inhibit the PATH hacking. This is mostly
                  ;; for the SICStus maintainers while debugging
                  ;; SICStus.
                  inhibit-sicstus-bin-directory-on-PATH))
        (progn
          (setq exec-path (cons sicstus-bin-directory exec-path))
          (setenv "PATH" (concat sicstus-bin-directory path-separator (getenv "PATH")))))
    ))

(sicstus-init-emacs (and (boundp 'load-file-name) load-file-name))
