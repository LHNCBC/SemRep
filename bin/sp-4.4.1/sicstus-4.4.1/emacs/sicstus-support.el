;; -*- Mode: Emacs-Lisp; dummy:""; buffer-read-only:t; dummy2:""; -*-

;; [PM] 1.43 The strange EMACS_MODE_LINE_BUFFER_READONLY is
;; substituted by configure when generating sicstus-support.el to make
;; it read-only. See configure.in


;; sicstus-support.el -- SICStus specific support for prolog.el
;;; Copyright (C) 1998, 1999 SICS and IQSoft
;;
;; Author: Rozman Tamas, IQSoft
;;         Emil Astrom, SICS <emil@sics.se>
;; Maintainer: Per.Mildner@sics.se
;; Version: 1.43 (for SICStus Prolog 3.12.1)
;; (update sicstus-support-version below)
;;
;; Maintainer: Per.Mildner@sics.se
;;
;; Should work with FSF/GNU Emacs as well as XEmacs.
;;
;; This package depends on being automatically loaded from prolog.el
;; when prolog-system is set to sicstus.
;; 

;; To use this put the following in your ~/.emacs file
;; (load "SICSTUS_EMACS_PATH/sicstus_emacs_init")
;; where SICSTUS_EMACS_PATH is the folder containing this file and
;; sicstus_emacs_init.el
;;
;;
;; Obsolete:
;; To use this put the following in your ~/.emacs file
;; (setq load-path (cons (expand-file-name "/usr/local/lib/sicstus3/emacs") load-path))
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;; (setq prolog-system 'sicstus) ; This will ensure that sicstus-sup is loaded!
;; (setq auto-mode-alist (cons '("\\.pl$" . prolog-mode) auto-mode-alist))

;; where the path in the first line is the file system path to the
;; directory containing this file as well as prolog.el.
;; MS Windows paths can be written like "c:/sicstus3/emacs".
;;
;; The last line above makes sure that files ending with .pl are
;; assumed to be Prolog files and not Perl, which is the default Emacs
;; setting. If this is not wanted, remove that line. It is then necessary
;; for the user to manually switch to prolog mode by typing
;; M-x prolog-mode after opening a prolog file.
;;
;; If the shell command to start the prolog process ('sicstus') is not
;; available in the default path, then it is necessary to set the
;; value of the environment variable EPROLOG to a shell command to
;; invoke the prolog process.

;;; Version history
;; 1.4beta (with SICStus 3.9b4)

;; . Update for new message syntax (when not
;;   prolog-use-standard-consult-compile-method-flag)
;; . added det, nondet as keywords
;; . Moved some code here that did not belong in prolog.el
;;
;; 1.3 Released (as sicstus-support.el) with SICStus 3.8
;; . Now preserves existing process filter of the inferior prolog
;;   buffer. This is good since it allows composing of filter based
;;   features.
;; . Generalize, clean-up
;; . Better support for XEmacs
;; . Sending commands to SICStus now works when debugger is waiting
;;   for input (SICStus >= 3.8)
;; . C-X SPC now sets and clears line breakpoints using the
;;   programmable debugger interface of SICStus 3.8.
;; . `M-x sicstus-download-new-packages' checks for and downloads new
;;   versions of emacs lisp code and Info files.
;; 
;; 1.2pre (under the name pltrace.el): ?
;; 1.1 (under the name pltrace.el): Released with SICStus Prolog 3#7


(require 'prolog)

;; The overlay stuff is in a separate package under XEmacs. In FSF Emacs
;; this is builtin.
(if (string-match "XEmacs\\|Lucid" emacs-version)
    (require 'overlay))

(eval-when-compile
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (require 'overlay)))


;; [MC] 4.3 bumped version to 4.3 for SP 4.3
(defconst sicstus-support-version '(4 . 3)
  "Version of sicstus-support library as a pair (MAJOR . MINOR)")

;; [PM] 3.11.1 configure fills in version info here
(defvar sicstus-version '(4 . 4)
  "*The SICStus Prolog version used. In the format (MAJOR . MINOR).")

(defvar sicstus-keywords
  '("block" "discontiguous" "dynamic" "initialization"
    "meta_predicate" "mode" "module" "multifile" "public" "volatile"
    "det" "nondet"                      ; for spdet
    )
  "*Keywords which is used for font locking of directives.")

(defvar sicstus-program-name "sicstus"
  "*Program name for invoking an inferior SICStus Prolog with `run-prolog'.")

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



(defun sicstus-emacs-utf-8-coding-system ()
  "Returns coding system (as a symbol) for UTF-8 or nil if not supported"
  (let ((name 'utf-8))
    (cond ((and (fboundp 'coding-system-p)
                (coding-system-p name))
           name)
          ((and (fboundp 'find-coding-system)
                (find-coding-system name))
           name))))

(defvar sicstus-program-switches (append
                                  ;; [PM] 4.0.1 Use UTF-8 for stdio if Emacs has it. xref sicstus-support-inferior-prehook
                                  (if (sicstus-emacs-utf-8-coding-system)
                                      '("-DSP_CTYPE_STDIO=utf8")
                                    '())
                                        ; [PM] 4.2 Primarily for Mac OS X where ulimit -d is not unlimited by default
                                  '("-DSP_ULIMIT_DATA_SEGMENT_SIZE=unlimited")
                                  '("-i"))
  "*Switches given to inferior SICStus Prolog run with `run-prolog'.")

(defvar pltrace-cmd-prefix
  ":-\n"
  "Magic string prefixed to commands sent to prolog.")


(defvar sicstus-consult-string
  (concat pltrace-cmd-prefix
          "prolog:zap_file(%m,%b,consult,%l).")
  "*See documentation of prolog-consult-string.")

(defvar sicstus-compile-string 
  (concat pltrace-cmd-prefix
          "prolog:zap_file(%m,%b,compile,%l).")
  "*See documentation of prolog-compile-string.")

(defvar sicstus-prompt-regexp "| [ ?][- ] *"
  "*Regexp matching the command line prompt.")

(defvar sicstus-continued-prompt-regexp "^\\(| +\\|     +\\)"
  "*Regexp matching the prompt when consulting `user'.")

(defvar sicstus-module-directive-pattern
  '(":-\\s-*module(\\s-*'?\\(\\w+\\)'?\\W*" . 1)
  "Pair (REGEXP . MATCH-NO) where regexp REGEXP matches a module directive.
(match-string MATCH-NO) is the module name.")

(defvar sicstus-help-function 'sicstus-find-documentation
  "*Function to lookup documentation of a predicate.")

(defvar pltrace-mode-map
  (if (boundp 'prolog-mode-map-sicstus)
      prolog-mode-map-sicstus
    prolog-mode-map))


;;; On-line file updates [PM]

(defvar sicstus-installation-notes)


(defvar sicstus-elisp-online-path 
  (concat "/anonymous@ftp.sics.se:"
          "/archive/sicstus4/emacs_%M_%m")
  "*Location, as an ftp pathname, of the sicstus elisp files.
%M and %m in this string will be substituted with MAJOR and MINOR respectively.
Where `sicstus-version' is (MAJOR . MINOR)
Default \"/anonymous@ftp.sics.se:/archive/sicstus4/emacs_%M_%m\".")


(defvar sicstus-info-online-path
  (concat "/anonymous@ftp.sics.se:"
          "/archive/sicstus4/docs/%M.%m"
          "/"
          )
  "*Location, as an ftp pathname, of the sicstus info files.
Default \"/anonymous@ftp.sics.se:/archive/sicstus4/docs/%M.%m/\".
%M and %m in this string will be substituted with MAJOR and MINOR respectively.
Where `sicstus-version' is (MAJOR . MINOR)")


;; (defvar sicstus-downloadable-packages
;;   '(("sicstus-support" sicstus-support-version (eval . (sicstus-substitute-version-numbers sicstus-elisp-online-path sicstus-version)) ".el" sicstus-elisp-postinstall)
;;     ("prolog" prolog-mode-version (eval . (sicstus-substitute-version-numbers sicstus-elisp-online-path sicstus-version)) ".el" sicstus-elisp-postinstall)
;;     ("info" (eval . (sicstus-info-installed-version)) (eval . (sicstus-substitute-version-numbers sicstus-info-online-path sicstus-version)) "/" sicstus-info-postinstall)
;;     )
;;   "List of SICStus related packages downloadable from SICS
;; Each element is a list (PACKAGE VERSION PATH EXT), where PACKAGE is
;; the name of the package as a string, e.g., \"sicstus-support\";
;; VERSION specifies the presently installed version as a pair (MAJOR . MINOR), 
;; VERSION can also be a symbol bound to such a pair.
;; PATH is a path to the directory where new versions can be found,
;; typically using ftp pathname syntax to allow downloading over the net,
;; e.g., \"/anonymous@foo.bar.com:/pub/downloads/\"
;; EXT is optional and specifies the file extension (defaults to \".el\".")

(defun sicstus-substitute-version-numbers (string version)
  (let ((major (car version))
        (minor (cdr version))
        (case-fold-search nil))
    (while (string-match "%M" string)
      (setq string (replace-match (int-to-string major) t t string)))
    (while (string-match "%m" string)
      (setq string (replace-match (int-to-string minor) t t string))))
  string)

(defun sicstus-info-postinstall (dest &optional version pretty-name)
  (let* ((Info-var (or (get 'Info-default-directory-list
                            'byte-obsolete-variable)
                       'Info-default-directory-list))
         (orig (symbol-value Info-var)))
    (setq Info-default-directory-list (append Info-default-directory-list
                                              (list dest)))
    (if Info-directory-list             ; if nil then not yet inited from default
        (or (member dest Info-directory-list)
            (setq Info-directory-list (append Info-directory-list
                                              (list dest)))))
      
    (setq Info-dir-contents nil)        ; force recompute Info dir 
    ;; (Info-goto-node "(sicstus)")
    (and orig
         (let ((msg (concat 
                     (format "Installed %s SICStus Info files in \"%s\"\n"
                             (if version
                                 (format "version %s.%s of"
                                         (car version) (cdr version))
                               "")
                             dest)
                     (format "%%%% To ensure that %s is available,\n"
                             (or pretty-name "the SICStus Manual"))
                     "%% add the following to your `.emacs' file:\n"
                     (format "(setq %s\n" Info-var)
                     (format "   (append %s '(\"%s\")))\n\n" Info-var dest))))
           (setq sicstus-installation-notes
                 (cons msg sicstus-installation-notes))
           (with-output-to-temp-buffer "*Installation Notes*"
             (princ msg))))))


(defun sicstus-info-installed-version ()
  "Attempt to obtain the version of the installed Info SICStus Manual."
  (condition-case nil
      (save-window-excursion
        (save-excursion
          (Info-goto-node "(sicstus)Top")
          (save-restriction
            (widen)
            (goto-char (point-min))
            (if (re-search-forward
                 "[gG]enerated \\([123]?[0-9]\\) \\(Jan\\|Feb\\|Mar\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[a-z]* \\([12][0-9][0-9][0-9]\\)"
                 1000
                 t)
                (let ((day (string-to-int (match-string 1)))
                      (month (length (member (match-string 2)
                                             '("Dec" "Nov" "Oct" "Sep"
                                               "Aug" "Jul" "Jun" "May"
                                               "Mar" "Feb" "Jan"))))
                      (year (string-to-int (match-string 3))))
                  (cons year (+ (* month 100) day)))
              sicstus-version))))
    (error nil)))


;; (define-key pltrace-mode-map [(control c) (space)] 'pltrace-break)

(defvar pltrace-process "prolog"
  "Name of prolog process.")

(defvar pltrace-debug nil
  "*Non-nil to debug the pltrace package.")

(defvar pltrace-cmd-prefix
  "\t"
  "Magic string prefixed to commands sent to prolog.")

(defun pltrace-debug-msg (msg)
  (and pltrace-debug
       (message "PLTRACE: %s" msg)))

(defun pltrace-send-cmd (goal)
  "Send GOAL to prolog. GOAL must not have any terminating period."
  (prolog-ensure-process)
  (let ((cmd (format "%s%s.\n" pltrace-cmd-prefix goal)))
    (pltrace-debug-msg cmd)
    (process-send-string pltrace-process cmd)))


(defun pltrace-break (arg &optional action)
  "Set a break-point at the current source line.
Numeric argument (C-U) removes breakpoint for this line.
Negative argument (C-U -) removes all breakpoints for this file.
When called noninteractively optional ACTION should be one of
set, clear, clear_all in which case ARG is ignored"
;; temp not documented since it is same as set in traceui.
;; set, temp, clear, clear_all
  (interactive "P")
  (let ((file (prolog-bsts buffer-file-name))
        (line (save-restriction
                (widen)
                (beginning-of-line)
                (1+ (count-lines 1 (point))))))
    (or action
        (setq action (cond ((null arg)
                            'set)
                           ((> (prefix-numeric-value arg) 0)
                            'clear)
                           (t 'clear_all))))
    (pltrace-send-cmd (format "prolog:lbp_breakpoint_emacs(%s,'%s',%d)" action file line))))




;; executes a Prolog-trace command
;;
;; the format of a command:
;;
;;   <begin>c,Buffer
;;   Text
;;   <end>
;;        Creates a buffer named Buffer, or empties it if it already
;;        exists; and deposits Text into it.
;;
;;  <begin>f,Port,Line,File<end>
;;        Shows line Line in file File with an overlay corresponding to Port.
;;
;;  <begin>b,Port,Line,Buffer<end>
;;        Shows line Line in buffer Buffer with an overlay corresponding to
;;        Port.
;;
;;  <begin>o,Cov,Line,File<end>
;;        Shows line Line in file File with an overlay corresponding to Cov (#hits)
;;
;;  <begin>r,Buffer<end>
;;        Shows buffer Buffer.
;;
;;  <begin>u<end>
;;        Removes the last highlight made by ..._show
;;
;;  <begin>k,Buffer<end>
;;        Kills buffer named Buffer created by a c (create_buffer) command.
;;

(defvar pltrace-command-alist
  '(
    (?e . pltrace-execute-breakpoint-command)
    (?f . pltrace-execute-normal-command-wrapper)
    (?b . pltrace-execute-normal-command-wrapper)
    (?u . pltrace-execute-normal-command-wrapper)
    (?k . pltrace-execute-normal-command-wrapper)
    (?r . pltrace-execute-normal-command-wrapper)
    (?o . pltrace-execute-normal-command-wrapper)
    (?c . pltrace-execute-extended-command)))
  

(defun pltrace-execute-normal-command-wrapper (command-char param-list ignore)
  (pltrace-execute-normal-command command-char param-list))

;; this is called by the process filter pltrace-filter, so
;; proc and global variable pltrace-process is always the same
;; (defvar foo nil)
(defun pltrace-execute-command (proc command)
  (if (string-match "\\(.*\\)\\(\n\\)?" command)
      (let* ((first-line (substring command 0 (match-end 1)))
             (text (and (match-end 2) (substring command (match-end 2))))
             ;; (pos (pltrace-find command "\n"))
             ;; (first (substring command 0 pos))
             ;; (second (substring command pos))
             (param-list (pltrace-split-command first-line))
             (command-char (car param-list))
             (ent (assoc (elt command-char 0) pltrace-command-alist))
             (handler-fun (cdr ent)))
        (if ent
            (progn
              (pltrace-debug-msg (format "%s" (list handler-fun command-char (cdr param-list) text)))
              (pltrace-apply-command handler-fun command-char (cdr param-list) text))))))


(defun pltrace-execute-breakpoint-command (command-char param-list ignore)
  (let* ((what (nth 0 param-list))
         (line (nth 1 param-list))
         (file (nth 2 param-list)))
    (cond ((equal what "lbp_set")
           (pltrace-mark-breakpoint file 'set)
           (message "Breakpoint: line %s@%s" line (file-name-nondirectory file)))
          ((equal what "lbp_temp")
           (pltrace-mark-breakpoint file 'temp)
           (message "Breakpoint: line %s@%s" line (file-name-nondirectory file)))
          ((equal what "lbp_clear")
           (pltrace-mark-breakpoint file 'clear)
           (message "Breakpoint removed: line %s@%s" line (file-name-nondirectory file)))
          ((equal what "lbp_clear_all")
           (pltrace-mark-breakpoint file 'clear_all)
           (message "Breakpoints removed: %s" (file-name-nondirectory file)))
          ((equal what "lbp_error")
           (message "Could not set breakpoint on line %s@%s" line (file-name-nondirectory file))))))
           


(defun pltrace-mark-breakpoint (file kind)
  ;; dummy
  )
  
(defun pltrace-apply-command (fun &rest args)
  ;; Would like this to be interruptible (which is not the case when
  ;; run from a process filter) Unfortunately I did not get Idle
  ;; timers to work in XEmacs 21.1.7
  (apply fun args))

; SPRM 8662 used to invert the meaning of arg
(defun sicstus-debug-cmd (arg on_cmd off_cmd)
  (if arg
      (pltrace-send-cmd off_cmd)
    (pltrace-send-cmd on_cmd)))
      

(defun sicstus-debug-on (&optional arg)
    (interactive "P")
    (sicstus-debug-cmd arg "debug" "nodebug"))
(put 'prolog-debug-on 'sicstus 'sicstus-debug-on)


(defun sicstus-debug-off ()
  (interactive)
  (sicstus-debug-on t))
(put 'prolog-debug-off 'sicstus 'sicstus-debug-off)

(defun sicstus-trace-on (&optional arg)
    (interactive "P")
    (sicstus-debug-cmd arg "trace" "notrace"))
(put 'prolog-trace-on 'sicstus 'sicstus-trace-on)
  
(defun sicstus-trace-off ()
  (interactive)
  (sicstus-trace-on t))
(put 'prolog-trace-off 'sicstus 'sicstus-trace-off)

(defun sicstus-zip-on (&optional arg)
  (interactive "P")
  (sicstus-debug-cmd arg "zip" "nozip"))
(put 'prolog-zip-on 'sicstus 'sicstus-zip-on)

; [PM] 1.43 Was missing
(defun sicstus-zip-off ()
  (interactive)
  (sicstus-zip-on t))
(put 'prolog-zip-off 'sicstus 'sicstus-zip-off)

; [MC] 4.2
(defun sicstus-bindings-on (&optional arg)
  "Enable automatic bindings at debug prompts.
When called with prefix argument ARG, disable automatic bindings instead."
    (interactive "P")
    (sicstus-debug-cmd arg "prolog:set_auto_bindings(on)" "prolog:set_auto_bindings(off)")
    (let ((buffer-name "*Prolog Bindings*"))
      (let ((cb (current-buffer))
	    (pb (get-buffer buffer-name)))
	(cond ((and arg pb)
	       (save-excursion
		 (delete-window (get-buffer-window pb))
		 (kill-buffer pb))) 
	      ((and (not arg) (not pb))
	       (let ((new-buffer (get-buffer-create buffer-name)))
		 (save-excursion
                   (and (get-buffer-window "*prolog*") ; [PM] 4.2
                        (select-window (get-buffer-window "*prolog*")))
		   (split-window-vertically)
		   (switch-to-buffer-other-window new-buffer)
		   (select-window (get-buffer-window cb)))))))))

(defun sicstus-bindings-off ()
  "Disable automatic bindings at debug prompts."
  (interactive)
  (sicstus-bindings-on t))

(defun sicstus-bindings-print-depth (&optional max)
  "Set print depth for automatic bindings at debug prompts and top-level."
  (interactive "nprint depth: ")
  (pltrace-send-cmd (format "prolog:new_printdepth(toplevel,%s)" max)))

(defun sicstus-coverage-on (&optional arg)
  "Refresh code coverage highlights in the current buffer.
When called with prefix argument ARG, clear code coverage highlights instead."
    (interactive "P")
    ;;(remove-overlays (point-min) (point-max) 'coverage t) ;requires Emacs 21.4
    (delete-overlay-loop (overlays-in (point-min) (point-max)))
    (if (not arg)
	(pltrace-send-cmd (format "prolog:emacs_coverage('%s')"
				  (prolog-bsts buffer-file-name)))))

(defun delete-overlay-loop (olays)
  (cond ((null olays) t)
	(t (cond ((overlay-get (car olays) 'coverage)
		  (delete-overlay (car olays))))
	   (delete-overlay-loop (cdr olays)))))		   

(defun sicstus-coverage-off ()
  "Clear code coverage highlights in the current buffer."
  (interactive)
  (sicstus-coverage-on t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help function with completion
;; Originally from Per Mildner's SICStus debugger mode modified by SICS and perhaps others

(defvar sicstus-info-predicate-index
  "(sicstus)Predicate Index"
  "*The info node for the SICStus predicate index.")

(defvar sicstus-info-alist nil 
  "Alist with all builtin predicates.
Only for internal use by `sicstus-find-documentation'")

(defun sicstus-find-documentation (pred)
  "Go to the Info node for a predicate in the SICStus Info manual."
  (interactive
   (list (sicstus-read-predicate
          nil
          nil
          (or sicstus-info-alist
              (sicstus-build-info-alist))
          )))
  (if (and pred (not (equal pred "")))
      (sicstus-goto-predicate-info pred)))


(defun sicstus-goto-predicate-info (predicate)
  "Go to the info page for PREDICATE, which is a PredSpec."
  (require 'info)
  (if (string-match "\\(.*\\)/\\([0-9]+\\).*$" predicate)
      (let ((buffer (current-buffer))
            (name (match-string 1 predicate))
            (arity (match-string 2 predicate)))
        (setq arity (string-to-number arity))
        (pop-to-buffer nil)

        (Info-goto-node 
         sicstus-info-predicate-index);; We must be in the SICStus pages
          
        ;; [PM] If there are multiple entries then pick the first
        ;;      where pred appears to be defined as opposed to
        ;;      tutorial entries etc, the latter will err since
        ;;     `PREDNAME/ARITY' is not found
        ;; Was:
        ;; (Info-goto-node (car (cdr (assoc predicate sicstus-info-alist))))
        ;; (prolog-find-term (regexp-quote name) arity "^`")
        (let ((nodes (cdr (assoc predicate sicstus-info-alist)))
              (node-info nil))          ; (NODE . LINE-OR-NIL)
          (while (and nodes (null node-info))
            (setq node-info (car nodes)
                  node (car node-info)
                  line (cdr node-info)
                  nodes (cdr nodes))
            (condition-case nil
                (progn
                  ;; perhaps this could err as well if node not found
                  (Info-goto-node node)
                  (goto-char (point-min))
                  (if line
                      (forward-line (- (string-to-int line)
                                       1
                                       ;; adjust for the fact that
                                       ;; the texi sources places
                                       ;; PLindex on the line after
                                       ;; the predicate
                                       1
                                       ))
                    ;; [PM] 3.12.0+ Simplified to just look for ^`NAME(
                    (re-search-forward (message (concat "^`" (regexp-quote name) (if (zerop arity) "[' \t]" "("))))
                    ))
              (error
               (setq node nil))))
          (if node
              (recenter 0))
          (pop-to-buffer buffer)))
    (error "Predicate arity not specified for '%s'" predicate) ; [PM]
    ))

(defun sicstus-read-predicate (&optional prompt default alist)
  "Read a PredSpec from the user.
Optional argument PROMPT is used to prompt, if nil then \"Help on predicate:\" is used
Optional DEFAULT is used as an initial quess, if nil then prolog-atom-under-point is used.
Optional ALIST if non-nil is used for completing read.
Returned value is a string \"FUNCTOR/ARITY\"."
  (let ((initial (or default (prolog-atom-under-point)))
        (prompt (or prompt "Help on predicate: "))
	answer)

    ;; Test if the initial string could be the base for completion.
    ;; Discard it if not.
    (if (and alist                      ; only if completion used
             (null (try-completion initial sicstus-info-alist)))
	(setq initial ""))
    ;; Read the PredSpec from the user
    (setq answer
          (if alist
              (completing-read
               prompt
               sicstus-info-alist nil t initial)
            (read-from-minibuffer
             prompt
             initial)))

    (if (equal answer "")
	initial
      answer)))

(defun sicstus-build-info-alist (&optional verbose)
  "Build and return sicstus-info-alist. An alist of all predicates.
Each element is of the form
(\"NAME/ARITY\" . ( ( INFO-NODE1 . LINE1-OR-NIL) ( INFO-NODE2 . LINE2-OR-NIL ) ...)).
Typically there is just one Info node associated with each name
If an optional argument VERBOSE is non-nil, print messages at the beginning
and end of list building."
  (if verbose
      (message "Building info alist..."))
  (setq sicstus-info-alist
        (let ((l ())
              (last-entry (cons "" ())))
          (save-excursion
            (save-window-excursion
              ;; select any window but the minibuffer (as we cannot switch
              ;; buffers in minibuffer window.
              ;; I am not sure this is the right/best way
              (if (active-minibuffer-window) ; nil if none active
                  (select-window (next-window)))
              ;; Do this after going away from minibuffer window
              (save-window-excursion
                (info))
              (Info-goto-node sicstus-info-predicate-index)
              (goto-char (point-min))
              (while (re-search-forward
                      ;; "^\\* \\(.+\\)/\\([0-9]+\\)\\([^\n:*]*\\):"
                      ;; ;; [PM] Ignore sequence number <n>  following duplicate entries
                      ;; "^\\* \\(.+\\)/\\([0-9]+\\)\\([^\n:*<]*\\)\\( <[0-9]+>\\)?:"
                      ;; [PM] 3.12.0+ SPRM 8472 match [ARITIES] entries
                      "^\\* \\(.+\\)/\\(\\([0-9]+\\)\\|\\(\\[\\([0-9,]+\\)\\]\\)\\)\\([^\n:*<]*\\)\\( <[0-9]+>\\)?:"
                      nil t)
                (let* ((name (match-string 1))
                       (arities-string (or (match-string 3) (match-string 5)))
                       (comment (match-string 6))
                       (line-string (match-string 7))
                                        ; (fa (format "%s/%d%s" name arity comment))
                       (fas '())
                       info-node)
                  ;; arities-string is comma separated arities (by convention ordered by increasing arity)
                  (let ((i 0))
                    (while (string-match "\\([0-9]+\\),?" arities-string i)
                      (setq fas (cons (format "%s/%s%s" name (match-string 1 arities-string) comment) fas)
                            i (match-end 0)))
                    (setq fas (nreverse fas)))

                  
                  (beginning-of-line)
                  ;; Extract the info node name ([PM] 3.12.0+ and line for texinfo 4.7
                  (if (re-search-forward "[^\n]*:[ \t]*\\([^:]+\\)\\.[ \t\n]*\\((line[\t ]+\\([0-9]+\\))\\)?$"
                                         nil 'noerror)

                      (let* ((node-name (match-string 1))
                             (node-line (match-string 3)) ; may be nil
                             (node (cons node-name node-line)))
                        (while fas
                          (let ((fa (car fas)))
                            (if (equal fa (car last-entry))
                                (setcdr last-entry (cons node (cdr last-entry)))
                              (setq last-entry (cons fa (list node))
                                    l (cons last-entry l)))
                            (setq fas (cdr fas)))))
                    )))))
          (nreverse l)
          ))
  (if verbose
      (message "Building info alist... done."))
  sicstus-info-alist)

;; region-exists-p is not in FSF Emacs.
(if (fboundp 'region-exists-p)
    (defalias 'sicstus-region-exists-p 'region-exists-p)
(defun sicstus-region-exists-p ()
  (condition-case nil
      (and (mark) t)                    ; in FSF this errs if no region
    (error nil))))


;; (defun sicstus-download-new-packages (&optional packages)
;;   "EXPERIMENTAL Check if there are new versions of sicstus related packages available for download.
;; See `sicstus-downloadable-packages'"
;;   (interactive)
;;   (setq packages (or packages sicstus-downloadable-packages))
;;   (let ((sicstus-installation-notes '()))
;;     (while packages
;;       (let* ((info (car packages))
;;              (package (nth 0 info))
;;              (current-version (let ((v (nth 1 info)))
;;                                 (if (and (consp v) (eq (car v) 'eval))
;;                                     (setq v (eval (cdr v))))
;;                                 (if (and (symbolp v) (boundp v))
;;                                     (setq v (symbol-value v)))
;;                                 (and (consp v)
;;                                      (numberp (car v))
;;                                      (numberp (cdr v))
;;                                      v)))
;;              (source-dir (let ((d (nth 2 info)))
;;                            (if (and (consp d) (eq (car d) 'eval))
;;                                (setq d (eval (cdr d))))
;;                            (if (and (symbolp d) (boundp d))
;;                                (setq d (symbol-value d)))
;;                            (and (stringp d)
;;                                 d)))
;;              (extension (nth 3 info))
;;              (postinstall (nth 4 info)))
;;         (message "Checking `%s'" package)
;; 
;;       
;; 
;;         (let* ((newer-version (sicstus-check-for-new-version 
;;                                package current-version source-dir extension)))
;;           (if (and
;;                newer-version
;;                (yes-or-no-p (format "A newer version (%d.%d) of `%s' exists. Download? "
;;                                     (car (car newer-version))
;;                                     (cdr (car newer-version))
;;                                     package)))
;;               (sicstus-download-install-package package newer-version extension nil nil postinstall))))
;;       (setq packages (cdr packages)))
;;     (if sicstus-installation-notes
;;         (with-output-to-temp-buffer "*Installation Notes*"
;;           (princ "%% Installation summary:\n\n")
;;           (let ((msgs (reverse sicstus-installation-notes)))
;;             (while msgs
;;               (princ (car msgs))
;;               (setq msgs (cdr msgs))))))))


;; (defun sicstus-check-for-new-version (package current-version source
;;                                               &optional 
;;                                               extension
;;                                               match maj-match min-match)
;;   (setq extension (or extension ".el")
;;         match (or match "-\\([0-9]+\\)_\\([0-9]+\\(\\.[0-9]+\\)?\\)")
;;         maj-match (or maj-match 1)
;;         min-match (or min-match 2))
;;   (let* ((current-major (car current-version))
;;          (current-minor (cdr current-version))
;;          (newest-info (sicstus-get-newest-version-info
;;                        source
;;                        (concat (regexp-quote package)
;;                                match
;;                                (if (equal extension "/")
;;                                    "" ; (concat "\\(" (regexp-quote extension) "\\)?")
;;                                  (regexp-quote extension)))
;;                        maj-match
;;                        min-match))
;;          (version (car newest-info))
;;          (major (car version))
;;          (minor (cdr version)))
;;     (and major
;;          minor
;;          (or (null current-version)
;;              (> major current-major)
;;              (and (= major current-major)
;;                   (> minor current-minor)))
;;          newest-info)))

;; (defun sicstus-maybe-download-new-version (package current-version source
;;                                                    &optional extension
;;                                                    default-save-dir
;;                                                    default-save-name)
;;
;;   (let* ((newer-version (sicstus-check-for-new-version 
;;                         package current-version source extension)))
;;     (if (and
;;          newer-version
;;          (yes-or-no-p (format "A newer version (%d.%d) of `%s' exists. Download? "
;;                               (car (car newer-version))
;;                               (cdr (car newer-version))
;;                               package)))
;;         (sicstus-download-install-package package newer-version 
;;                                           default-save-dir default-save-name))))


;; (defun sicstus-download-install-package (package
;;                                          newer-version
;;                                          &optional
;;                                          extension
;;                                          default-save-dir
;;                                          default-save-name
;;                                          postinstall)
;;   (let* ((source (cdr newer-version))
;;          (extension (or extension
;;                         (let ((base-name (file-name-nondirectory source)))
;;                           (cond ((fboundp 'file-name-extension)
;;                                  ;; not in Emacs 19.34
;;                                  (file-name-extension source t))
;;                                 ((string-match "\\.[^./\\]*$" base-name)
;;                                  (match-string 0 base-name))
;;                                 (t "")))))
;;          (elisp (equal extension ".el"))
;;          (dirp (equal extension "/"))
;;          (old-path (and elisp
;;                         (fboundp 'find-library)
;;                         (locate-library package)))
;;          ;; FIX THIS (if dirp)
;;          (old-source-path (and old-path ; likely XXX.elc
;;                                (concat (file-name-sans-extension old-path)
;;                                        extension)))
;; 
;;          ;; FIX THIS (if dirp)
;;          (default-save-dir (or default-save-dir
;;                                (and old-source-path
;;                                     (file-name-directory old-source-path))))
;;          ;; FIX THIS (if dirp)
;;          (default-save-name (or default-save-name
;;                                 (and old-source-path
;;                                      (file-name-nondirectory old-source-path))
;;                                 (concat package extension)))
;;          (dest
;;           (if (and dirp (fboundp 'read-directory-name))
;;               (read-directory-name "Save new version in:"
;;                                    default-save-dir
;;                                    nil
;;                                    nil
;;                                    default-save-name)
;;             (read-file-name "Save new version as:"
;;                             default-save-dir
;;                             default-save-name
;;                             nil
;;                             default-save-name))))
;;     (if dest
;;         (progn
;;           (setq dest (expand-file-name dest default-save-dir))
;;           (if dirp
;;               (sicstus-copy-dir source dest)
;;             (copy-file source dest 42 t))
;; 
;; 
;;           (cond (postinstall
;;                  (funcall postinstall dest))
;;                 (elisp (sicstus-elisp-postinstall dest)))))))


;; (defun sicstus-copy-dir (source dest &optional match overwrite)
;;   (setq dest (file-name-as-directory dest)
;;         source (file-name-as-directory source))
;;   ;; If ftp download does not work it should happen in directory-files
;;   (let* ((files-only t)                 ; for now
;;          (files (directory-files source nil match t
;;                                  ;; efs ignores files-only arg and FSF
;;                                  ;; Emacs 19.34.1 lacks it
;;                                  ;; files-only
;;                                  ))
;;          (downloaded 0)
;;          (overwrite-next nil))
;;     (and files
;;          (progn
;;            (while files
;;              ;; (message (format "Downloading file %d/%d" i numfiles))
;;              (let ((efs-verify-anonymous-modtime t)
;;                    (source-file (concat source (car files)))
;;                    (dest-file (concat dest (car files))))
;; 
;;                (condition-case nil
;;                    (progn
;;                      (if (and files-only
;;                               ;; efs ignores files-only arg to list-directory
;;                               (not (file-directory-p source-file)))
;;                          (progn
;;                            (copy-file source-file
;;                                       dest-file
;;                                       (or overwrite-next
;;                                           (memq 'all overwrite))
;;                                       t       ; keep date
;;                                       )
;;                            (setq downloaded (1+ downloaded))))
;;                      (setq overwrite-next nil)
;;                      (setq files (cdr files)))
;;                  (file-already-exists
;;                   (cond ((file-newer-than-file-p source-file dest-file)
;;                          (if (or (memq 'all overwrite)
;;                                  (memq 'older overwrite)
;;                                  (and
;;                                   (not (or (memq 'not-older overwrite)
;;                                            (memq 'not-all overwrite)))
;;                                   (y-or-n-p "Overwrite older files? ")))
;;                              ;; should use adjoin
;;                              (setq overwrite (cons 'older overwrite)
;;                                    overwrite-next t)
;;                            (setq overwrite (cons 'not-older overwrite)
;;                                  files (cdr files))))
;;                         ((file-newer-than-file-p dest-file source-file)
;;                          (if (or (memq 'all overwrite)
;;                                  (memq 'newer overwrite)
;;                                  (and
;;                                   (not (or (memq 'not-newer overwrite)
;;                                            (memq 'not-all overwrite)))
;;                                   (yes-or-no-p "Overwrite newer files? ")))
;;                              (setq overwrite (cons 'newer overwrite)
;;                                    overwrite-next t)
;;                            (setq overwrite (cons 'not-newer overwrite)
;;                                  files (cdr files))))
;;                         (t
;;                          (if (or (memq 'all overwrite)
;;                                  (and
;;                                   (not (memq 'not-all overwrite))
;;                                   (yes-or-no-p "Overwrite existing files? ")))
;;                              (setq overwrite (cons 'all overwrite))
;;                            (setq overwrite (cons 'not-all overwrite)
;;                                  files (cdr files)))))
;;                   ))))
;;            downloaded))))


;; (defun sicstus-elisp-postinstall (elisp-file &optional version)
;;   (let (bc-file)
;;     (if (and (file-exists-p elisp-file)
;;              (yes-or-no-p (format "Compile `%s'? " 
;;                                   (file-name-nondirectory
;;                                    elisp-file)))
;;              (byte-compile-file elisp-file) ; nil if error
;;              (setq bc-file (concat elisp-file "c"))
;;              nil                        ; do not load
;;              (file-exists-p bc-file)
;;              (yes-or-no-p (format "Load `%s'? " bc-file)))
;;         (load bc-file nil nil t)))
;;   (let ((msg (concat 
;;               (format "Installed %s`%s' Emacs Lisp file in \"%s\"\n"
;;                       (if version
;;                           (format "version %s.%s of "
;;                                   (car version) (cdr version))
;;                         "")
;;                       (file-name-nondirectory elisp-file)
;;                       (file-name-directory elisp-file))
;;               "For maximum performance you should compile it.\n"
;;               "If you have not done so already as part of the installation process,\n"
;;               "do it now with `M-x byte-compile-file'\n"
;;               (let* ((d1 (file-name-directory elisp-file))
;;                      (d2 (and d1 (substring d1 0 -1))))
;;                 (if (not (or (member d1 load-path)
;;                              (member d2 load-path)))
;;                     (concat
;;                       (format "\n%%%% Note! `%s' is not on the `load-path'\n" d1)
;;                       "%% You must add the following to you .emacs for Emacs to find the new code\n"
;;                       (format "(setq load-path (cons (expand-file-name \"%s\")\n" d1)
;;                               "                      load-path))\n\n"
;;                               ))))))
;;     (setq sicstus-installation-notes
;;           (cons msg sicstus-installation-notes))
;;     (with-output-to-temp-buffer "*Installation Notes*"
;;       (princ msg))))


;; (defun sicstus-get-newest-version-info (source match maj-match min-match)
;;   (let ((files (directory-files source nil match))
;;         (versions '()))
;;     (while files
;;       (let ((file (car files))
;;             maj min)
;;         (if (string-match match file)
;;             (if (condition-case nil
;;                     (let ((majs (match-string maj-match file))
;;                           (mins (match-string min-match file)))
;;                       (setq maj (and majs (string-to-int majs))
;;                             min (and mins (string-to-number mins)))
;;                       (and (integerp maj)
;;                            (numberp min)))
;;                   (error nil))
;;                 (setq versions (cons
;;                                 (cons (cons maj min) (concat source file))
;;                                 versions))))
;;         (setq files (cdr files))))
;;     (car
;;      (sort versions (function (lambda (x y)
;;                                 (setq x (car x)
;;                                       y (car y))
;;                                 (or (> (car x) (car y))
;;                                     (and (= (car x) (car y))
;;                                          (> (cdr x) (cdr y))))))))))



;;; Menus [PM]

(defvar sicstus-prolog-menu '(sicstus-prolog-menu)
  "Form to eval to obtain the definition for the menu in the editing buffers.")

(defvar sicstus-prolog-inferior-menu '(sicstus-prolog-inferior-menu)
  "Form to eval to obtain the definition for the menu in the inferior prolog buffer")

(defun sicstus-prolog-menu ()
  "Create the menu definition for the menu in the editing buffers."
  (list
   ;; "SICStus"
   "Prolog"
   '("Consult"
     ["File" prolog-consult-file :active t]
     ["Buffer" prolog-consult-buffer :active t]
     ["Region" prolog-consult-region :active (sicstus-region-exists-p)]
     ["Predicate" prolog-consult-predicate :active t])
   '("Compile"
     ["File" prolog-compile-file :active t]
     ["Buffer" prolog-compile-buffer :active t]
     ["Region" prolog-compile-region :active (sicstus-region-exists-p)]
     ["Predicate" prolog-compile-predicate :active t])
   ["Run Prolog" run-prolog :active t]
   "-"
   (list "Debugging"
         ["Debug" prolog-debug-on :active t]
         ["Trace" prolog-trace-on :active t]
         ["Zip" prolog-zip-on :active t]
         (vector "None" 'prolog-debug-off
                 ':active 't
                 ':keys (substitute-command-keys
                         (if (boundp 'pltrace-mode-map)
                             "C-u \\<pltrace-mode-map>\\[prolog-debug-on]"
                           "C-u \\<prolog-mode-map>\\[prolog-debug-on]"))

                 )
         )
   '("Source-linked debugging"
     ["Enable" prolog-enable-sicstus-sd :active t]
     ["Disable" prolog-disable-sicstus-sd :active t])
   (list "Bindings at debug prompts"
	 ["Enable" sicstus-bindings-on :active t]
	 (vector "Disable" 'sicstus-bindings-off
		 ':active 't
		 ':keys (substitute-command-keys
			 (if (boundp 'pltrace-mode-map)
			     "C-u \\<pltrace-mode-map>\\[sicstus-bindings-on]"
			   "C-u \\<prolog-mode-map>\\[sicstus-bindings-on]"))

		 )
	 ["Set Print Depth" sicstus-bindings-print-depth :active t]
	 )
   (list "Line Breakpoint"
         ["Set" pltrace-break t]
         ;;   (vector "Set Temporary" 
         ;;           '(pltrace-break nil 'temp)
         ;;           ':active t
         ;;           ':keys (substitute-command-keys
         ;;                   (if (boundp 'pltrace-mode-map)
         ;;                       "C-u \\<pltrace-mode-map>\\[pltrace-break]"
         ;;                       "C-u \\<prolog-mode-map>\\[pltrace-break]"))
         ;;           )
         (vector "Clear" 
                 '(pltrace-break nil 'temp)
                 ':active t
                 ':keys (substitute-command-keys
                         (if (boundp 'pltrace-mode-map)
                             "C-u - \\<pltrace-mode-map>\\[pltrace-break]"
                           "C-u - \\<prolog-mode-map>\\[pltrace-break]"))
                 )
         (vector "Clear All" 
                 '(pltrace-break nil 'temp)
                 ':active t
                 ':keys (substitute-command-keys
                         (if (boundp 'pltrace-mode-map)
                             "C-u C-u - \\<pltrace-mode-map>\\[pltrace-break]"
                           "C-u C-u - \\<prolog-mode-map>\\[pltrace-break]"))
                 )
         )
   "-"
   (list "Code coverage"
	 ["Refresh" sicstus-coverage-on :active t]
	 (vector "Clear" 'sicstus-coverage-off
		 ':active 't
		 ':keys (substitute-command-keys
			 (if (boundp 'pltrace-mode-map)
			     "C-u \\<pltrace-mode-map>\\[sicstus-coverage-on]"
			   "C-u \\<prolog-mode-map>\\[sicstus-coverage-on]"))

		 ))
   "-"
   (vector "Comment region" 'prolog-comment-region
           ':active '(and (sicstus-region-exists-p) (not buffer-read-only))
           ':keys (substitute-command-keys
                   (if (boundp 'pltrace-mode-map)
                       "C-u 3 \\<pltrace-mode-map>\\[comment-region]"
                     "C-u 3 \\<prolog-mode-map>\\[comment-region]"))
           )
   (vector "Uncomment region" 'prolog-uncomment-region
           ':active '(and (sicstus-region-exists-p) (not buffer-read-only))
           ':keys (substitute-command-keys
                   (if (boundp 'pltrace-mode-map)
                       "C-u - 3 \\<pltrace-mode-map>\\[comment-region]"
                     "C-u - 3 \\<prolog-mode-map>\\[comment-region]"))
           )
   ["Insert/move to comment" indent-for-comment :active (not buffer-read-only)]
   "-"
   '("Insert"
     ["Predicate template" prolog-insert-predicate-template :active (not buffer-read-only)]
     ["Next clause head" prolog-insert-next-clause :active (not buffer-read-only)]
     ["Predicate spec" prolog-insert-predspec :active (not buffer-read-only)]
     ["Module modeline" prolog-insert-module-modeline 
      ;; Cannot insert when narrowing hides beginning of buffer
      :active (and (= (point-min) 1)
                   (not buffer-read-only))])
   '("Move"
     ["Beginning of clause" prolog-beginning-of-clause :active t]
     ["End of clause" prolog-end-of-clause :active t]
     ["Beginning of predicate" prolog-beginning-of-predicate :active t]
     ["End of predicate" prolog-end-of-predicate :active t])
   '("Fontify"
     ["Buffer" font-lock-fontify-buffer :active t])
   '("Indent"
     ["Line" prolog-indent-line :active (not buffer-read-only)]
     ["Region" indent-region :active (and (sicstus-region-exists-p) (not buffer-read-only))]
     ["Predicate" prolog-indent-predicate :active (not buffer-read-only)]
     ["Buffer" prolog-indent-buffer :active (not buffer-read-only)])
   '("Mark"
     ["Clause" prolog-mark-clause :active t]
     ["Predicate" prolog-mark-predicate :active t]
     ["Paragraph" mark-paragraph :active t])
   '("Transform"
     ["All variables in region to '_'"
      prolog-variables-to-anonymous :active (and (sicstus-region-exists-p) (not buffer-read-only))])
   ["Help on Predicate" prolog-help-on-predicate :active t]

   ;; [PM] 3.12.0 This feature was never useful.
   ;;   "-"
   ;;   '("Misc"
   ;;     ["Check for new versions"
   ;;      sicstus-download-new-packages
   ;;      :active (fboundp 'sicstus-download-new-packages)]
   ;;     )

   ))

;; [PM] More stuff should go here. Some sharing between this and
;;      sicstus-prolog-menu
(defun sicstus-prolog-inferior-menu ()
  "Create the menu definition for the inferior prolog buffer"
  (list "Prolog"
        ;; In SICStus, these are pairwise disjunctive,
        ;; so it's enough with one "off"-command
        (list "Debugging"
              ["Debug" prolog-debug-on :active t]
              ["Trace" prolog-trace-on :active t]
              ["Zip" prolog-zip-on :active t]
              (vector "None" 'prolog-debug-off
		      ':active 't
		      ':keys (substitute-command-keys
			      (if (boundp 'pltrace-mode-map)
				  "C-u \\<pltrace-mode-map>\\[prolog-debug-on]"
				"C-u \\<prolog-mode-map>\\[prolog-debug-on]"))

		      ))
        '("Source-linked debugging"
	  ["Enable" prolog-enable-sicstus-sd :active t]
	  ["Disable" prolog-disable-sicstus-sd :active t])
	(list "Bindings at debug prompts"
	      ["Enable" sicstus-bindings-on :active t]
	      (vector "Disable" 'sicstus-bindings-off
		      ':active 't
		      ':keys (substitute-command-keys
			      (if (boundp 'pltrace-mode-map)
				  "C-u \\<pltrace-mode-map>\\[sicstus-bindings-on]"
				"C-u \\<prolog-mode-map>\\[sicstus-bindings-on]"))

		      )
	      ["Set Print Depth" sicstus-bindings-print-depth :active t]
	      )
        "-"
        ["Help on Predicate" prolog-help-on-predicate :active t]
        "-"
        ["Interrupt Prolog" comint-interrupt-subjob :active t]
        ;; [PM] Hmm, these are not attempting to call `halt' as
        ;; run-prolog with an argument does.
        ["Quit Prolog" comint-quit-subjob :active t]
        ["Kill Prolog" comint-kill-subjob :active t]
        ))


;;; ORIGINAL pltrace.el starts here
;;; 
;; pltrace.el --- support for the source debugger of SICStus Prolog >= 3.8
;;; Copyright (C) 1998, 1999 SICS and IQSoft
;;
;; Author: Rozman Tamas, IQSoft
;;         Emil Astrom, SICS <emil@sics.se>
;; Maintainer: Per.Mildner@sics.se
;; Version: 1.2.1
;; Keywords: source-level debugger sicstus

;;; Commentary:
;;
;; It is recommended that this file (pltrace.el) is kept in the same
;; directory as prolog.el, and it probably already is since this is
;; the default. No other installation is required for the debugger
;; mode. However prolog.el must of course be properly installed, see
;; that file for instructions of how to do that.

;;; Usage:
;;
;; When running SICStus in a shell buffer of Emacs and not in the
;; standard inferior buffer then the debugging support is turned on by
;; typing "M-x pltrace-current". To turn it on for the inferior buffer
;; use the Prolog menu or type "M-x pltrace-on". To turn it off for
;; either the inferior buffer or for a shell buffer, use the Prolog
;; menu or type "M-x pltrace-off".

;;; Version history
;; 1.2pre ?
;; 1.1 Released with SICStus Prolog 3#7


;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables and initializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar pltrace-port-arrow-assoc '( 
;; 	("call" . ">>>") ("exit" . "+++") ("ndexit" . "?++")
;; 	("redo" . "<<<") ("fail" . "---") ("exception" . "==>"))
;; "*Association list for mapping Prolog ports to overlay arrows")

;; (defvar pltrace-port-face-assoc '( 
;; 	("call" . pltrace-face-call)
;; 	("exit" . pltrace-face-exit)
;; 	("ndexit" . pltrace-face-ndexit)
;; 	("redo" . pltrace-face-redo)
;; 	("fail" . pltrace-face-fail)
;; 	("exception" . pltrace-face-exception))
;; "*Association list for mapping Prolog ports to overlay faces")

(defvar pltrace-port-help-assoc '( 
	("call" . "Call:")
	("exit" . "Exit (det):")
	("ndexit" . "Exit (nondet):")
	("redo" . "Redo:")
	("fail" . "Fail:")
	("exception" . "Exception:"))
"*Association: list for mapping Prolog ports to overlay help texts")

(defvar pltrace-source-debug-on "prolog:source_info_on.\n" 
"*When the Prolog-trace is switched on, the contents of this variable
is sent to the standard input of the Prolog process")

(defvar pltrace-source-debug-off "prolog:source_info_off.\n"
"*When the Prolog-trace is switched off, the contents of this variable
is sent to the standard input of the Prolog process")

(defvar pltrace-overlay (make-overlay (point) (point))
"Overlay used by the Prolog-trace to highlight a line")

(defface pltrace-face-call
  '((((class color)) (:background "yellow")))
  "Highlight at call port.")

(defface pltrace-face-redo
  '((((class color)) (:background "orange")))
  "Highlight at redo port.")

(defface pltrace-face-ndexit
  '((((class color)) (:background "green")))
  "Highlight at ndexit port.")

(defface pltrace-face-exit
  '((((class color)) (:background "cyan")))
  "Highlight at exit port.")

(defface pltrace-face-fail
  '((((class color)) (:background "magenta")))
  "Highlight at fail port.")

(defface pltrace-face-exception
  '((((class color)) (:background "red")))
  "Highlight at exception port.")

(defface pltrace-face-reached-det
  '((((class color)) (:background "green")))
  "Highlight line of code that was reached and made no nondet calls.")

(defface pltrace-face-reached-nondet
  '((((class color)) (:background "yellow")))
  "Highlight line of code that was reached and made some nondet calls.")

(defface pltrace-face-reached-not
  '((((class color)) (:background "red")))
  "Highlight line of code that was not reached.")

;; set the default highlight properties
(overlay-put pltrace-overlay 'face 'highlight)

;; initially make the overlay invisible
(delete-overlay pltrace-overlay)

(defvar pltrace-command-begin "$([{"
"defines a string that identifies the beginning of a Prolog-trace 
command")

(defvar pltrace-command-end "\n$)]}"
"Defines a string that identifies the end of a Prolog-trace command
Note that a command have to end with a newline, but that newline need 
not be put at the end of this string")

(defvar pltrace-queue ""
"A string that stores the characters came from the Prolog process")

(defvar pltrace-command-coming nil
"A bool value indicating whether currently a Prolog-trace command is
coming")

(defvar pltrace-process nil
"Identifies the Prolog process if Prolog-trace is running")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general string-manipulating functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; finds a pattern in string
;; the beginning position of the first occurrence of the 
;; pattern is returned; if pattern is not found and the string ends 
;; with a prefix of the pattern, returns that position
(defun pltrace-find (string pattern)
  (let ((i 0) 
        (to (length string))
        (char (aref pattern 0)))
    (catch 'loop
      (while (< i to)
        (if (and 
	     (= char (aref string i))
	     (let ((m ((lambda (a b) (if (< a b) a b))
		       (- to i) (length pattern))))
	       (pltrace-substring-compare 1 m pattern (1+ i) string)))
	    (throw 'loop i))
        (setq i (1+ i)))
      to)))

;; compares substrings; substring1 is (substring pattern i-pat to)
;; substring2 is (substring string i-str (+ (- i-str i-pat) to))

(defun pltrace-substring-compare (i-pat to pattern i-str string)
  (or (= i-pat to)
    (and
     (= (aref pattern i-pat) (aref string i-str))
     (pltrace-substring-compare (1+ i-pat) to pattern (1+ i-str) string))))

;; returns comma-separated substrings of string in a list
(defun pltrace-split-command (string)
  (let* ((pos (pltrace-find string ","))
	 (sub (substring string 0 pos)))
    (if (= pos (length string))
	 (list sub)
         (cons sub (pltrace-split-command (substring string (1+ pos)))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the implementation of the interace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
;; finds a new command in pltrace-queue and displays all parts of 
;; pltrace-queue that are not part of a command;
;; the beginning of the command is identified by the pattern defined in 
;; pltrace-command-begin
;;
;; this is called by the process filter pltrace-filter, so
;; proc and global variable pltrace-process is always the same
(defun pltrace-find-command (proc)
  (let* ((pos (pltrace-find pltrace-queue pltrace-command-begin))
         (first (substring pltrace-queue 0 pos))
         (second (substring pltrace-queue pos)))
    (pltrace-insert-process proc first)
    (setq pltrace-queue second)
    (if (<= (length pltrace-command-begin) (length second))
      (progn
	(setq pltrace-queue (substring pltrace-queue (length pltrace-command-begin)))
        (setq pltrace-command-coming t)
        (pltrace-parse-command proc)))))

;; finds the end of an already-found command in pltrace-queue and 
;; executes it;
;; the command end is identified by the pattern defined in 
;; pltrace-command-end
;;
;; this is called by the process filter pltrace-filter, so
;; proc and global variable pltrace-process is always the same
(defun pltrace-parse-command (proc)
  (let* ((real-end (concat pltrace-command-end "\n"))
	 (pos (pltrace-find pltrace-queue real-end))
         (first (substring pltrace-queue 0 pos))
         (second (substring pltrace-queue pos)))
    (if (<= (length real-end) (length second))
      (progn 
        (pltrace-execute-command proc first)
        (setq pltrace-command-coming nil)
        (setq pltrace-queue (substring second (length real-end)))
        (pltrace-find-command proc)))))

;; executes commands f, b, u, k, r, o
(defun pltrace-execute-normal-command (command param-list)
  (cond ((string= "u" command)
	 (pltrace-unshow))
	((string= "k" command)
	 (pltrace-kill-buffer (car param-list)))
	((string= "r" command)
	 (pltrace-refresh-buffer (car param-list)))
	(t
	 (let ((port (car param-list))
	       (line-num (string-to-number (nth 1 param-list)))
	       (rest (nth 2 param-list)))
	   (cond ((string= "o" command)
		  (pltrace-coverage-show port line-num (pltrace-ensure-buffer rest)))
		 ((string= "f" command)
		  (pltrace-show port line-num (pltrace-ensure-buffer rest)))
		 ((string= "b" command)
		  (pltrace-show port line-num (get-buffer rest))))))))

;; executes command c
(defun pltrace-execute-extended-command (command param-list text)
  (if (string= "c" command)
      (pltrace-create-buffer (car param-list) text)))

;; displays file file-name and highlights line line-num
;; corresponding to cov (#hits)
(defun pltrace-coverage-show (cov line-num buffer)
  (set-buffer buffer)
  (goto-line (+ 1 line-num))
  (let ((end-pos (point)))
    (goto-line line-num)
    (let ((face (cond ((string= cov "det(0)") 'pltrace-face-reached-not)
		      ((string-match "nondet" cov) 'pltrace-face-reached-nondet)
		      (t 'pltrace-face-reached-det)))
	  (help (cond ((string-match "nondet(\\(.*\\))" cov)
		       (concat (match-string 1 cov) " hits making some nondet calls"))
		      ((string-match "det(\\(.*\\))" cov)
		       (concat (match-string 1 cov) " hits"))))
	  (lay (make-overlay (point) end-pos)))
      (overlay-put lay 'help-echo help)
      (overlay-put lay 'coverage t)
      (overlay-put lay 'face face))
    )
  )

;; Now we should make absolutely sure that this file is
;; not already visited under another name 
(defun pltrace-ensure-buffer (file-name)
  (let ((bufs (buffer-list))
	(truefilename (file-truename file-name))
	buf
	buffer)
    ;; Go through all open buffers to see if their true file names
    ;; match file-name.
    (let ((case-fold-search (memq system-type '(windows-nt cygwin32 ms-dos vax-vms)))
          (truefilename-regexp (regexp-quote truefilename)))
      (while bufs
        (setq buf (car bufs))
        (if (and
             (buffer-file-name buf)

             ;; [PM] However, we should make sure that the case tables
             ;;      used are not arbitrarily taken from the current
             ;;      buffer. Investigate this.
             ;; [PM] on windows sicstus will lowercase all paths
             ;;      This is not true of Emacs so there might be
             ;;      spurious mismatches. Arguably Xemacs
             ;;      file-truename is buggy and should return the
             ;;      casified name if given a matching all lowercase
             ;;      name on these systems. However, using case
             ;;      insensitive compare might be a good idea even
             ;;      when file-truename does the right thing (such as
             ;;      GNU Emacs 20.4.1)
             ;; (string= truefilename (file-truename (buffer-file-name buf)))
             (string-match truefilename-regexp 
                           (file-truename (buffer-file-name buf)))
             )
            (progn
              (setq buffer buf)
              (setq bufs nil)))		; End while loop
        (setq bufs (cdr bufs))))        ; Get next buffer

    ;; If the file was not already opened, then open it
    (if (not buffer)
	(setq buffer (find-file-noselect file-name))
      buffer)))

;; clears buffer named buffer-name and puts text in it;
;; or if it does not exist it creates a new buffer and puts text into it
(defun pltrace-create-buffer (buffer-name text)
  (save-excursion
    (let ((new-buffer (get-buffer-create buffer-name)))
      (set-buffer new-buffer)
      (let ((buffer-read-only nil))
        (widen)
        (let ((valid                    ; existing content is OK
               (and (= (buffer-size) (length text)) ; redundant
                 (string= (buffer-substring-no-properties (point-min) (point-max))
                          text))))
          ;; [PM] avoid reinserting and re-fontifying.
          (if (not valid)
              (progn
                (erase-buffer)
                (insert text)
                ;; Start the Prolog mode if it is available (for
                ;; font-locking)
                ;; [PM] used to require non-nil window-system.
                ;;      This is a bad idea since
                ;;      . font locking also works for color capable
                ;;        terminals. Instead added a test that font-lock is
                ;;        on
                ;;      . prolog-mode has other benefits for prolog code
                (if (fboundp 'prolog-mode)
                    (progn
                      (prolog-mode)
                      ;; [PM] should be done by major mode.
                      ;;      prolog-mode now does this.
                      ;; (and font-lock-mode       ; prolog-mode-hook may change this
                      ;;      (font-lock-fontify-buffer))
                      ))))
          (set-buffer-modified-p nil)))
      (display-buffer new-buffer))))

;; hides pltrace-overlay an overlay-arrow
(defun pltrace-unshow ()
  ;; (setq overlay-arrow-string nil)
  (delete-overlay pltrace-overlay))

;; kills a buffer
(defun pltrace-kill-buffer (buffer)
  (kill-buffer buffer))

;; refreshes a buffer
(defun pltrace-refresh-buffer (buffer-name)
  (pltrace-display-buffer (get-buffer buffer-name)))

;; displays a buffer and highlights line 
;; line-num with the overlay-arrow corresponding to port
(defun pltrace-show (port line-num buffer)
  (let ((pltrace-window (pltrace-display-buffer buffer)))
  (save-excursion
    (set-buffer buffer)
    (goto-line (+ 1 line-num))
    (let ((end-pos (point)))
      (goto-line line-num)
      (overlay-put pltrace-overlay 'face 'highlight
		   ;; (cdr (assoc port pltrace-port-face-assoc))
		   )
      (overlay-put pltrace-overlay 'help-echo (cdr (assoc port pltrace-port-help-assoc)))
      (move-overlay pltrace-overlay (point) end-pos buffer)
      ;; [PM] Why was this done again?
      ;; (goto-line line-num)
      ;; (setq overlay-arrow-position (point-marker))
      ;; (setq overlay-arrow-string (cdr (assoc port pltrace-port-arrow-assoc)))

      (if (pos-visible-in-window-p (point) pltrace-window)
          ;; [PM] This has the poorly documented side effect of
          ;; forcing the next redisplay to ensure that window-start is
          ;; not changed by point motion.  Without set-window-start
          ;; the newly created "*Prolog source*" buffer will always
          ;; show the end of the buffer.  I suspect that the problem
          ;; has to do with the newly inserted buffer contents (the
          ;; source to display) and the fact that this is all
          ;; happening in a process filter so there has been no
          ;; redisplay of this window yet.
          (set-window-start pltrace-window (window-start pltrace-window))
        (forward-line (/ (- (window-height pltrace-window) 1) -2))
        (set-window-start pltrace-window (point)))))))

;; [MC] help function for refreshing buffers in the context of *prolog* and *bindings*
(defun pltrace-display-buffer (buffer)
  (let ((cb (current-buffer))
	(bb (get-buffer "*Prolog Bindings*")))
    (cond (bb
	   (save-excursion
	     (delete-window (get-buffer-window bb))
             (and (get-buffer-window "*prolog*") ; [PM] 4.2
                  (select-window (get-buffer-window "*prolog*")))
	     (display-buffer buffer)
	     (split-window-vertically)
	     (switch-to-buffer-other-window bb)
             (and (get-buffer-window cb) ; [PM] 4.2
                  (select-window (get-buffer-window cb))))
	   (get-buffer-window buffer))
	  (t (display-buffer buffer)))))


;; [PM] Old code did not handle pre-existing process filter (e.g.,
;;      comint-filter)
;; [PM] Make sure pre-existing filter gets a shot at whatever is not
;;      a pltrace command. This way it should be possible for several 
;;      filters to coexist peacefully.
;;      Also, make it harmless to call pltrace-prolog (and thus
;;      pltrace-current) repeatedly.
(defvar pltrace-old-process-filter nil
  ;; note that FILTER may be nil or t as well
  "A cons (PROCESS . FILTER) of the process and its old filter.
PROCESS is needed since the user may have restarted the prolog
process, possibly in the same buffer.")

;; [PM]
;; searches for a command in the output of the Prolog process 
;; and displays all parts of the output that are not 
;; part of a command
;;
;; this is a process filter for Prolog-trace, so
;; proc and global variable pltrace-process is always the same
(defun pltrace-filter (proc string)
  (setq pltrace-queue (concat pltrace-queue string)) 
  (if pltrace-command-coming 
    (pltrace-parse-command proc)
    (pltrace-find-command proc)))

;; [PM] We always want the process filter to look for emacs commands
;; However, we may not want prolog to use source debugging.
;; treats the process of the buffer buf as a Prolog process and (by
;;;default) starts Prolog-trace
(defun pltrace-prolog (buf &optional no-debug-on)
  (let ((process (get-buffer-process buf)))
    (if process
        (progn
          (setq pltrace-process process) ; [PM] only change if non-nil
          ;; zap if obsolete
          (and pltrace-old-process-filter
               (not (equal (car pltrace-old-process-filter)
                           pltrace-process))
               (setq pltrace-old-process-filter nil))

          (if pltrace-old-process-filter ; we're already installed
              nil                       ; do nothing
            (progn
              (setq pltrace-old-process-filter
                    (cons pltrace-process
                          (process-filter pltrace-process)))
              (setq ;; overlay-arrow-string nil
                    ;; overlay-arrow-position nil
                    pltrace-queue ""
                    pltrace-command-coming nil)
              (set-process-filter pltrace-process 'pltrace-filter)
              ))
          (if (not no-debug-on)
              (process-send-string pltrace-process pltrace-source-debug-on))))))



;; [PM] insert into buffer using old process-filter, if any
(defun pltrace-insert-process (proc string)
  (let ((old-filter
         (and (equal proc (car pltrace-old-process-filter))
              (cdr pltrace-old-process-filter))))
    (if (or (eq old-filter t)
            (eq old-filter nil))
        (pltrace-insert-process-default proc string)
      (funcall old-filter proc string))))
           
           

;; inserts string to the buffer of process proc
;; prolog.el has a corresponding function called
;; `prolog-process-insert-string' but it might be better not to
;; rely on the existance of it.
;; [PM] This used to be pltrace-insert-process
(defun pltrace-insert-process-default (proc string)
  (let ((old-buffer (current-buffer)))
    (unwind-protect                     ; aka with-current-buffer
        (let (moving)
          (set-buffer (process-buffer proc))
          (setq moving (= (point) (process-mark proc)))
          (save-excursion
            ;; Insert the text, moving the process-marker.
            (goto-char (process-mark proc))
            (insert string)
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc))))
      (set-buffer old-buffer))))

  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks and interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [PM]
(defvar sicstus-support-hook '()
  "List of functions to call when entering SICStus prolog-mode.
This hook is run by run-sicstus-support-hook if prolog-system is `sicstus'.
Also see sicstus-support-inferior-hook.")

;; [PM]
(defvar sicstus-support-inferior-hook '()
  "List of functions to call when entering SICStus prolog-inferior-mode.
This hook is run by run-sicstus-support-inferior-hook if prolog-system is `sicstus'.
Also see sicstus-support-hook.")

;; [PM] 4.0.1
(defvar sicstus-support-inferior-prehook '()
  "List of functions to call early when entering SICStus prolog-inferior-mode.
This hook is run by run-sicstus-support-inferior-hook before sicstus-support-inferior-hook.
Also see sicstus-support-inferior-hook and sicstus-support-hook.")


;; [PM] 4.0.1 xref sicstus-program-switches
(defun sicstus-support-set-process-coding-system-hook ()
  "Sets buffer process coding system to UTF-8 if possible"
  (let ((utf8 (sicstus-emacs-utf-8-coding-system)))
    (if utf8
        (set-buffer-process-coding-system utf8 utf8))))

;; [PM] 4.0.1 must be done early, before any I/O.  This is something
;; of a hack since SICStus may produce some output before we get to
;; change the coding system. The right thing (tm) would be to tell
;; comint what coding-system to use when it starts the sicstus
;; process.
(add-hook 'sicstus-support-inferior-prehook 'sicstus-support-set-process-coding-system-hook)

;; [PM]
(defun sicstus-support-initialization ()
  "Initialize SICStus support.
Does nothing if `prolog-system' is not `sicstus'.
Intended to always be on prolog-mode-hook"
  ;; Key bindings
  (define-key pltrace-mode-map 
    "\C-x "                       ; also works in GNU Emacs 19.34.1
    'pltrace-break)
  (define-key pltrace-mode-map "\C-c\C-g" 'sicstus-bindings-on)
  (define-key pltrace-mode-map "\C-c<" 'sicstus-bindings-print-depth)
  (define-key pltrace-mode-map "\C-c\C-o" 'sicstus-coverage-on)
  ;; [PM] SP 3.9b4
  (make-variable-buffer-local 'prolog-parse-compilation-errors)
  (setq prolog-parse-compilation-errors 'sicstus-prolog-parse-compilation-errors)
  (run-hooks 'sicstus-support-hook))

(add-hook 'prolog-mode-hook 'sicstus-support-initialization)

;; [PM]
(defun sicstus-support-inferior-initialization ()
  "Initialize SICStus support for inferior prolog.
Does nothing if `prolog-system' is not `sicstus'.
Intended to always be on prolog-inferior-mode-hook"
  (run-hooks 'sicstus-support-inferior-prehook)
  (run-hooks 'sicstus-support-inferior-hook))

(add-hook 'prolog-inferior-mode-hook 'sicstus-support-inferior-initialization)

(defun pltrace-maybe-on ()
  ;; "Call pltrace-on if `prolog-use-sicstus-sd' true. Used as Hook"
  (if prolog-use-sicstus-sd
    (pltrace-on)))

(defun sicstus-inferior-keybindings ()
  (define-key prolog-inferior-mode-map "\C-c\C-g" 'sicstus-bindings-on)
  (define-key prolog-inferior-mode-map "\C-c<" 'sicstus-bindings-print-depth)
  (define-key prolog-inferior-mode-map "\C-c\C-o" 'sicstus-coverage-on))

(add-hook 'sicstus-support-inferior-hook 'pltrace-maybe-on)
(add-hook 'sicstus-support-inferior-hook 'sicstus-inferior-keybindings)



;; [PM] Used to be in prolog.el
(defun prolog-enable-sicstus-sd ()
  "Enable the source-linked debugging facilities.
To turn source debugging on (off) only for the currently running
inferior prolog, use \\[pltrace-on] (\\[pltrace-off])."
  (interactive)
  ;; [PM] Hook always on, it now looks at prolog-use-sicstus-sd
  ;; ;; Turn on the source-linked debugging by default
  ;; (add-hook 'sicstus-support-inferior-hook 'pltrace-on t)

  ;; [PM] Logic is lacking, if set after inferior prolog starts then
  ;; enabling will turn it on in the running inferior mode.

  (if (not prolog-use-sicstus-sd)
      (progn
        ;; If there is a *prolog* buffer, then call pltrace-on
        (if (get-buffer "*prolog*")
            (pltrace-on))
        (setq prolog-use-sicstus-sd t))))

;; [PM] Used to be in prolog.el
(defun prolog-disable-sicstus-sd ()
  "Disable the source-linked debugging facilities.
To turn source debugging on (off) dynamically, use pltrace-on (pltrace-off)."
  (interactive)
  (setq prolog-use-sicstus-sd nil)      ; disable for future run-prologs
  ;; [PM] the hook now looks at prolog-use-sicstus-sd
  ;; ;; Remove the hook
  ;; (remove-hook 'sicstus-support-inferior-hook 'pltrace-on)

  ;; If there is a *prolog* buffer, then call pltrace-off
  (if (get-buffer "*prolog*")
      (pltrace-off)))
  
;; turn on the trace for the current buffer
(defun pltrace-current ()  
  "Turn on source-linked debugging for the current buffer."
   (interactive)
   (pltrace-prolog (current-buffer)))

;; treats the process of the current buffer as a Prolog process and starts
;; Prolog-trace
(defun pltrace-on ()  
  "Turn on source-linked debugging for the *prolog* buffer."
   (interactive)
   (pltrace-prolog (get-buffer "*prolog*")))

(defun pltrace-off (&optional remove-process-filter)
  "Turn off source-linked debugging."
  (interactive)
  (if pltrace-process 
      (progn
        ;; [PM] Now we want the process filter to look for emacs
        ;; commands even if not source debugging.
        (if remove-process-filter
            (progn
              ;; [PM] used to set to nil unconditionally, but
              ;;      e.g., the comint filter should be reinstalled
              (set-process-filter pltrace-process
                                  (and pltrace-old-process-filter
                                       (equal (car pltrace-old-process-filter)
                                              pltrace-process)
                                       (cdr pltrace-old-process-filter)))
              (setq pltrace-old-process-filter nil)))

	(delete-overlay pltrace-overlay)
	;; (setq overlay-arrow-string nil
        ;;       overlay-arrow-position nil)
	(process-send-string pltrace-process pltrace-source-debug-off)

	(if remove-process-filter (setq pltrace-process nil)))))

;; [PM] SP 3.9b4 moved from prolog.el, adapted for 3.9
(defun sicstus-prolog-parse-compilation-errors (limit &optional find-at-least)
  "Parse the prolog compilation buffer for errors.
Argument LIMIT is a buffer position limiting searching.
For use with the `compilation-parse-errors-function' variable."
  (setq compilation-error-list nil)
  (message "Parsing SICStus error messages...")

  ;; In case limit is within a line this will extend the limit to
  ;; encompass the entire line.

  ;; Actually, We should probably do what compile.el does, look at
  ;; each line manually.
  (if limit
      (save-excursion
        (goto-char limit)
        (forward-line 1)
        (setq limit (point))))
  (let ((found 0)
        (last-error-end compilation-parsing-end)
        filepath dir file errorline
        error-sign                      ; "!" or "*"
        )
    (goto-char compilation-parsing-end)
    (while
        (and
         ;; [PM] ignore find-at-least
         ;;      1. XEmacs will call with find-at-least zero!
         ;;      2. Want to always process a complete file since we
         ;;         want markers in case the user edits the file upon
         ;;         visiting the first error.
         ;; (or (null find-at-least) (< found find-at-least))
         (re-search-forward
          ;; 3.8 "{\\([a-zA-Z ]* ERROR\\|Warning\\):.* in line[s ]*\\([0-9]+\\)"
          "^\\\([*!]\\) Approximate lines: \\([0-9]+\\)"
          limit t))

      (setq errorline (string-to-number (match-string 2)) ; may be adjusted below
            error-sign (regexp-quote (match-string 1)))
      
      (save-excursion
	(re-search-backward
         
	 ;; 3.8 "{\\(consulting\\|compiling\\|processing\\) \\(.*\\)\\.\\.\\.}"
	 "% \\(consulting\\|compiling\\|processing\\) \\(.*\\)\\.\\.\\."
	 limit t)
	(setq filepath (match-string 2)))

      ;; Does this work with SICStus under Windows (i.e. backslashes and stuff?)
      ;; [PM] SP 3.9b4 SICStus does not print backslashes even on Windows
      ;;               'C:' and UNC paths is what differs from UNIX
      (if (string-match "\\(.*/\\)\\([^/]*\\)$" filepath)
	  (progn
	    (setq dir (match-string 1 filepath))
	    (setq file (match-string 2 filepath))))
      
      ;; In 3.9 error messages looks like
      ;; ! Syntax error
      ;; ! , cannot start an expression
      ;; ! in line 21
      ;; ! main1 :- 
      ;; ! <<here>>
      ;; ! , , foo ( [ 1 , 2 , 3 ] ) . 
      ;; ! Approximate lines: 17-23, file: '/home/perm/sicstus/foo.pl'
      ;; ! Syntax error
      ;; ! operator expected after expression
      ;; ! in line 24
      ;; ! main1 :- a 
      ;; ! <<here>>
      ;; ! a a foo ( [ 1 , 2 , 3 ] ) . 
      ;; ! Approximate lines: 17-26, file: '/home/perm/sicstus/foo.pl'
      ;;
      ;; The error-sign ! can be * for warnings
      ;;
      ;; We have now found the line containing "Approximate lines: "
      ;;
      ;; We should scan backwards over ! (*) until we reach a non-! 
      ;; (non-*) or we reach part of the previous error.
      ;; If we find a "in line NN we should adjust errorline to the more
      ;; precise info
      ;;

      (let ((error-point (point))       ; beginning of this error message
            error-end)                  ; last-error-end to use for next error
        (save-excursion
          (end-of-line)
          (setq error-end (point))
          (beginning-of-line 0)         ; beginning of previous line
          (while (and (< last-error-end (point))
                      (looking-at error-sign))
            (if (looking-at (concat error-sign " in line \\([0-9][0-9]*\\)"))
                (setq errorline (string-to-number (match-string 1))))
            (setq error-point (point))
            (beginning-of-line 0))
          (goto-char error-point)       ; beginning of this error
          
          (setq last-error-end error-end
                error-point (point-marker)))
        (setq compilation-error-list
              (cons
               (cons error-point
                     (list (list file dir) errorline))
               compilation-error-list)
              found (+ found 1)
              )))
    )
  (setq compilation-error-list (nreverse compilation-error-list)))

;; [PM] This is so wrong. The faces should be customizable
(defun sicstus-prolog-font-lock-keywords ()
  "Set up font lock keywords for the SICStus Prolog system."
  (if (or window-system
          ;; [PM] XEmacs does font-lock on terminals as well
          ;;      Also, window-system is bogus in a multi-display
          ;;      environment
          (eq prolog-emacs 'xemacs))
      (progn
        (require 'font-lock)
        ;; This is only for backward compatibility. Not needed with
        ;; Emacs 19.34 or later.
        (if (boundp 'font-lock-background-mode)
            ()
          (make-local-variable 'font-lock-background-mode)
          (setq font-lock-background-mode 'light)) ; Assume light bg
        (if (boundp 'font-lock-display-type)
            ()
          (make-local-variable 'font-lock-display-type)
          (setq font-lock-display-type 'color)) ; Assume color

        ;; Create faces
        ;; Format: (FACE FOREGROUND BACKGROUND BOLD-P ITALIC-P UNDERLINE-P)
        (let* ((dark-bg (eq font-lock-background-mode 'dark))
               (faces
                (cond
                 ((memq font-lock-display-type '(mono monochrome))
                  '((prolog-error-face nil nil t t nil)
                    (prolog-warning-face nil nil t t nil)
                    (prolog-informational-face nil nil nil t nil)
                    (prolog-exception-face nil nil t t t)
                    (prolog-builtin-face nil nil nil nil t)
		    (prolog-call-face nil nil nil nil nil)
		    (prolog-exit-face nil nil nil nil nil)
		    (prolog-redo-face nil nil nil nil nil)
		    (prolog-fail-face nil nil nil nil nil)
		    ))
                 ((memq font-lock-display-type '(grayscale greyscale
                                                           grayshade greyshade))
                  '((prolog-error-face nil nil t t nil)
                    (prolog-warning-face nil nil t t nil)
                    (prolog-informational-face nil nil nil t nil)
                    (prolog-exception-face nil nil t t t)
                    (prolog-builtin-face nil nil nil nil t)
		    (prolog-call-face nil nil nil nil nil)
		    (prolog-exit-face nil nil nil nil nil)
		    (prolog-redo-face nil nil nil nil nil)
		    (prolog-fail-face nil nil nil nil nil)))
                 (dark-bg 		; dark colour background
                  '((prolog-error-face "red" nil t nil nil)
                    (prolog-warning-face "orange" nil t nil nil)
                    (prolog-informational-face "gray50" nil t nil nil)
                    (prolog-exception-face "black" "Khaki" t nil nil)
                    (prolog-builtin-face "LightSkyBlue" nil nil nil nil)
                    (prolog-call-face "black" nil nil nil nil)
                    (prolog-exit-face "black" nil nil nil nil)
                    (prolog-redo-face "black" nil nil nil nil)
                    (prolog-fail-face "black" nil nil nil nil)))
                 (t			; light colour background
                  '((prolog-error-face "red" nil t nil nil)
                    (prolog-warning-face "orange" nil t nil nil)
                    (prolog-informational-face "gray50" nil t nil nil)
                    (prolog-exception-face "black" "Khaki" t nil nil)
                    (prolog-builtin-face "Orchid" nil nil nil nil)
                    (prolog-call-face "black" nil nil nil nil)
                    (prolog-exit-face "black" nil nil nil nil)
                    (prolog-redo-face "black" nil nil nil nil)
                    (prolog-fail-face "black" nil nil nil nil))))))

          (while faces
            (if (fboundp 'font-lock-make-face)
                ;; The preferred way
                (font-lock-make-face (car faces))
              ;; The clumsy way
              (let ((facename (nth 0 (car faces)))
                    (fg (nth 1 (car faces)))
                    (bg (nth 2 (car faces)))
                    (bold (nth 3 (car faces)))
                    (ital (nth 4 (car faces)))
                    (under (nth 5 (car faces))))
                (make-face facename)
                (if fg (set-face-foreground facename fg))
                (if bg (set-face-background facename bg))
                (if bold (make-face-bold facename))
                (if ital (make-face-italic facename))
                (if bold (make-face-bold facename))
                (set-face-underline-p facename under)
                ;; This is needed under Emacs 20 for some reason.
                (set facename facename)
                ))
            (setq faces (cdr faces))))
      
        ;; Font Lock Patterns
        (let (
              ;; "Native" Prolog patterns
              (head-predicates
               (list (format "^%s" prolog-atom-regexp)
                     0 font-lock-function-name-face))
              (variables
                                        ;xx
                                        ;	       '("\\<\\([_A-Z][a-zA-Z0-9_]*\\)"
                                        ;		 1 font-lock-variable-name-face))
               (list (format "\\<\\([_%s][%s%s0-9_]*\\)"
                             prolog-upper-case-string
                             prolog-lower-case-string
                             prolog-upper-case-string)
                     1 'font-lock-variable-name-face))

              (important-elements
               (list "[][}{!;|]\\|\\*->"
                     0 'font-lock-keyword-face))
              (important-elements-1
               '("[^-*]\\(->\\)" 1 font-lock-keyword-face))
              (predspecs		; module:predicate/cardinality
               (list (format "\\<\\(%s:\\|\\)%s/[0-9]+"
                             prolog-atom-regexp prolog-atom-regexp)
                     0 font-lock-function-name-face 'prepend))
              (keywords                 ; directives (queries)
               (list
                (concat
                 "^[?:]- *\\("
                 (prolog-make-keywords-regexp prolog-keywords-i)
                 "\\)\\>")
                1 'prolog-builtin-face))

              (quoted_atom (list prolog-quoted-atom-regexp
                                 2 'font-lock-string-face 'append))
              (string (list prolog-string-regexp
                            1 'font-lock-string-face 'append))
              ;; Inferior mode specific patterns
              (prompt
               (list prolog-prompt-regexp-i 0 'font-lock-keyword-face)) 
              (trace-call
               '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Call\\):"
                 1 prolog-call-face))
              (trace-exit
               '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Exit\\):"
                 1 prolog-exit-face))
              (trace-redo
               '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Redo\\):"
                 1 prolog-redo-face))
              (trace-fail
               '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Fail\\):"
                 1 prolog-fail-face))
              (trace-exception
               '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Exception\\):"
                 1 prolog-exception-face))
              (error-message-identifier
               '(
                 ;; 3.8 "{\\([A-Z]* ?ERROR:\\)"
                 ;; any line that starts with an exclamation mark
                 ;; extend any lines containing 'error' to the end of line
                 "\\(^! \\(.*error.*$\\)?\\)"
                 1 prolog-exception-face prepend))
              (error-whole-messages
               '(
                 ;; 3.8 "{\\([A-Z]* ?ERROR:.*\\)}[ \t]*$"
                 "\\(^! .*\\)$"           ; any line that starts with an exclamation mark
                 1 prolog-error-face append))
              (error-warning-messages
               ;; Mostly errors that SICStus asks the user about how to solve,
               ;; such as "NAME CLASH:" for example.
               '("^[A-Z ]*[A-Z]+:" 0 prolog-warning-face))
              (warning-messages
               '(
                 ;; 3.8 ;; We allow one newline in the warning message
                 ;; 3.8 "\\({ ?\\(Warning\\|WARNING\\) ?:[^\n]*\n?[^\n]*}\\)[ \t]*$" 
                 "\\(^\\* .*\\)$"         ; any line that starts with an asterisk
                 ;; 3.8 2
                 1 prolog-warning-face prepend))
              (informational-messages
               '(
                 "\\(^% .*\\)$"         ; any line that starts with a percent
                 1 prolog-informational-face prepend))
              )

          ;; Make font lock list
          (delq
           nil
           (cond
            ((eq major-mode 'prolog-mode)
             (list
              head-predicates
              quoted_atom
              string
              variables
              important-elements
              important-elements-1
              predspecs
              keywords))
            ((eq major-mode 'prolog-inferior-mode)
             (list
              ;; prompt
	      ;; predspecs
              error-message-identifier
              error-whole-messages
              error-warning-messages
              warning-messages
	      informational-messages
              trace-exit
              trace-fail
              trace-redo
              trace-call
              trace-exception
              ))
            ((eq major-mode 'compilation-mode)
             (list
              error-message-identifier
              error-whole-messages
              error-warning-messages
              warning-messages
	      informational-messages
              predspecs))))
          ))))

(put 'prolog-font-lock-keywords 'sicstus 'sicstus-prolog-font-lock-keywords)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (provide 'pltrace)

;;; ORIGINAL pltrace.el ends here

(provide 'sicstus-support)

;;; sicstus-support.el ends here
