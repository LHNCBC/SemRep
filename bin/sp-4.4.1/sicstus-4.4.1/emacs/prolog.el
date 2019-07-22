;; -*- Mode: Emacs-Lisp; buffer-read-only:t; -*-
;; [PM] 4.0 Except for the first six lines this file should be a
;;          verbatim copy of the sicstus3 version. Differences should
;;          be localized to sicstus-support.el
;;          Do not make SP4-specific changes without notifying PM.

;; prolog.el --- major mode for editing and running Prolog code
;;
;; Copyright (C) 1986, 1987, 1997, 1998, 1999 Free Software Foundation, Inc.
;;
;; Author: Emil Astrom <emil.astrom@excosoft.se>
;;         Milan Zamazal <pdm@freesoft.cz>
;;         (see below for more details)
;; Keywords: prolog major mode sicstus swi mercury
;;
;; Version: 1.41 (for SICStus Prolog 3.12.1)
;;
;; Used to be prolog.el version 0.1.31, now a separate development branch
;; Synched up with prolog.el 0.1.31
;; Maintainer: Per.Mildner@sics.se  ([PM])
;;
;; Should work with FSF/GNU Emacs as well as XEmacs.
;; 
;; To use this put the following in your ~/.emacs file
;; (setq load-path (cons (expand-file-name "/usr/local/lib/sicstus3/emacs") load-path))
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;; (setq prolog-system 'sicstus)
;; (setq auto-mode-alist (cons '("\\.pl$" . prolog-mode) auto-mode-alist))
;; 
;; where the path in the first line is the file system path to this file.
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
;;
;; What is different from prolog.el 0.1.31?
;; . Should work reliably in XEmacs.
;; . All autoloaded functions now try to load prolog-system specific
;;   code (by doing (require <prolog-system>-support)). This makes it
;;   easy to develop system specific code independently form the
;;   development of prolog.el. See prolog-load-system-support. In
;;   particular if prolog-system is sicstus then "sicstus-support"
;;   will be loaded. See also next item:
;; . A method to override most functions with prolog-system specific
;;   code, see prolog-system-override. Please use it when modifying
;;   this code, it will ensure that this prolog-mode can be developed
;;   independently from prolog-system specific support.
;; . Some bug fixes and minor features
;;   
;; Notes: The original prolog.el 0.1.31 supported several prolog
;; dialects. In contrast the purpose of this version is to support the
;; latest version of SICStus Prolog. I [PM] will try to avoid breaking
;; the existing support for other Prolog systems but that aspect of
;; the code will not be further improved and is likely to deteriorate
;; with time.
;;

;; [PM]
(defconst prolog-mode-version '(1 . 41)
  "The version of the prolog library as a pair (MAJOR . MINOR).")

;; Original 0.1.31 header follows

;; prolog.el --- major mode for editing and running Prolog code

;; Copyright (C) 1986, 1987, 1997, 1998, 1999 Free Software Foundation, Inc.

;; Author: Emil Astrom <emil.astrom@excosoft.se>
;;         Milan Zamazal <pdm@freesoft.cz>
;;         (see below for more details)
;; Keywords: prolog major mode sicstus swi mercury
;; Version: 0.1.31

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Original author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Parts of this file was taken from a modified version of the original
;; by Johan Andersson, Peter Olin, Mats Carlsson, Johan Bevemyr, Stefan
;; Andersson, and Per Danielsson (all SICS people), and Henrik Båkman
;; at Uppsala University, Sweden.
;;
;; Some ideas and also a few lines of code have been borrowed (not stolen ;-)
;; from Oz.el, the Emacs major mode for the Oz programming language,
;; Copyright (C) 1993 DFKI GmbH, Germany, with permission.
;; Authors: Ralf Scheidhauer and Michael Mehl ([scheidhr|mehl]@dfki.uni-sb.de)
;;
;; More ideas and code have been taken from the SICStus debugger mode
;; (http://www.csd.uu.se/~perm/source_debug/index.shtml) by Per Mildner.
;;
;; Additions for ECLiPSe and other helpful suggestions: Stephan Heuel
;; <heuel@ipb.uni-bonn.de>

;;; Commentary:
;;
;; This package provides a major mode for editing Prolog.  It knows
;; about Prolog syntax and comments, and can send regions to an inferior
;; Prolog interpreter process. Font-lock and auto indentation is
;; supported.
;;
;; The code requires the comint, easymenu, info, imenu, and font-lock
;; libraries. These are normally distributed with GNU Emacs.

;;; Installation:
;;
;; Insert the following lines in your ~/.emacs to make Emacs use this
;; mode when editing Prolog files:
;;
;; (setq load-path (cons "/usr/local/lib/sicstus3" load-path))
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;; (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
;; (setq prolog-system 'sicstus)
;; (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
;;                                 ("\\.m$" . mercury-mode))
;;                                auto-mode-alist))
;;
;; where the path in the first line is the file system path to this file.
;; MSDOS paths can be written like "d:/programs/emacs-19.34/site-lisp". 
;;
;; The last line above makes sure that files ending with .pl are
;; assumed to be Prolog files and not Perl, which is the default Emacs
;; setting. If this is not wanted, remove that line. It is then necessary
;; for the user to manually switch to prolog mode by typing
;; M-x prolog-mode after opening a prolog file.
;;
;; If the shell command to start the prolog process ('sicstus' or 'pl' for SWI
;; prolog) is not available in the default path, then it is necessary to
;; set the value of the environment variable EPROLOG to a shell command
;; to invoke the prolog process.


;;; Code:

(eval-when-compile
  (require 'compile)
  (require 'font-lock)
  ;; [PM] imenu is available in later XEmacs
  ;;      (The test should have used emacs-version)
  ;;      Now attempt the require but ignore errors if imenu not present.
  ;;      With the package system it may or not be present even in
  ;;      later XEmacsen.
  ;; ;; Don't require imenu under XEmacs
  ;; (if (not (and (boundp 'running-xemacs) running-xemacs))
  ;;     (require 'imenu))
  (condition-case nil
      (require 'imenu)                  ; third arg no-error is not in XEmacs
    (error nil))
  (require 'info)
  (require 'shell))

(require 'comint)
(require 'easymenu)


;;-------------------------------------------------------------------
;; User configurable variables
;;-------------------------------------------------------------------

;; General configuration

(defvar prolog-system 'sicstus
  "*Prolog interpreter/compiler used.
The value of this variable is nil or a symbol.
If it is a symbol, it determines default values of other configuration
variables with respect to properties of the specified Prolog
interpreter/compiler.
If an elisp library <prolog-system>-support (e.g., sicstus-support) exists it
is loaded automagically.

Currently recognized symbol values are:
eclipse - Eclipse Prolog
mercury - Mercury
sicstus - SICStus Prolog
swi     - SWI Prolog")
(make-variable-buffer-local 'prolog-system)


;; [PM]
(defmacro prolog-system-override (fun arglist &rest body)
  "Syntax (prolog-system-override FUN ARGLIST . BODY)
If FUN has a prolog-system property then apply it to ARGLIST.
Otherwise evaluate body.
Useful to allow prolog-system to override the function body of the default implementation.
The following example will make prolog-foo-on call a prolog-system
specific replacement if it exists:
(defun prolog-foo-on ()
  \"...\"
  (interactive)
  (prolog-system-override prolog-foo-on ()
      (prolog-process-insert-and-send-string prolog-foo-on-string)))
"

   ;; (if (get 'FUN prolog-system)
   ;;     (funcall (get 'FUN prolog-system) . ARGLIST)
   ;;   (progn
   ;;     . BODY))
  ;; Backquotes would be better
  (list 'if (list 'get (list 'quote fun) 'prolog-system)
        (cons 'funcall (cons (list 'get (list 'quote fun) 'prolog-system) arglist))
        (cons 'progn body)))

;; [PM] Indent as defun
(put 'prolog-system-override 'lisp-indent-function 'defun)
   
  


;; NB: This alist can not be processed in prolog-mode-variables to
;; create a prolog-system-version-i variable since it is needed
;; prior to the call to prolog-mode-variables.
(defvar prolog-system-version
  '((sicstus  (eval sicstus-version))
    (swi      (0 . 0))
    (mercury  (0 . 0))
    (eclipse  (3 . 7)))
  "*Alist of Prolog system versions.
The version numbers are of the format (Major . Minor).")


(defvar prolog-use-sicstus-sd t
  "*If non-nil, use the SICStus source-linked debugger.")

;; Keyboard

(defvar prolog-electric-newline-flag t
  "*Non-nil means automatically indent the next line when the user types RET.")

(defvar prolog-hungry-delete-key-flag nil
  "*Non-nil means delete key consumes all preceding spaces.")

(defvar prolog-electric-dot-flag nil
  "*Non-nil means make dot key electric.
Electric dot appends newline or inserts head of a new clause.
If dot is pressed at the end of a line where at least one white space
precedes the point, it inserts a recursive call to the current predicate.
If dot is pressed at the beginning of an empty line, it inserts the head
of a new clause for the current predicate. It does not apply in strings
and comments.")

(defvar prolog-electric-underscore-flag nil
  "*Non-nil means make underscore key electric.
Electric underscore replaces the current variable with underscore.
If underscore is pressed not on a variable then it behaves as usual.")

(defvar prolog-electric-tab-flag nil
  "*Non-nil means make TAB key electric.
Electric TAB inserts spaces after parentheses, ->, and ;
in ( If -> Then ; Else) and ( Disj1 ; Disj2 ) style expressions.")

;; Indentation

(defvar prolog-indent-width tab-width
  "*The indentation width used by the editing buffer.")

(defvar prolog-align-comments-flag t
  "*Non-nil means automatically align comments when indenting.")

(defvar prolog-indent-mline-comments-flag t
  "*Non-nil means indent contents of /* */ comments.
Otherwise leave such lines as they are.")

(defvar prolog-left-indent-regexp "\\(;\\|\\*?->\\)"
  "*Regexp for character sequences after which next line is indented.
Next line after such a regexp is indented to the opening paranthesis level.")

(defvar prolog-paren-indent 4
  "*The indentation increase for parenthesis expressions.
Only used in ( If -> Then ; Else) and ( Disj1 ; Disj2 ) style expressions.")

(defvar prolog-parse-mode 'beg-of-line
  "*The parse mode used (decides from which point parsing is done).
Legal values:
'beg-of-line   - starts parsing at the beginning of a line, unless the
                 previous line ends with a backslash. Fast, but has
                 problems detecting multiline /* */ comments.
'beg-of-clause - starts parsing at the beginning of the current clause.
                 Slow, but copes better with /* */ comments.")

;; Font locking

(defvar prolog-keywords
  '((eclipse
     ("use_module" "begin_module" "module_interface" "dynamic"
      "external" "export" "dbgcomp" "nodbgcomp" "compile"))
    (mercury
     ("all" "else" "end_module" "equality" "external" "fail" "func" "if"
      "implementation" "import_module" "include_module" "inst" "instance"
      "interface" "mode" "module" "not" "pragma" "pred" "some" "then" "true"
      "type" "typeclass" "use_module" "where"))
    (sicstus
     (eval (if (boundp 'sicstus-keywords)
               sicstus-keywords
             '("block" "dynamic" "mode" "module" "multifile" "meta_predicate"
               "parallel" "public" "sequential" "volatile")))
     )
    (swi
     ("discontiguous" "dynamic" "ensure_loaded" "export" "export_list" "import"
      "meta_predicate" "module" "module_transparent" "multifile" "require"
      "use_module" "volatile"))
    (t
     ("dynamic" "module")))
  "*Alist of Prolog keywords which is used for font locking of directives.")

(defvar prolog-types
  '((mercury
     ("char" "float" "int" "io__state" "string" "univ"))
    (t nil))
  "*Alist of Prolog types used by font locking.")

(defvar prolog-mode-specificators
  '((mercury
     ("bound" "di" "free" "ground" "in" "mdi" "mui" "muo" "out" "ui" "uo"))
    (t nil))
  "*Alist of Prolog mode specificators used by font locking.")

(defvar prolog-determinism-specificators
  '((mercury
     ("cc_multi" "cc_nondet" "det" "erroneous" "failure" "multi" "nondet"
      "semidet"))
    (t nil))
  "*Alist of Prolog determinism specificators used by font locking.")

(defvar prolog-directives
  '((mercury
     ("^#[0-9]+"))
    (t nil))
  "*Alist of Prolog source code directives used by font locking.")

;; Inferior mode

(defvar prolog-program-name
  '(((getenv "EPROLOG") (eval (getenv "EPROLOG")))
    (eclipse "eclipse")
    (mercury nil)
    (sicstus (eval (if (boundp 'sicstus-program-name)
                       sicstus-program-name
                     "sicstus")))
    (swi "pl")
    (t "prolog"))
  "*Alist of program names for invoking an inferior Prolog with `run-prolog'.")

(defvar prolog-program-switches
  '((sicstus (eval (if (boundp 'sicstus-program-switches)
                       sicstus-program-switches
                     '("-i"))))
    (t nil))
  "*Alist of switches given to inferior Prolog run with `run-prolog'.")

(defvar prolog-consult-string
  '((eclipse "[%f].")
    (mercury nil)
    (sicstus (eval (if (boundp 'sicstus-consult-string)
                       sicstus-consult-string
		     "prolog:zap_file(%m,%b,consult).")))
    (swi "[%f].")
    (t "reconsult(%f)."))
  "*Alist of strings defining predicate for reconsulting.

Some parts of the string are replaced:
`%f' by the name of the consulted file (can be a temporary file)
`%b' by the file name of the buffer to consult
`%m' by the module name and name of the consulted file separated by colon
`%l' by the line offset into the file. This is 0 unless consulting a
     region of a buffer, in which case it is the number of lines before
     the region.")

(defvar prolog-compile-string
  '((eclipse "[%f].")
    (mercury "mmake ")
    (sicstus (eval
              (if (boundp 'sicstus-compile-string)
                  sicstus-compile-string
                "prolog:zap_file(%m,%b,compile,%l).")))
    (swi "[%f].")
    (t "compile(%f)."))
  "*Alist of strings and lists defining predicate for recompilation.

Some parts of the string are replaced:
`%f' by the name of the compiled file (can be a temporary file)
`%b' by the file name of the buffer to compile
`%m' by the module name and name of the compiled file separated by colon
`%l' by the line offset into the file. This is 0 unless compiling a
     region of a buffer, in which case it is the number of lines before
     the region.

If `prolog-program-name' is non-nil, it is a string sent to a Prolog process.
If `prolog-program-name' is nil, it is an argument to the `compile' function.")

(defvar prolog-eof-string "end_of_file.\n"
  "*Alist of strings that represent end of file for prolog.
nil means send actual operating system end of file.")

(defvar prolog-prompt-regexp
  '((eclipse "^[a-zA-Z0-9()]* *\\?- \\|^\\[[a-zA-Z]* [0-9]*\\]:")
    (sicstus (eval (if (boundp 'sicstus-prompt-regexp)
                       sicstus-prompt-regexp
                     "| [ ?][- ] *")))
    (swi "^[1-9][0-9]* \\?- \\|^| +")
    (t "^ *\\?-"))
  "*Alist of prompts of the prolog system command line.")

(defvar prolog-continued-prompt-regexp
  '((sicstus (eval (if (boundp 'sicstus-continued-prompt-regexp)
                       sicstus-continued-prompt-regexp
                     "^\\(| +\\|     +\\)")))
    (t "^|: +"))
  "*Alist of regexps matching the prompt when consulting `user'.")

;; [PM] Used for to extract module from program text.
(defvar prolog-module-directive-pattern
  '((sicstus (eval (if (boundp 'sicstus-module-directive-pattern)
                       sicstus-module-directive-pattern
                     '(":-\\s-*module(\\s-*'?\\(\\w+\\)'?\\W*" . 1))))
    (t nil))
  "*Alist of pairs (REGEXP . MATCH-NO) for extracting module name from module directive.
Match-string MATCH-NO is the module of a module directive matching REGEXP.")

(defvar prolog-debug-on-string "debug.\n"
  "*Predicate for enabling debug mode.")

(defvar prolog-debug-off-string "nodebug.\n"
  "*Predicate for disabling debug mode.")

(defvar prolog-trace-on-string "trace.\n"
  "*Predicate for enabling tracing.")

(defvar prolog-trace-off-string "notrace.\n"
  "*Predicate for disabling tracing.")

(defvar prolog-zip-on-string "zip.\n"
  "*Predicate for enabling zip mode for SICStus.")

(defvar prolog-zip-off-string "nozip.\n"
  "*Predicate for disabling zip mode for SICStus.")

(defvar prolog-use-standard-consult-compile-method-flag t
  "*Non-nil means use the standard compilation method.
Otherwise the new compilation method will be used. This
utilises a special compilation buffer with the associated
features such as parsing of error messages and automatically
jumping to the source code responsible for the error.

Warning: the new method is so far only experimental and
does contain bugs. The recommended setting for the novice user
is non-nil for this variable.")


;; Miscellaneous

(defvar prolog-use-prolog-tokenizer-flag t
  "*Non-nil means then use the internal prolog tokenizer for indentation etc.
Otherwise use `parse-partial-sexp' which is faster but sometimes incorrect.")

(defvar prolog-imenu-flag nil
  "*Non-nil means then add a predicate index menu for all prolog files.")

(defvar prolog-imenu-max-lines 3000
  "*The maximum number of lines of the file for imenu to be enabled.
Relevant only when `prolog-imenu-flag' is non-nil.")

(defvar prolog-info-predicate-index
  "(sicstus)Predicate Index"
  "*The info node for the SICStus predicate index.")

(defvar prolog-underscore-wordchar-flag t
  "*Non-nil means that underscore (_) is a word-constituent character.")


;;-------------------------------------------------------------------
;; Internal variables
;;-------------------------------------------------------------------

(defvar prolog-emacs 
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      'xemacs
    'gnuemacs)
  "The variant of Emacs we're running.
Valid values are 'gnuemacs and 'xemacs.")

(defvar prolog-known-systems '(eclipse mercury sicstus swi))

(defvar prolog-temp-filename "")   ; Later set by `prolog-temporary-file'

(defvar prolog-mode-syntax-table nil)
(defvar prolog-mode-abbrev-table nil)
(defvar prolog-mode-map nil)
(defvar prolog-upper-case-string ""
  "A string containing all upper case characters.
Set by prolog-build-case-strings.")
(defvar prolog-lower-case-string ""
  "A string containing all lower case characters.
Set by prolog-build-case-strings.")
  
(defvar prolog-atom-char-regexp ""
  "Set by prolog-set-atom-regexps.")
;; "Regexp specifying characters which constitute atoms without quoting.")
(defconst prolog-atom-regexp ""
  "Set by prolog-set-atom-regexps.")

(defconst prolog-left-paren "[[({]" 
  "The characters used as left parentheses for the indentation code.")
(defconst prolog-right-paren "[])}]"
  "The characters used as right parentheses for the indentation code.")
;xx
;(defconst prolog-atom-char-regexp "[a-zA-Z0-9_$]"
;  "Regexp specifying characters which constitute atoms without quoting.")
;(defconst prolog-atom-regexp (format "[a-z$]+%s*" prolog-atom-char-regexp)
;  "Regexp specifying non-quoted atoms.")

(defconst prolog-quoted-atom-regexp
  "\\(^\\|[^0-9]\\)\\('\\([^\n']\\|\\\\'\\)*'\\)"
  "Regexp matching a quoted atom.")
(defconst prolog-string-regexp
  "\\(\"\\([^\n\"]\\|\\\\\"\\)*\"\\)"
  "Regexp matching a string.")
(defconst prolog-head-delimiter "\\(:-\\|\\+:\\|-:\\|\\+\\?\\|-\\?\\|-->\\)"
  "A regexp for matching on the end delimiter of a head (e.g. \":-\").")

(defvar prolog-compilation-buffer "*prolog-compilation*"
  "Name of the output buffer for Prolog compilation/consulting.")

(defvar prolog-keywords-i nil)
(defvar prolog-types-i nil)
(defvar prolog-mode-specificators-i nil)
(defvar prolog-determinism-specificators-i nil)
(defvar prolog-directives-i nil)
(defvar prolog-program-name-i nil)
(defvar prolog-program-switches-i nil)
(defvar prolog-consult-string-i nil)
(defvar prolog-compile-string-i nil)
(defvar prolog-eof-string-i nil)
(defvar prolog-prompt-regexp-i nil)
(defvar prolog-continued-prompt-regexp-i nil)
(defvar prolog-help-function-i nil)


;;-------------------------------------------------------------------
;; Prolog mode
;;-------------------------------------------------------------------

;; Example: (prolog-atleast-version '(3 . 6))
(defun prolog-atleast-version (version)
  "Return t if the version of the current prolog system is VERSION or later.
VERSION is of the format (Major . Minor)"
  ;; Version.major < major or
  ;; Version.major = major and Version.minor <= minor
  (let* ((thisversion (prolog-find-value-by-system prolog-system-version))
	 (thismajor (car thisversion))
	 (thisminor (cdr thisversion)))
    (or (< (car version) thismajor)
	(and (= (car version) thismajor)
	     (<= (cdr version) thisminor)))
    ))

(if prolog-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (if prolog-underscore-wordchar-flag
	(modify-syntax-entry ?_ "w" table)
      (modify-syntax-entry ?_ "_" table))
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\' "." table)
    (setq prolog-mode-syntax-table table)))

(define-abbrev-table 'prolog-mode-abbrev-table ())

(defun prolog-find-value-by-system (alist)
  "Get value from ALIST according to `prolog-system'."
  (if (listp alist)
      (let (result
	    id)
	(while alist
	  (setq id (car (car alist)))
	  (if (or (eq id prolog-system)
		  (eq id t)
		  (and (listp id)
		       (eval id)))
	      (progn
		(setq result (car (cdr (car alist))))
		(if (and (listp result)
			 (eq (car result) 'eval))
		    (setq result (eval (car (cdr result)))))
		(setq alist nil))
	    (setq alist (cdr alist))))
	result)
    alist))

(defun prolog-mode-variables ()
  "Set some common variables to Prolog code specific values."
  (setq local-abbrev-table prolog-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "[ \t]*$\\|" page-delimiter)) ;'%%..'
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'prolog-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  ;; This complex regexp makes sure that comments cannot start
  ;; inside quoted atoms or strings
  (setq comment-start-skip 
	(format "^\\(\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\)\\(/\\*+ *\\|%%+ *\\)" 
		prolog-quoted-atom-regexp prolog-string-regexp))
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'prolog-comment-indent)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'prolog-comment-indent)
  (make-local-variable 'parens-require-spaces)
  (setq parens-require-spaces nil)
  ;; Initialize Prolog system specific variables
  (let ((vars '(prolog-keywords prolog-types prolog-mode-specificators
		prolog-determinism-specificators prolog-directives
		prolog-program-name prolog-program-switches
		prolog-consult-string prolog-compile-string prolog-eof-string
		prolog-prompt-regexp prolog-continued-prompt-regexp
		prolog-help-function)))
    (while vars
      (set (intern (concat (symbol-name (car vars)) "-i"))
	   (prolog-find-value-by-system (eval (car vars))))
      (setq vars (cdr vars))))
  (if (null prolog-program-name-i)
;      (setq compile-command prolog-compile-string-i))
      (progn
	(make-local-variable 'compile-command)
	(setq compile-command prolog-compile-string-i)))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(prolog-font-lock-keywords nil nil ((?_ . "w"))))
)

(defun prolog-mode-keybindings-common (map)
  "Define keybindings common to both Prolog modes in MAP."
  (define-key map "\C-c?" 'prolog-help-on-predicate)
  (define-key map "\C-c/" 'prolog-help-apropos)
  (define-key map "\C-c\C-d" 'prolog-debug-on)
  (define-key map "\C-c\C-t" 'prolog-trace-on)
  (define-key map "\C-c\C-z" 'prolog-zip-on)
  (define-key map "\C-c\r" 'run-prolog))

(defun prolog-mode-keybindings-edit (map)
  "Define keybindings for Prolog mode in MAP."
  (define-key map "\M-a" 'prolog-beginning-of-clause)
  (define-key map "\M-e" 'prolog-end-of-clause)
;   (define-key map "\M-p" 'prolog-previous-clause)
;   (define-key map "\M-n" 'prolog-next-clause)
  (define-key map "\C-\M-a" 'prolog-beginning-of-predicate)
  (define-key map "\C-\M-e" 'prolog-end-of-predicate)
  (define-key map "\M-\C-c" 'prolog-mark-clause)
  (define-key map "\M-\C-h" 'prolog-mark-predicate)
  (define-key map "\M-\C-n" 'prolog-forward-list)
  (define-key map "\M-\C-p" 'prolog-backward-list)
  (define-key map "\C-c\C-n" 'prolog-insert-predicate-template)
  (define-key map "\C-c\C-s" 'prolog-insert-predspec)
  (define-key map "\M-\r" 'prolog-insert-next-clause)
  (define-key map "\C-c\C-va" 'prolog-variables-to-anonymous)
  (define-key map "\C-c\C-v\C-s" 'prolog-view-predspec)

  (define-key map "\177" 'prolog-electric-delete)
  (define-key map "." 'prolog-electric-dot)
  (define-key map "_" 'prolog-electric-underscore)
  (if prolog-electric-newline-flag 
      (define-key map "\r" 'newline-and-indent))

  (define-key map "\C-c\C-p" 'prolog-consult-predicate)
  (define-key map "\C-c\C-r" 'prolog-consult-region)
  (define-key map "\C-c\C-b" 'prolog-consult-buffer)
  (define-key map "\C-c\C-f" 'prolog-consult-file)
  (define-key map "\C-c\C-cp" 'prolog-compile-predicate)
  (define-key map "\C-c\C-cr" 'prolog-compile-region)
  (define-key map "\C-c\C-cb" 'prolog-compile-buffer)
  (define-key map "\C-c\C-cf" 'prolog-compile-file))

(defun prolog-mode-keybindings-inferior (map)
  "Define keybindings for inferior Prolog mode in MAP."
  ;; No inferior mode specific keybindings now.
  )

(if prolog-mode-map
    ()
  (setq prolog-mode-map (make-sparse-keymap))
  (prolog-mode-keybindings-common prolog-mode-map)
  (prolog-mode-keybindings-edit prolog-mode-map))
  

(defvar prolog-mode-hook nil
  "List of functions to call after the prolog mode has initialised.")

;; [PM]
(defvar prolog-mode-nonexisting-prolog-system-libraries '()
  "List of prolog-systems for which (require <prolog-system>-support) fails.
Used by prolog-load-system-support, do not touch.")

;; [PM] attempt to load prolog-system specific code
;;      Should be called early in any autloaded funs in this file
(defun prolog-load-system-support (&optional system)
  "Attempt to load library for prolog-system SYSTEM (optional).
If SYSTEM is nil then it defaults to the value of prolog-system.
Uses require and caches failed attempts for speed."
  (or system (setq system prolog-system))

  (if (and system
           (not (memq system prolog-mode-nonexisting-prolog-system-libraries)))
      (let ((support-lib (intern (concat (symbol-name system) "-support"))))
        (condition-case nil
            (progn
              (require support-lib) ; XEmacs does not have a no-error arg
              t)
          (error
           (setq prolog-mode-nonexisting-prolog-system-libraries
                 (cons system prolog-mode-nonexisting-prolog-system-libraries))
           nil)))))
  
;;;###autoload
(defun prolog-mode (&optional system)
  "Major mode for editing Prolog code.

Blank lines and `%%...' separate paragraphs.  `%'s starts a comment
line and comments can also be enclosed in /* ... */.

If an optional argument SYSTEM is non-nil, set up mode for the given system.

Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil."
  (interactive)
  (prolog-load-system-support (or system prolog-system)) ; [PM]
  (kill-all-local-variables)
  (if system (setq prolog-system system))
  
  (use-local-map
   ;; [PM] the prolog-system specific maps are not used
   ;;      Should they be reinstated then they should be created in a
   ;;      portable way using set-keymap-parent
   ;;    ;; This does not work with Xemacs 19, so it is not currently used
   ;;    ;; with Xemacs. It seems that the representation of a mode-map is
   ;;    ;; different between Gnu Emacs and Xemacs.
   ;; ;xx
   ;; (if (and prolog-system
   ;;          (not (eq prolog-emacs 'xemacs)))
   ;;     (eval (intern (concat "prolog-mode-map-" (symbol-name prolog-system))))
     prolog-mode-map
   ;;   )
   )
  (setq major-mode 'prolog-mode)
  (setq mode-name (concat "Prolog"
			  (cond
			   ((eq prolog-system 'eclipse) "[ECLiPSe]")
			   ((eq prolog-system 'mercury) "[Mercury]")
			   ((eq prolog-system 'sicstus) "[SICStus]")
			   ((eq prolog-system 'swi) "[SWI]")
			   (t ""))))
  (set-syntax-table prolog-mode-syntax-table)
  (prolog-mode-variables)
  (prolog-build-case-strings)
  (prolog-set-atom-regexps)

  ;; Add predicate index menu
  (make-variable-buffer-local 'imenu-create-index-function)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
;Emil (does not detect facts, problems in Windows? Try TACIT main.pl!)
;  (setq imenu-create-index-function 'prolog-create-predicate-index)
;Milan (this has problems with object methods...)
  (setq imenu-prev-index-position-function 'prolog-beginning-of-predicate)
  (setq imenu-extract-index-name-function 'prolog-get-predspec)

  (if (and prolog-imenu-flag
	   (< (count-lines (point-min) (point-max)) prolog-imenu-max-lines))
      (imenu-add-to-menubar "Predicates"))
  
  (run-hooks 'prolog-mode-hook)
  ;; [PM] Turn on font lock if appropriate.
  ;; It is better to do it here than relying on the settings for
  ;; global font locking since:
  ;; . XEmacs will only turn font-lock on from find-file-hooks and
  ;;   thus not for prolog-mode buffer not visiting files
  ;; . For buffers not visiting files FSF Emacs will turn it on from
  ;;   post-command-hook which will cause an annoying delay and
  ;;   flicker for the first character typed after prolog-mode is
  ;;   turned on. This was an issue with SICStus source debugger but
  ;;   is a general problem.
  ;; Do it after prolog-mode-hooks since hooks may turn on font-lock
  (cond ((fboundp 'turn-on-font-lock-if-enabled) ; FSF Emacs
         (turn-on-font-lock-if-enabled))
        ((fboundp 'font-lock-set-defaults) ; XEmacs
         ;; Available in FSF Emacs as well but with different
         ;; semantics
         ;; In XEmacs is is safe to call multiple times and fills a
         ;; similar role as turn-on-font-lock-if-enabled
         (font-lock-set-defaults))))


;;;###autoload
(defun mercury-mode ()
  "Major mode for editing Mercury programs.
Actually this is just customized `prolog-mode'."
  (interactive)
  (prolog-mode 'mercury))


;;-------------------------------------------------------------------
;; Inferior prolog mode
;;-------------------------------------------------------------------

(defvar prolog-inferior-mode-map nil)
(defvar prolog-inferior-mode-hook nil
  "List of functions to call after the inferior prolog mode has initialised.")

(defun prolog-inferior-mode ()
  ;; [PM] Used to mention send-region and send-string as 'commands'
  ;;      They are not interactive command (and they are now named
  ;;      process-send-XXX).
  ;;      Changed doc to correctly state what hooks are run.
  "Major mode for interacting with an inferior Prolog process.

The following commands are available:
\\{prolog-inferior-mode-map}

Entry to this mode calls the functions on `prolog-inferior-mode-hook',
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`prolog-inferior-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior Prolog from other buffers
using \\[prolog-consult-region].

Commands:
Tab indents for Prolog; with argument, shifts rest
 of expression rigidly with the current line.
Paragraphs are separated only by blank lines and '%%'. '%'s start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-delchar-or-maybe-eof] sends end-of-file as input.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands,
imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops, likewise.
\\[comint-quit-subjob] sends quit signal, likewise."
  (interactive)
  (cond ((not (eq major-mode 'prolog-inferior-mode))
	 (kill-all-local-variables)
	 (comint-mode)
	 (setq comint-input-filter 'prolog-input-filter)
	 (setq major-mode 'prolog-inferior-mode)
	 (setq mode-name "Inferior Prolog")
	 (setq mode-line-process '(": %s"))
	 (prolog-mode-variables)
	 (if prolog-inferior-mode-map
	     ()
	   (setq prolog-inferior-mode-map (copy-keymap comint-mode-map))
	   (prolog-mode-keybindings-common prolog-inferior-mode-map)
	   (prolog-mode-keybindings-inferior prolog-inferior-mode-map))
	 (use-local-map prolog-inferior-mode-map)
	 (setq comint-prompt-regexp prolog-prompt-regexp-i)
	 (make-variable-buffer-local 'shell-dirstack-query)
	 (setq shell-dirstack-query "pwd.") ; [PM] Ouch 
	 (run-hooks 'prolog-inferior-mode-hook))))

(defun prolog-input-filter (str)
  (cond ((string-match "\\`\\s *\\'" str) nil) ;whitespace
	((not (eq major-mode 'prolog-inferior-mode)) t)
	((= (length str) 1) nil)	;one character
	((string-match "\\`[rf] *[0-9]*\\'" str) nil) ;r(edo) or f(ail)
	(t t)))

(defvar *prolog-extra-arguments* nil
  "List of extra arguments passed on the command line when starting prolog.
The global value is ignored, it is bound dynamically by run-prolog.")

;;;###autoload
(defun run-prolog (arg &optional command-line)
  "Run an inferior Prolog process, input and output via buffer *prolog*.
With prefix argument ARG, prompt for extra arguments and
restart the Prolog process if running before.
Arguments are read as a Lisp list.
"
  ;; [PM] 4.1 SPRM 11526 prompt for extra arguments but only if prefix arg
  (interactive (list current-prefix-arg (if current-prefix-arg (prolog-read-extra-args) '())))
  (prolog-load-system-support) ; [PM]
  (let ((*prolog-extra-arguments* command-line))
  (prolog-system-override run-prolog (arg)
  (if (and arg (get-buffer-process "*prolog*"))	;[PD] SPRM 10626
      (progn
	(process-send-string (get-buffer-process "*prolog*") "halt.\n")	;[PD] SPRM 10626
	(while (get-buffer-process "*prolog*") (sit-for 0.1))))	;[PD] SPRM 10626
  (let ((buff (buffer-name)))
    (if (not (string= buff "*prolog*"))
	(prolog-goto-prolog-process-buffer))
    (prolog-mode-variables)
    (prolog-ensure-process)
    ))))

;; [PM] 4.1
(defun prolog-read-extra-args ()
  "Read a list of extra command line arguments as a Lisp list."
  (read-from-minibuffer "Argument list: "
                                   (cons "(  )" 2)
                                   nil
                                   t    ; read
                                   ;; FIXME: history
                                   ))

;; [PM] A note:
;;      If the names "prolog" and "*prolog*" where not hardwired then
;;      we could have multiple prologs (possibly using different
;;      prolog systems) at the same time. By ensuring that this fun
;;      returns the appropriate process buffer it is possible to
;;      isolate the dependency on the "prolog" name to this single
;;      function.  Currently this is not utilized in the rest of this
;;      file. Beware though, there are other global state that assumes
;;      a single prolog sub-process. Having more than one is likely to
;;      malfunction in subtle ways, e.g. in the process filter.


(defun prolog-ensure-process (&optional wait no-create)
  "Ensure that there is a running Prolog process.
Return the name of its buffer
If the optional argument WAIT is non-nil, wait for Prolog prompt specified by
the variable `prolog-prompt-regexp'.
Optional third argument NO-CREATE if non-nil means do not start a process,
may still return the name of the Prolog process buffer if it exists."
  (prolog-system-override prolog-ensure-process (wait no-create)
    (if (null prolog-program-name-i)
        (error "This Prolog system has defined no interpreter."))

    (let* ((prolog-process-name "prolog")      ; [PM] this should be a global
           (buf-name (concat "*" prolog-process-name "*"))) ; see make-comint docs
      (cond ((comint-check-proc buf-name)
             buf-name)
            (no-create
             (and (get-buffer buf-name)
                  buf-name))
            (t
             (apply 'make-comint prolog-process-name prolog-program-name-i nil
                    (append prolog-program-switches-i *prolog-extra-arguments*))
             (save-excursion
               (set-buffer buf-name)
               (prolog-inferior-mode)
               (if wait
                   (progn
                     (goto-char (point-max))
                     (while
                         (save-excursion
                           (not
                            (re-search-backward
                             (concat "\\(" prolog-prompt-regexp-i "\\)" "\\=")
                             nil t)))
                       (sit-for 0.1)))))
             buf-name)))))


(defun prolog-process-insert-string (process string)
  "Insert STRING into inferior Prolog buffer running PROCESS."
  ;; Copied from elisp manual, greek to me
  (let ((buf (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer process))
	  (setq moving (= (point) (process-mark process)))
	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	    (goto-char (process-mark process))
	    (insert string)
	    (set-marker (process-mark process) (point)))
	  (if moving (goto-char (process-mark process))))
      (set-buffer buf))))

;; [PM] 1.41
(defun prolog-process-insert-and-send-string (string &optional process)
  "Insert STRING into and send it to process of inferior Prolog buffer running PROCESS."
  
  (if (null process) (setq process (get-buffer-process "*prolog*"))) ;[PD] SPRM 10626
  (setq process (get-process process))  ;works for string and process
  (prolog-process-insert-string process string)
  (process-send-string process string))


;;------------------------------------------------------------
;; Old consulting and compiling functions
;;------------------------------------------------------------

(defun prolog-old-process-region (compilep start end)
  "Process the region limited by START and END positions.
If COMPILEP is non-nil then use compilation, otherwise consulting."
   (prolog-ensure-process)
   (let ((tmpfile prolog-temp-filename)
         ;; [PM] not used:
	 ;; (process (get-process "prolog"))
	 (first-line (1+ (count-lines 
			  1 ; [PM] OK even if narrowing
                          ;; [PM] Was (point-min)
			  (save-excursion
			    (goto-char start)
			    (point))))))
     (write-region start end tmpfile)
     (process-send-string
      (get-buffer-process "*prolog*") (prolog-build-prolog-command ;[PD] SPRM 10626
		compilep tmpfile (prolog-bsts buffer-file-name)
		first-line))
     (prolog-goto-prolog-process-buffer)))

(defun prolog-old-process-predicate (compilep)
  "Process the predicate around point.
If COMPILEP is non-nil then use compilation, otherwise consulting."
  (prolog-old-process-region
   compilep (prolog-pred-start) (prolog-pred-end)))

(defun prolog-old-process-buffer (compilep)
  "Process the entire buffer.
If COMPILEP is non-nil then use compilation, otherwise consulting."
  (prolog-old-process-region compilep (point-min) (point-max)))

(defun prolog-old-process-file (compilep)
  "Process the file of the current buffer.
If COMPILEP is non-nil then use compilation, otherwise consulting."
  (save-some-buffers)
  (prolog-ensure-process)
  (let ((filename (prolog-bsts buffer-file-name)))
    (process-send-string
     (get-buffer-process "*prolog*") ;[PD] SPRM 10626
     (prolog-build-prolog-command compilep filename filename))
    (prolog-goto-prolog-process-buffer)))


;;------------------------------------------------------------
;; Consulting and compiling
;;------------------------------------------------------------

;;; Interactive interface functions, used by both the standard
;;; and the experimental consultation and compilation functions
(defun prolog-consult-file ()
  "Consult file of current buffer."
  (interactive)
  (prolog-system-override prolog-consult-file ()
    (if prolog-use-standard-consult-compile-method-flag
        (prolog-old-process-file nil)
      (prolog-consult-compile-file nil))))

(defun prolog-consult-buffer ()
  "Consult buffer."
  (interactive)
  (prolog-system-override prolog-consult-buffer ()
    (if prolog-use-standard-consult-compile-method-flag
        (prolog-old-process-buffer nil)
      (prolog-consult-compile-buffer nil))))

(defun prolog-consult-region (beg end)
  "Consult region between BEG and END."
  (interactive "r")
  (prolog-system-override prolog-consult-region (beg end)
    (if prolog-use-standard-consult-compile-method-flag
        (prolog-old-process-region nil beg end)
      (prolog-consult-compile-region nil beg end))))

(defun prolog-consult-predicate ()
  "Consult the predicate around current point."
  (interactive)
  (prolog-system-override prolog-consult-predicate ()
    (if prolog-use-standard-consult-compile-method-flag
        (prolog-old-process-predicate nil)
      (prolog-consult-compile-predicate nil))))

(defun prolog-compile-file ()
  "Compile file of current buffer."
  (interactive)
  (prolog-system-override prolog-compile-file ()
    (if prolog-use-standard-consult-compile-method-flag
        (prolog-old-process-file t)
      (prolog-consult-compile-file t))))

(defun prolog-compile-buffer ()
  "Compile buffer."
  (interactive)
  (prolog-system-override prolog-compile-buffer ()
    (if prolog-use-standard-consult-compile-method-flag
        (prolog-old-process-buffer t)
      (prolog-consult-compile-buffer t))))

(defun prolog-compile-region (beg end)
  "Compile region between BEG and END."
  (interactive "r")
  (prolog-system-override prolog-compile-region (beg end)
    (if prolog-use-standard-consult-compile-method-flag
        (prolog-old-process-region t beg end)
      (prolog-consult-compile-region t beg end))))

(defun prolog-compile-predicate ()
  "Compile the predicate around current point."
  (interactive)
  (prolog-system-override prolog-compile-predicate ()
    (if prolog-use-standard-consult-compile-method-flag
        (prolog-old-process-predicate t)
      (prolog-consult-compile-predicate t))))

(defun prolog-buffer-module ()
  "Select Prolog module name appropriate for current buffer.
Bases decision on buffer contents (-*- line)."
  ;; Look for -*- ... module: MODULENAME; ... -*-
  (let (beg end)
    (save-excursion
      (save-restriction
        (widen)                         ; [PM]
        (goto-char (point-min))
        (skip-chars-forward " \t")
        (and (search-forward "-*-" (save-excursion (end-of-line) (point)) t)
             (progn
               (skip-chars-forward " \t")
               (setq beg (point))
               (search-forward "-*-" (save-excursion (end-of-line) (point)) t))
             (progn
               (forward-char -3)
               (skip-chars-backward " \t")
               (setq end (point))
               (goto-char beg)
               (and (let ((case-fold-search t))
                      (search-forward "module:" end t))
                    (progn
                      (skip-chars-forward " \t")
                      (setq beg (point))
                      (if (search-forward ";" end t)
                          (forward-char -1)
                        (goto-char end))
                      (skip-chars-backward " \t")
                      (buffer-substring beg (point))))))))))


(defun prolog-build-prolog-command (compilep file buffername 
				    &optional first-line)
  "Make Prolog command for FILE compilation/consulting.
If COMPILEP is non-nil, consider compilation, otherwise consulting."
  (let* ((compile-string
	  (if compilep prolog-compile-string-i prolog-consult-string-i))
	 (module (prolog-buffer-module))
	 (file-name (concat "'" file "'"))
	 (module-name (if module (concat "'" module "'")))
	 (module-file (if module
			  (concat module-name ":" file-name)
			file-name))
	 strbeg strend
	 (lineoffset (if first-line
			 (- first-line 1)
		       0)))

    ;; Assure that there is a buffer name
    (if (not buffername)
	(error "The buffer is not saved"))

    (if (not (string-match "^'.*'$" buffername)) ; Add quotes
	(setq buffername (concat "'" buffername "'")))
    (while (string-match "%m" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg module-file strend)))
    (while (string-match "%f" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg file-name strend)))
    (while (string-match "%b" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg buffername strend)))
    (while (string-match "%l" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg (format "%d" lineoffset) strend)))
    (concat compile-string "\n")))

;;; The rest of this page is experimental code!

;; Global variables for process filter function
(defvar prolog-process-flag nil
  "Non-nil means that a prolog task (i.e. a consultation or compilation job) 
is running.")
(defvar prolog-consult-compile-output ""
  "Hold the unprocessed output from the current prolog task.")
(defvar prolog-consult-compile-first-line 1
  "The number of the first line of the file to consult/compile.
Used for temporary files.")
(defvar prolog-consult-compile-file nil
  "The file to compile/consult (can be a temporary file).")
(defvar prolog-consult-compile-real-file nil
  "The file name of the buffer to compile/consult.")

(defun prolog-consult-compile (compilep file &optional first-line)
  "Consult/compile FILE.
If COMPILEP is non-nil, perform compilation, otherwise perform CONSULTING.
COMMAND is a string described by the variables `prolog-consult-string'
and `prolog-compile-string'.
Optional argument FIRST-LINE is the number of the first line in the compiled
region.

This function must be called from the source code buffer."
  (if prolog-process-flag
      (error "Another Prolog task is running."))
  (prolog-ensure-process t)
  (let* ((buffer (get-buffer-create prolog-compilation-buffer))
	 (real-file buffer-file-name)
	 (command-string (prolog-build-prolog-command compilep file 
						      real-file first-line))
	 (process (get-buffer-process "*prolog*")) ;[PD] SPRM 10626
	 (old-filter (process-filter process))
         ;; [PM] SP 3.9b4 prolog-parse-compilation-errors should be a
         ;; buffer local and thus prolog system local variable.
         (parse-errors-function (if (boundp 'prolog-parse-compilation-errors)
                                    prolog-parse-compilation-errors))
)
    (save-excursion
      (set-buffer buffer)
      (delete-region (point-min) (point-max))
      (compilation-mode)
      ;; Setting up font-locking for this buffer
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults 
	    '(prolog-font-lock-keywords nil nil ((?_ . "w"))))

      ;; [PM] SP 3.9b4 made it generic
      (if parse-errors-function
          (progn
	    (make-local-variable 'compilation-parse-errors-function)
	    (setq compilation-parse-errors-function
		  parse-errors-function)))

      (insert command-string "\n"))
    (save-selected-window
      (pop-to-buffer buffer))
    (setq prolog-process-flag t
	  prolog-consult-compile-output ""
	  prolog-consult-compile-first-line (if first-line (1- first-line) 0)
	  prolog-consult-compile-file file
	  prolog-consult-compile-real-file (if (string= 
						file buffer-file-name)
					       nil
					     real-file))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (set-process-filter process 'prolog-consult-compile-filter)
      (process-send-string
       (get-buffer-process "*prolog*")	;[PD] SPRM 10626
       command-string)
;       (prolog-build-prolog-command compilep file real-file first-line))
      (while (and prolog-process-flag
		  (accept-process-output process 10)) ; 10 secs is ok?
	(sit-for 0.1)
	(if (not (get-buffer-process "*prolog*")) ;[PD] SPRM 10626
	    (setq prolog-process-flag nil)))
      (insert (if compilep
		  "\nCompilation finished.\n"
		"\nConsulted.\n"))
      (set-process-filter process old-filter))))

(defun prolog-consult-compile-filter (process output)
  "Filter function for Prolog compilation PROCESS.
Argument OUTPUT is a name of the output file."
  ;;(message "start")
  (setq prolog-consult-compile-output
	(concat prolog-consult-compile-output output))
  ;;(message "pccf1: %s" prolog-consult-compile-output)
  ;; Iterate through the lines of prolog-consult-compile-output
  (let (outputtype)
    (while (and prolog-process-flag
		(or
		 ;; Trace question
		 (progn 
		   (setq outputtype 'trace)
		   (and (eq prolog-system 'sicstus)
			(string-match
			 "^[ \t]*[0-9]+[ \t]*[0-9]+[ \t]*Call:.*? "
			 prolog-consult-compile-output)))
		 
		 ;; Match anything
		 (progn 
		   (setq outputtype 'normal)
		   (string-match "^.*\n" prolog-consult-compile-output))
		   ))
      ;;(message "outputtype: %s" outputtype)

      (setq output (match-string 0 prolog-consult-compile-output))
      ;; remove the text in output from prolog-consult-compile-output
      (setq prolog-consult-compile-output
	    (substring prolog-consult-compile-output (length output)))
      ;;(message "pccf2: %s" prolog-consult-compile-output)
      
      ;; If temporary files were used, then we change the error
      ;; messages to point to the original source file.
      (cond

       ;; If the prolog process was in trace mode then it requires
       ;; user input
       ((and (eq prolog-system 'sicstus) 
	     (eq outputtype 'trace))
	(let (input)
	  (setq input (concat (read-string output) "\n"))
	  (process-send-string (get-buffer-process "*prolog*") input);[PD] SPRM 10626
	  (setq output (concat output input))))

       ((eq prolog-system 'sicstus)
	(if (and prolog-consult-compile-real-file
		 (string-match
		  "\\({.*:.* in line[s ]*\\)\\([0-9]+\\)-\\([0-9]+\\)" output))
	    (setq output (replace-match
			  ;; Adds a {processing ...} line so that 
			  ;; `prolog-parse-sicstus-compilation-errors'
			  ;; finds the real file instead of the temporary one.
			  ;; Also fixes the line numbers.
			  (format "Added by Emacs: {processing %s...}\n%s%d-%d"
				  prolog-consult-compile-real-file
				  (match-string 1 output)
				  (+ prolog-consult-compile-first-line
				     (string-to-number
				      (match-string 2 output)))
				  (+ prolog-consult-compile-first-line
				     (string-to-number
				      (match-string 3 output))))
			  t t output)))
	)
       ((eq prolog-system 'swi)
	(if (and prolog-consult-compile-real-file
		 (string-match (format
				"%s\\([ \t]*:[ \t]*\\)\\([0-9]+\\)"
				prolog-consult-compile-file)
			       output))
	    (setq output (replace-match
			  ;; Real filename + text + fixed linenum
			  (format "%s%s%d"
				  prolog-consult-compile-real-file
				  (match-string 1 output)
				  (+ prolog-consult-compile-first-line
				     (string-to-number
				      (match-string 2 output))))
			  t t output)))
	)
       (t ())
       )
      ;; Write the output in the *prolog-compilation* buffer
      (insert output)))

  ;; If the prompt is visible, then the task is finished
  (if (string-match prolog-prompt-regexp-i prolog-consult-compile-output)
      (setq prolog-process-flag nil)))

(defun prolog-consult-compile-file (compilep)
  "Consult/compile file of current buffer.
If COMPILEP is non-nil, compile, otherwise consult."
  (let ((file buffer-file-name))
    (if file
	(progn
	  (save-some-buffers)
	  (prolog-consult-compile compilep file))
      ;; [PM] restriction is not a file property and should not affect
      ;;      file commands.
      (save-restriction
        (widen)
        (prolog-consult-compile-region compilep (point-min) (point-max))))))

(defun prolog-consult-compile-buffer (compilep)
  "Consult/compile current buffer.
If COMPILEP is non-nil, compile, otherwise consult."
  (prolog-consult-compile-region compilep (point-min) (point-max)))

(defun prolog-consult-compile-region (compilep beg end)
  "Consult/compile region between BEG and END.
If COMPILEP is non-nil, compile, otherwise consult."
  (let ((file prolog-temp-filename)
	(lines (count-lines 1 beg)))
    (write-region beg end file nil 'no-message)
    (write-region "\n" nil file t 'no-message)
    (prolog-consult-compile compilep file
			    (if (looking-at "^") (1+ lines) lines))
    (delete-file file)))

(defun prolog-consult-compile-predicate (compilep)
  "Consult/compile the predicate around current point.
If COMPILEP is non-nil, compile, otherwise consult."
  (prolog-consult-compile-region
   compilep (prolog-pred-start) (prolog-pred-end)))


;;-------------------------------------------------------------------
;; Font-lock stuff
;;-------------------------------------------------------------------

;; Auxilliary functions
(defun prolog-make-keywords-regexp (keywords &optional protect)
  "Create regexp from the list of strings KEYWORDS.
If PROTECT is non-nil, surround the result regexp by word breaks."
  (let ((regexp
	 (if (fboundp 'regexp-opt)
	     ;; Emacs 20
	     ;; Avoid compile warnings under earlier versions by using eval
	     (eval '(regexp-opt keywords))
	   ;; Older Emacsen
	   (concat (mapconcat 'regexp-quote keywords "\\|")))
	 ))
    (if protect
	(concat "\\<\\(" regexp "\\)\\>")
      regexp)))

;; Set everything up
(defun prolog-font-lock-keywords ()
  "Set up font lock keywords for the current Prolog system."
  (prolog-system-override prolog-font-lock-keywords ()
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
                    '((prolog-warning-face nil nil t t nil)
                      (prolog-builtin-face nil nil nil nil t)
                      (prolog-redo-face nil nil nil t nil)
                      (prolog-exit-face nil nil nil nil t)
                      (prolog-exception-face nil nil t t t)))
                   ((memq font-lock-display-type '(grayscale greyscale
                                                             grayshade greyshade))
                    '((prolog-warning-face nil nil t t nil)
                      (prolog-builtin-face nil nil nil nil t)
                      (prolog-redo-face nil nil nil t nil)
                      (prolog-exit-face nil nil nil nil t)
                      (prolog-exception-face nil nil t t t)))
                   (dark-bg 		; dark colour background
                    '((prolog-warning-face "red" nil t nil nil)
                      (prolog-builtin-face "LightSkyBlue" nil nil nil nil)
                      (prolog-redo-face "darkorchid" nil nil nil nil)
                      (prolog-exit-face "green" nil nil nil nil)
                      (prolog-exception-face "black" "Khaki" t nil nil)))
                   (t			; light colour background
                    '((prolog-warning-face "red" nil t nil nil)
                      (prolog-builtin-face "Orchid" nil nil nil nil)
                      (prolog-redo-face "darkorchid" nil nil nil nil)
                      (prolog-exit-face "ForestGreen" nil nil nil nil)
                      (prolog-exception-face "black" "Khaki" t nil nil))))))

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
                 (list (if (eq prolog-system 'mercury)
                           "[][}{;|]\\|\\\\[+=]\\|<?=>?"
                         "[][}{!;|]\\|\\*->")
                       0 'font-lock-keyword-face))
                (important-elements-1
                 '("[^-*]\\(->\\)" 1 font-lock-keyword-face))
                (predspecs		; module:predicate/cardinality
                 (list (format "\\<\\(%s:\\|\\)%s/[0-9]+"
                               prolog-atom-regexp prolog-atom-regexp)
                       0 font-lock-function-name-face 'prepend))
                (keywords               ; directives (queries)
                 (list
                  (if (eq prolog-system 'mercury)
                      (concat
                       "\\<\\("
                       (prolog-make-keywords-regexp prolog-keywords-i)
                       "\\|"
                       (prolog-make-keywords-regexp
                        prolog-determinism-specificators-i)
                       "\\)\\>")
                    (concat
                     "^[?:]- *\\("
                     (prolog-make-keywords-regexp prolog-keywords-i)
                     "\\)\\>"))
                  1 'prolog-builtin-face))

                (quoted_atom (list prolog-quoted-atom-regexp
                                   2 'font-lock-string-face 'append))
                (string (list prolog-string-regexp
                              1 'font-lock-string-face 'append))
                ;; Mercury specific patterns
                (types
                 (if (eq prolog-system 'mercury)
                     (list
                      (prolog-make-keywords-regexp prolog-types-i t)
                      0 'font-lock-type-face)))
                (modes
                 (if (eq prolog-system 'mercury)
                     (list
                      (prolog-make-keywords-regexp prolog-mode-specificators-i t)
                      0 'font-lock-reference-face)))
                (directives
                 (if (eq prolog-system 'mercury)
                     (list
                      (prolog-make-keywords-regexp prolog-directives-i t)
                      0 'prolog-warning-face)))

                ;; Inferior mode specific patterns
                (prompt
                 (list prolog-prompt-regexp-i 0 'font-lock-keyword-face))
                (trace-exit
                 (cond
                  ((eq prolog-system 'sicstus)
                   '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Exit\\):"
                     1 prolog-exit-face))
                  ((eq prolog-system 'swi)
                   '("[ \t]*\\(Exit\\):[ \t]*([ \t0-9]*)" 1 prolog-exit-face))
                  (t nil)))
                (trace-fail
                 (cond
                  ((eq prolog-system 'sicstus)
                   '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Fail\\):"
                     1 prolog-warning-face))
                  ((eq prolog-system 'swi)
                   '("[ \t]*\\(Fail\\):[ \t]*([ \t0-9]*)" 1 prolog-warning-face))
                  (t nil)))
                (trace-redo
                 (cond
                  ((eq prolog-system 'sicstus)
                   '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Redo\\):"
                     1 prolog-redo-face))
                  ((eq prolog-system 'swi)
                   '("[ \t]*\\(Redo\\):[ \t]*([ \t0-9]*)" 1 prolog-redo-face))
                  (t nil)))
                (trace-call
                 (cond
                  ((eq prolog-system 'sicstus)
                   '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Call\\):"
                     1 font-lock-function-name-face))
                  ((eq prolog-system 'swi)
                   '("[ \t]*\\(Call\\):[ \t]*([ \t0-9]*)" 1 
                     font-lock-function-name-face))
                  (t nil)))
                (trace-exception
                 (cond
                  ((eq prolog-system 'sicstus)
                   '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Exception\\):"
                     1 prolog-exception-face))
                  ((eq prolog-system 'swi)
                   '("[ \t]*\\(Exception\\):[ \t]*([ \t0-9]*)"
                     1 prolog-exception-face))
                  (t nil)))
                (error-message-identifier
                 (cond
                  ((eq prolog-system 'sicstus)
                   '("{\\([A-Z]* ?ERROR:\\)" 1 prolog-exception-face prepend))
                  ((eq prolog-system 'swi)
                   '("^[[]\\(WARNING:\\)" 1 prolog-builtin-face prepend))
                  (t nil)))
                (error-whole-messages
                 (cond
                  ((eq prolog-system 'sicstus)
                   '("{\\([A-Z]* ?ERROR:.*\\)}[ \t]*$"
                     1 font-lock-comment-face append))
                  ((eq prolog-system 'swi)
                   '("^[[]WARNING:[^]]*[]]$" 0 font-lock-comment-face append))
                  (t nil)))
                (error-warning-messages
                 ;; Mostly errors that SICStus asks the user about how to solve,
                 ;; such as "NAME CLASH:" for example.
                 (cond
                  ((eq prolog-system 'sicstus)
                   '("^[A-Z ]*[A-Z]+:" 0 prolog-warning-face))
                  (t nil)))
                (warning-messages
                 (cond
                  ((eq prolog-system 'sicstus)
                   ;; We allow one newline in the warning message
                   '("\\({ ?\\(Warning\\|WARNING\\) ?:[^\n]*\n?[^\n]*}\\)[ \t]*$" 
                     2 prolog-warning-face prepend))
                  (t nil))))

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
                keywords
                types
                modes
                directives))
              ((eq major-mode 'prolog-inferior-mode)
               (list
                prompt
                error-message-identifier
                error-whole-messages
                error-warning-messages
                warning-messages
                predspecs
                trace-exit
                trace-fail
                trace-redo
                trace-call
                trace-exception))
              ((eq major-mode 'compilation-mode)
               (list
                error-message-identifier
                error-whole-messages
                error-warning-messages
                warning-messages
                predspecs))))
            )))))


;;-------------------------------------------------------------------
;; Indentation stuff
;;-------------------------------------------------------------------

;; NB: This function *MUST* have this optional argument since XEmacs
;; assumes it. This does not mean we have to use it...
(defun prolog-indent-line (&optional whole-exp)
  "Indent current line as Prolog code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not yet)."
  (interactive "p")
  (prolog-system-override prolog-indent-line (whole-exp)
    (let ((indent (prolog-indent-level))
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
    
      ;; Align comments
      (if prolog-align-comments-flag
          (save-excursion
            (prolog-goto-comment-column t)))

      ;; Insert spaces if needed
      (if prolog-electric-tab-flag
          (prolog-insert-spaces-after-paren))
      )))

(defun prolog-comment-indent ()
  "Compute prolog comment indentation."
  (cond ((looking-at "%%%") (prolog-indentation-level-of-line))
	((looking-at "%%") (prolog-indent-level))
	(t
	 (save-excursion
	   (skip-chars-backward " \t")
	   ;; Insert one space at least, except at left margin.
	   (max (+ (current-column) (if (bolp) 0 1))
		comment-column)))
	))

(defun prolog-indent-level ()
  "Compute prolog indentation level."
  (save-excursion
    (beginning-of-line)
    (let ((totbal (prolog-region-paren-balance
		   (prolog-clause-start t) (point)))
	  (oldpoint (point)))
      (skip-chars-forward " \t")
      (cond
       ((looking-at "%%%") (prolog-indentation-level-of-line))
					;Large comment starts
       ((looking-at "%[^%]") comment-column) ;Small comment starts
       ((bobp) 0)			;Beginning of buffer

       ;;End of /* */ comment
       ((looking-at "\\*/")                  
	(save-excursion
	  (prolog-find-start-of-mline-comment)
	  (skip-chars-backward " \t")
	  (- (current-column) 2)))

       ;; Here we check if the current line is within a /* */ pair
       ((and (looking-at "[^%/]")
	     (eq (prolog-in-string-or-comment) 'cmt)) 
	(if prolog-indent-mline-comments-flag
	    (prolog-find-start-of-mline-comment)
	  ;; Same as before
	  (prolog-indentation-level-of-line)))

       (t
	(let ((empty t) ind linebal)
	  ;; See previous indentation
	  (while empty
	    (forward-line -1)
	    (beginning-of-line)
	    (if (= (point) (point-min))
		(setq empty nil)
	      (skip-chars-forward " \t")
	      (if (not (or (prolog-in-string-or-comment)
			   (looking-at "%") 
			   (looking-at "\n")))
		  (setq empty nil))))

	  ;; Store this line's indentation
	  (if (= (point) (point-min))
	      (setq ind 0)		;Beginning of buffer
	    (setq ind (current-column))) ;Beginning of clause

	  ;; Compute the balance of the line
	  (setq linebal (prolog-paren-balance))
	  ;;(message "bal of previous line %d totbal %d" linebal totbal)
	  (if (< linebal 0)
	      (progn
		;; Add 'indent-level' mode to find-unmatched-paren instead?
		(end-of-line)
		(setq ind (prolog-find-indent-of-matching-paren))))

	  ;;(message "ind %d" ind)
	  (beginning-of-line)

	  ;; Check if the line ends with ":-", "."
	  (cond
	   ;; Increase indentation if the previous line was the head of a rule
	   ;; and does not contain a '.'
	   ((and (looking-at (format ".*%s[^\\.]*[ \t]*\\(%%.*\\|\\)$" 
				     prolog-head-delimiter))
		 ;; We must check that the match is at a paren balance of 0.
		 (save-excursion
		   (let ((p (point)))
		     (re-search-forward prolog-head-delimiter)
		     (>= 0 (prolog-region-paren-balance p (point))))))
	    (let (headindent)
	      (if (< (prolog-paren-balance) 0)
		  (save-excursion
		    (end-of-line)
		    (setq headindent (prolog-find-indent-of-matching-paren)))
		(setq headindent (prolog-indentation-level-of-line)))
	      (setq ind (+ headindent prolog-indent-width))))

	   ;; If a '.' is found at the end of the previous line, then decrease
	   ;; the indentation. (The \\(%.*\\|\\) part of the regexp is for
	   ;; comments at the end of the line)
	   ((and (looking-at "^.+\\.[ \t]*\\(%.*\\|\\)$") 
		 ;; Make sure that the '.' found is not in a comment or string
		 (save-excursion
		   (end-of-line)
		   (re-search-backward "\\.[ \t]*\\(%.*\\|\\)$" (point-min))
		   (not (prolog-in-string-or-comment))))
	    (setq ind 0))

	   )

	  ;; If the last non comment char is a ',' or left paren or a left-
	  ;; indent-regexp then indent to open parenthesis level
	  (if (> totbal 0)
	      (if (looking-at
		   (format "\\(%s\\|%s\\|0'.\\|[0-9]+'[0-9a-zA-Z]+\\|[^\n\'\"%%]\\)*\\(,\\|%s\\|%s\\)\[ \t]*\\(%%.*\\|\\)$" 
			   prolog-quoted-atom-regexp prolog-string-regexp
			   prolog-left-paren prolog-left-indent-regexp))
		  (progn
		    (goto-char oldpoint)
		    (setq ind (prolog-find-unmatched-paren 'termdependent)))
		(goto-char oldpoint)
		(setq ind (prolog-find-unmatched-paren nil))
		))
	  

	  ;; Return the indentation level
	  ind
	  ))))))

(defun prolog-find-indent-of-matching-paren ()
  "Find the indentation level based on the matching parenthesis.
Indentation level is set to the one the point is after when the function is
called."
  (save-excursion
    ;; Go to the matching paren
    (if prolog-use-prolog-tokenizer-flag
	(prolog-backward-list)
      (backward-list))

    ;; If this was the first paren on the line then return this line's
    ;; indentation level
    (if (prolog-paren-is-the-first-on-line-p)
	(prolog-indentation-level-of-line)
      ;; It was not the first one
      (progn
	 ;; Find the next paren
	 (prolog-goto-next-paren 0)

	 ;; If this paren is a left one then use its column as indent level,
	 ;; if not then recurse this function
	 (if (looking-at prolog-left-paren)
	     (+ (current-column) 1)
	   (progn
	      (forward-char 1)
	      (prolog-find-indent-of-matching-paren)))
	 ))
    ))

(defun prolog-indentation-level-of-line ()
  "Return the indentation level of the current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

(defun prolog-first-pos-on-line ()
  "Return the first position on the current line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun prolog-paren-is-the-first-on-line-p ()
  "Return t if the parenthesis under the point is the first one on the line.
Return nil otherwise.
Note: does not check if the point is actually at a parenthesis!"
  (save-excursion
    (let ((begofline (prolog-first-pos-on-line)))
      (if (= begofline (point))
	  t
	(if (prolog-goto-next-paren begofline)
	    nil
	  t)))))

(defun prolog-find-unmatched-paren (&optional mode)
  "Return the column of the last unmatched left parenthesis.
If MODE is `skipwhite' then any white space after the parenthesis is added to
the answer.
If MODE is `plusone' then the parenthesis' column +1 is returned.
If MODE is `termdependent' then if the unmatched parenthesis is part of
a compound term the function will work as `skipwhite', otherwise
it will return the column paren plus the value of `prolog-paren-indent'.
If MODE is nil or not set then the parenthesis' exact column is returned."
  (save-excursion
    ;; If the next paren we find is a left one we're finished, if it's
    ;; a right one then we go back one step and recurse
    (prolog-goto-next-paren 0)

    (let ((roundparen (looking-at "(")))
      (if (looking-at prolog-left-paren)
	  (let ((not-part-of-term 
		 (save-excursion
		   (backward-char 1)
		   (looking-at "[ \t]"))))
	    (if (eq mode nil)
		(current-column)
	      (if (and roundparen
		       (eq mode 'termdependent) 
		       not-part-of-term)
		  (+ (current-column)
		     (if prolog-electric-tab-flag
			 ;; Electric TAB
			 prolog-paren-indent
		       ;; Not electric TAB
		       (if (looking-at ".[ \t]*$")
			   2
			 prolog-paren-indent))
		     )

		(forward-char 1)
		(if (or (eq mode 'skipwhite) (eq mode 'termdependent) )
		    (skip-chars-forward " \t"))
		(current-column))))
	;; Not looking at left paren
	(progn
	  (forward-char 1)
	  ;; Go to the matching paren. When we get there we have a total
	  ;; balance of 0.
	  (if prolog-use-prolog-tokenizer-flag
	      (prolog-backward-list)
	    (backward-list))
	  (prolog-find-unmatched-paren mode)))
      )))


(defun prolog-paren-balance ()
  "Return the parenthesis balance of the current line.
A return value of n means n more left parentheses than right ones."
  (save-excursion
    (end-of-line)
    (prolog-region-paren-balance (prolog-first-pos-on-line) (point))))

(defun prolog-region-paren-balance (beg end)
  "Return the summed parenthesis balance in the region.
The region is limited by BEG and END positions."
  (save-excursion
    (let ((state (if prolog-use-prolog-tokenizer-flag
		     (prolog-tokenize beg end)
		   (parse-partial-sexp beg end))))
      (nth 0 state))))

(defun prolog-goto-next-paren (limit-pos)
  "Move the point to the next parenthesis earlier in the buffer.
Return t if a match was found before LIMIT-POS.  Return nil otherwise."
  (let (retval)
    (setq retval (re-search-backward
		  (concat prolog-left-paren "\\|" prolog-right-paren)
		  limit-pos t))

    ;; If a match was found but it was in a string or comment, then recurse
    (if (and retval (prolog-in-string-or-comment))
	(prolog-goto-next-paren limit-pos)
      retval)
    ))

(defun prolog-in-string-or-comment ()
  "Check whether string, atom, or comment is under current point.
Return:
 `txt' if the point is in a string, atom, or character code expression
 `cmt' if the point is in a comment
 nil otherwise."
  (save-excursion
    (let* ((start
	    (if (eq prolog-parse-mode 'beg-of-line)
		;; 'beg-of-line
		(save-excursion
		  (let (safepoint)
		    (beginning-of-line)
		    (setq safepoint (point))
		    (while (and (> (point) (point-min))
				(progn
				  (forward-line -1)
				  (end-of-line)
				  (if (not (bobp))
				      (backward-char 1))
				  (looking-at "\\\\"))
				)
		      (beginning-of-line)
		      (setq safepoint (point)))
		    safepoint))
	      ;; 'beg-of-clause
	      (prolog-clause-start)))
	   (end (point))
	   (state (if prolog-use-prolog-tokenizer-flag
		      (prolog-tokenize start end)
		    (parse-partial-sexp start end))))
      (cond
       ((nth 3 state) 'txt) ; String
       ((nth 4 state) 'cmt) ; Comment
       (t
	(cond
	 ((looking-at "%") 'cmt) ; Start of a comment
	 ((looking-at "/\\*") 'cmt) ; Start of a comment
	 ((looking-at "\'") 'txt) ; Start of an atom
	 ((looking-at "\"") 'txt) ; Start of a string
	 (t nil)
	 ))))
    ))

(defun prolog-find-start-of-mline-comment ()
  "Return the start column of a /* */ comment.
This assumes that the point is inside a comment."
  (re-search-backward "/\\*" (point-min) t)
  (forward-char 2)
  (skip-chars-forward " \t")
  (current-column))

(defun prolog-insert-spaces-after-paren ()
  "Insert spaces after the opening parenthesis.
Spaces are inserted if the parenthesis is the first non-white character
on the line."
  (save-excursion
    (let (incr level)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (looking-at (format "(\\|%s" prolog-left-indent-regexp))
	  (progn
	    ;; Treat "( If -> " lines specially.
	    (if (looking-at "(.*->")
		(setq incr 2)
	      (setq incr prolog-paren-indent))
	    
	    ;; Goto the first char after the matched text
	    (re-search-forward (format "(\\|%s" prolog-left-indent-regexp)
			       (point-max) t)
	    (goto-char (match-end 0))
	    
	    (setq level (+ (prolog-find-unmatched-paren) incr))

	    ;; Remove old white space
	    (let ((start (point)))
	      (skip-chars-forward " \t")
	      (delete-region start (point)))
	    
	    (indent-to level)
	    ))))
  (skip-chars-forward " \t"))


;;-------------------------------------------------------------------
;; The tokenizer
;;-------------------------------------------------------------------

(defconst prolog-tokenize-searchkey
  (concat "[0-9]+'"
	  "\\|"
	  "['\"]"
	  "\\|"
	  prolog-left-paren
	  "\\|"
	  prolog-right-paren
	  "\\|"
	  "%"
	  "\\|"
	  "/\\*"
	  ))

(defun prolog-tokenize (beg end &optional stopcond)
  "Tokenize a region of prolog code between BEG and END.
STOPCOND decides the stop condition of the parsing. Valid values
are 'zerodepth which stops the parsing at the first right parenthesis
where the parenthesis depth is zero, 'skipover which skips over
the current entity (e.g. a list, a string, etc.) and nil.

The function returns a list with the following information:
 0. parenthesis depth 
 3. 'atm if END is inside an atom
    'str if END is inside a string
    'chr if END is in a character code expression (0'x)
    nil otherwise
 4. non-nil if END is inside a comment
 5. end position (always equal to END if STOPCOND is nil)
The rest of the elements are undefined."
  (save-excursion
    (let* ((end2 (1+ end))
	   oldp
	   (depth 0)
	   (quoted nil)
	   inside_cmt
	   (endpos end2)
	   skiptype ; The type of entity we'll skip over
	   )
      (goto-char beg)

      (if (and (eq stopcond 'skipover)
	       (looking-at "[^[({'\"]"))
	  (setq endpos (point))		; Stay where we are
	(while (and
		(re-search-forward prolog-tokenize-searchkey end2 t)
		(< (point) end2))
	  (progn
	    (setq oldp (point))
	    (goto-char (match-beginning 0))
	    (cond
	     ;; Atoms and strings
	     ((looking-at "'")
	      ;; Find end of atom
	      (if (re-search-forward "[^\\]'" end2 'limit)
		  ;; Found end of atom
		  (progn
		    (setq oldp end2)
		    (if (and (eq stopcond 'skipover)
			     (not skiptype))
			(setq endpos (point))
		      (setq oldp (point)))) ; Continue tokenizing
		(setq quoted 'atm)))
	   
	     ((looking-at "\"")
	      ;; Find end of string
	      (if (re-search-forward "[^\\]\"" end2 'limit)
		  ;; Found end of string
		  (progn
		    (setq oldp end2)
		    (if (and (eq stopcond 'skipover)
			     (not skiptype))
			(setq endpos (point))
		      (setq oldp (point)))) ; Continue tokenizing
		(setq quoted 'str)))

	     ;; Paren stuff
	     ((looking-at prolog-left-paren)
	      (setq depth (1+ depth))
	      (setq skiptype 'paren))

	     ((looking-at prolog-right-paren)
	      (setq depth (1- depth))
	      (if (and
		   (or (eq stopcond 'zerodepth)
		       (and (eq stopcond 'skipover) 
			    (eq skiptype 'paren)))
		   (= depth 0))
		  (progn
		    (setq endpos (1+ (point)))
		    (setq oldp end2))))

	     ;; Comment stuff
	     ((looking-at comment-start)
	      (end-of-line)
	      (if (>= (point) end)
;	      (if (>= (point) end2)
		  (progn
		    (setq inside_cmt t)
		    (setq oldp end2))
		(setq oldp (point))))

	     ((looking-at "/\\*")
	      (if (re-search-forward "\\*/" end2 'limit)
		  (setq oldp (point))
		(setq inside_cmt t)
		(setq oldp end2)))

	     ;; 0'char
	     ((looking-at "0'")
	      (setq oldp (1+ (match-end 0)))
	      (if (> oldp end) 
		  (setq quoted 'chr)))
	   
	     ;; base'number
	     ((looking-at "[0-9]+'")
	      (goto-char (match-end 0))
	      (skip-chars-forward "0-9a-zA-Z")
	      (setq oldp (point)))

	     
	     )
	    (goto-char oldp)
	    ))				; End of while
	)

      ;; Create return list
      (list depth nil nil quoted inside_cmt endpos)
      )))


;;-------------------------------------------------------------------
;; Online help
;;-------------------------------------------------------------------

(defvar prolog-help-function
  '((mercury nil)
    (eclipse prolog-help-online-ask)
    (sicstus (eval (if (boundp 'sicstus-help-function)
                       sicstus-help-function
                     'prolog-find-documentation)))
    (swi prolog-help-online-ask)
    (t prolog-help-online-ask))
  "Alist for the name of the function for finding help on a predicate.")

(defun prolog-help-on-predicate ()
  "Invoke online help on the atom under cursor."
  (interactive)
  (prolog-system-override prolog-help-on-predicate ()
    (if prolog-help-function-i
        (if (commandp prolog-help-function-i)
            (call-interactively prolog-help-function-i)
          (funcall prolog-help-function-i))
      (error "Sorry, no help method defined for this Prolog system."))))

(defun prolog-help-online-ask (&optional predicate)
  (interactive 
   (list
    (prolog-read-predicate "Help on predicate")))
  (if predicate
      (prolog-help-online predicate)))

(defun prolog-help-online (predicate)
  (prolog-ensure-process)
  (process-send-string (get-buffer-process "*prolog*") (concat "help(" predicate ").\n")) ;[PD] SPRM 10626
  (display-buffer "*prolog*"))

(defun prolog-help-apropos (string)
  "Find Prolog apropos on given STRING.
This function is only available when `prolog-system' is set to `swi'."
  (interactive "sApropos: ")
  (prolog-system-override prolog-help-apropos (string)
    (cond
     ((eq prolog-system 'swi)
      (prolog-ensure-process)
      (process-send-string "prolog" (concat "apropos(" string ").\n"))
      (display-buffer "*prolog*"))
     (t
      (error "Sorry, no Prolog apropos available for this Prolog system.")))))

(defun prolog-atom-under-point ()
  "Return the atom under or left to the point."
  (save-excursion
    (let ((nonatom_chars "[](){},\. \t\n")
	  start)
      (skip-chars-forward (concat "^" nonatom_chars))
      (skip-chars-backward nonatom_chars)
      (skip-chars-backward (concat "^" nonatom_chars))
      (setq start (point))
      (skip-chars-forward (concat "^" nonatom_chars))
      (buffer-substring-no-properties start (point))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help function with completion
;; Stolen from Per Mildner's SICStus debugger mode and modified

(defvar prolog-info-alist nil 
  "Alist with all builtin predicates.
Only for internal use by `prolog-find-documentation'")

;; SICStus specific, only used for pre SICStus 3.9 
(defun prolog-find-documentation (pred)
  "Go to the Info node for a predicate in the SICStus Info manual."
  (interactive
   (list (prolog-read-predicate
          nil
          nil
          (or prolog-info-alist
              (prolog-build-info-alist))
          )))
  (prolog-system-override prolog-find-documentation (pred)
    (if (and pred (not (equal pred "")))
        (prolog-goto-predicate-info pred))))


;; SICStus specific, only used for pre SICStus 3.9
;; Very similar to prolog-help-info except that that function cannot
;; cope with arity and that it asks the user if there are several
;; functors with different arity. This function also uses
;; prolog-info-alist for finding the info node, rather than parsing
;; the predicate index.
(defun prolog-goto-predicate-info (predicate)
  "Go to the info page for PREDICATE, which is a PredSpec."
  ;; [PM] interactive with no args does not make sense here
  ;; (interactive) 
  (prolog-system-override prolog-goto-predicate-info (predicate)
    (require 'info)
    (if (string-match "\\(.*\\)/\\([0-9]+\\).*$" predicate)
        (let ((buffer (current-buffer))
              (name (match-string 1 predicate))
              (arity (match-string 2 predicate))
              ;; [PM] unused:
              ;; oldp
              ;; (str (regexp-quote predicate))
              )
          (setq arity (string-to-number arity))
          (pop-to-buffer nil)

          (Info-goto-node 
           prolog-info-predicate-index);; We must be in the SICStus pages
          
          ;; [PM] If there are multiple entries then pick the first
          ;;      where pred appears to be defined as opposed to
          ;;      tutorial entries etc, the latter will err since
          ;;     `PREDNAME/ARITY' is not found
          ;; Was:
          ;; (Info-goto-node (car (cdr (assoc predicate prolog-info-alist))))
          ;; (prolog-find-term (regexp-quote name) arity "^`")
          (let ((nodes (cdr (assoc predicate prolog-info-alist)))
                (node nil))
            (while (and nodes (null node))
              (setq node (car nodes)
                    nodes (cdr nodes))
              (condition-case nil
                  (progn
                    ;; perhaps this could err as well if node not found
                    (Info-goto-node node)
                    ;; will err if term not found
                    ;; [PM] Note that prolog-find-term used to take a
                    ;;      regexp quoted first arg
                    ;;      SUFFIX matches _[ISO]_, _[Hook]_,
                    ;;      *[Obsolescent]* etc
                    (prolog-find-term name arity "^`" "\\( +.\\[.+\\].\\)?'$"))
                (error
                 (setq node nil))))
            (if node
                (recenter 0))
            (pop-to-buffer buffer)))
      (error "Predicate arity not specified for '%s'" predicate) ; [PM]
      )))


;; [PM] Generalized to be useful not only for SICStus
(defun prolog-read-predicate (&optional prompt default alist)
  "Read a PredSpec from the user.
Optional argument PROMPT is used to prompt, if nil then \"Help on predicate:\" is used
Optional DEFAULT is used as an initial quess, if nil then prolog-atom-under-point is used.
Optional ALIST if non-nil is used for completing read.
Returned value is a string \"FUNCTOR/ARITY\"."
  (let ((initial (or default (prolog-atom-under-point)))
        (prompt (or prompt "Help on predicate: "))
	answer)
    ;; [PM] Pass alist from caller
    ;; ;; If the predicate index is not yet built, do it now 
    ;; (if (not prolog-info-alist) 
    ;;    (prolog-build-info-alist))

    ;; Test if the initial string could be the base for completion.
    ;; Discard it if not.
    (if (and alist                      ; only if completion used
             (null (try-completion initial prolog-info-alist)))
	(setq initial ""))
    ;; Read the PredSpec from the user
    (setq answer
          (if alist
              (completing-read
               prompt
               prolog-info-alist nil t initial)
            (read-from-minibuffer
             prompt
             initial)))

    (if (equal answer "")
	initial
      answer)))

;; SICStus specific, only used for pre SICStus 3.9
(defun prolog-build-info-alist (&optional verbose)
  "Build and return prolog-info-alist. An alist of all predicates.
Each element is of the form (\"NAME/ARITY\" . (INFO-NODE1 INFO-NODE2 ...)).
Typically there is just one Info node associated with each name
If an optional argument VERBOSE is non-nil, print messages at the beginning
and end of list building."
  (prolog-system-override prolog-build-info-alist (verbose)
    (if verbose
        (message "Building info alist..."))
    (setq prolog-info-alist
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
                (Info-goto-node prolog-info-predicate-index)
                (goto-char (point-min))
                (while (re-search-forward
                        ;; "^\\* \\(.+\\)/\\([0-9]+\\)\\([^\n:*]*\\):"
                        ;; [PM] Ignore sequence number <n>  following duplicate entries
                        "^\\* \\(.+\\)/\\([0-9]+\\)\\([^\n:*<]*\\)\\( <[0-9]+>\\)?:"
                        nil t)
                  (let* ((name (match-string 1))
                         (arity (string-to-int (match-string 2)))
                         (comment (match-string 3))
                         (fa (format "%s/%d%s" name arity comment))
                         info-node)
                    (beginning-of-line)
                    ;; Extract the info node name
                    (setq info-node (progn 
                                      (re-search-forward ":[ \t]*\\([^:]+\\).$")
                                      (match-string 1)
                                      ))
                                        ;xx  Easier? (from Milan version 0.1.28)
                                        ;		  (setq info-node (Info-extract-menu-node-name))

		  
                    (if (equal fa (car last-entry))
                        (setcdr last-entry (cons info-node (cdr last-entry)))
                      (setq last-entry (cons fa (list info-node))
                            l (cons last-entry l)))))
                (setq l (nreverse l))   ; [PM] added setq l :-)
                ))))
    (if verbose
        (message "Building info alist... done.")))
  prolog-info-alist)



;;-------------------------------------------------------------------
;; Miscellaneous functions
;;-------------------------------------------------------------------

;; For Windows. Change backslash to slash. SICStus handles either
;; path separator but backslash must be doubled, therefore use slash.
(defun prolog-bsts (string)
  "Change backslashes to slashes in STRING."
  (let ((str1 (copy-sequence string))
	(len (length string))
	(i 0))
    (while (< i len)
      (if (char-equal (aref str1 i) ?\\)
	  (aset str1 i ?/))
      (setq i (1+ i)))
    str1))

(defun prolog-temporary-file ()
  "Make temporary file name for compilation."
  (make-temp-name 
   (concat 
    (or
     (getenv "TMPDIR")
     (getenv "TEMP") 
     (getenv "TMP")
     (getenv "SYSTEMP")
     "/tmp")
    "/prolcomp")))
(setq prolog-temp-filename (prolog-bsts (prolog-temporary-file)))

(defun prolog-goto-prolog-process-buffer ()
  "Switch to the prolog process buffer and go to its end."
  (switch-to-buffer-other-window "*prolog*")
  (goto-char (point-max)))

(defun prolog-debug-on (&optional arg)
  "Enable debugging.
When called with prefix argument ARG, disable debugging instead."
  (interactive "P")
  (prolog-system-override prolog-debug-on (arg)
    (if arg
        (prolog-debug-off)
      (prolog-process-insert-and-send-string prolog-debug-on-string))))

(defun prolog-debug-off ()
  "Disable debugging."
  (interactive)
  (prolog-system-override prolog-debug-off ()
    (prolog-process-insert-and-send-string prolog-debug-off-string)))

(defun prolog-trace-on (&optional arg)
  "Enable tracing.
When called with prefix argument ARG, disable tracing instead."
  (interactive "P")
  (prolog-system-override prolog-trace-on (arg)
    (if arg
        (prolog-trace-off)
      (prolog-process-insert-and-send-string prolog-trace-on-string))))

(defun prolog-trace-off ()
  "Disable tracing."
  (interactive)
  (prolog-system-override prolog-trace-off ()
    (prolog-process-insert-and-send-string prolog-trace-off-string)))

(defun prolog-zip-on (&optional arg)
  "Enable zipping.
When called with prefix argument ARG, disable zipping instead."
  (interactive "P")
  (prolog-system-override prolog-zip-on (arg)
    (if arg
        (prolog-zip-off)
      (prolog-process-insert-and-send-string prolog-zip-on-string))))

(defun prolog-zip-off ()
  "Disable zipping."
  (interactive)
  (prolog-system-override prolog-zip-off ()
    (prolog-process-insert-and-send-string prolog-zip-off-string)))

(defun prolog-create-predicate-index ()
  "Create an index for all predicates in the buffer."
  (let ((predlist '())
	clauseinfo 
	pos
	)
    (goto-char (point-min))
    ;; Replace with prolog-clause-start!
    (while (re-search-forward "^.+:-" nil t)
      (setq pos (match-beginning 0))
      (setq clauseinfo (prolog-clause-info))
      (setq predlist (append
		      predlist
		      (list (cons 
			     (format "%s/%d"
				     (nth 0 clauseinfo) 
				     (nth 1 clauseinfo))
			     pos))))
      (prolog-end-of-predicate))
    predlist))

(defun prolog-get-predspec ()
  (prolog-system-override prolog-get-predspec ()
    (save-excursion
      (let ((state (prolog-clause-info)))
        (format "%s/%d"
		(nth 0 state) 
		(nth 1 state))
      
        ))))

;; For backward compatibility. Stolen from custom.el.
(or (fboundp 'match-string)
    ;; Introduced in Emacs 19.29.
    (defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num))))))

(defun prolog-pred-start ()
  "Return the starting point of the first clause of the current predicate."
  (prolog-system-override prolog-pred-start ()
    (save-excursion
      (goto-char (prolog-clause-start))
      ;; Find first clause, unless it was a directive
      (if (and (not (looking-at "[:?]-"))
               (not (looking-at "[ \t]*[%/]"))) ; Comment
          (let* ((pinfo (prolog-clause-info))
                 (predname (nth 0 pinfo))
                 (arity (nth 1 pinfo))
                 (op (point)))
            (while (and (re-search-backward
                         (format "^%s\\([(\\.]\\| *%s\\)" 
                                 predname prolog-head-delimiter) nil t)
                        (= arity (nth 1 (prolog-clause-info))))
              (setq op (point)))
            (if (eq prolog-system 'mercury)
                ;; Skip to the beginning of declarations of the predicate
                (progn
                  (goto-char (prolog-beginning-of-clause))
                  (while (and (not (eq (point) op))
                              (looking-at
                               (format ":-[ \t]*\\(pred\\|mode\\)[ \t]+%s"
                                       predname)))
                    (setq op (point))
                    (goto-char (prolog-beginning-of-clause)))))
            op)
        (point)))))

(defun prolog-pred-end ()
  "Return the position at the end of the last clause of the current predicate."
  (prolog-system-override prolog-pred-end ()
    (save-excursion
      (goto-char (prolog-clause-end))	; if we are before the first predicate
      (goto-char (prolog-clause-start))
      (let* ((pinfo (prolog-clause-info))
             (predname (nth 0 pinfo))
             (arity (nth 1 pinfo))
             oldp
             (notdone t)
             (op (point)))
        (if (looking-at "[:?]-")
            ;; This was a directive
            (progn
              (if (and (eq prolog-system 'mercury)
                       (looking-at
                        (format ":-[ \t]*\\(pred\\|mode\\)[ \t]+\\(%s+\\)"
                                prolog-atom-regexp)))
                  ;; Skip predicate declarations
                  (progn
                    (setq predname (buffer-substring-no-properties
                                    (match-beginning 2) (match-end 2)))
                    (while (re-search-forward
                            (format
                             "\n*\\(:-[ \t]*\\(pred\\|mode\\)[ \t]+\\)?%s[( \t]"
                             predname)
                            nil t))))
              (goto-char (prolog-clause-end))
              (setq op (point)))
          ;; It was not a directive, find the last clause
          (while (and notdone
                      (re-search-forward
                       (format "^%s\\([(\\.]\\| *%s\\)" 
                               predname prolog-head-delimiter) nil t)
                      (= arity (nth 1 (prolog-clause-info))))
            (setq oldp (point))
            (setq op (prolog-clause-end))
            (if (>= oldp op)
                ;; End of clause not found.
                (setq notdone nil)
              ;; Continue while loop
              (goto-char op))))
        op))))

(defun prolog-clause-start (&optional not-allow-methods)
  "Return the position at the start of the head of the current clause.
NOTALLOWMETHODS is obsolete and for backward compatibility."
  (prolog-system-override prolog-clause-start (not-allow-methods)
    (save-excursion
      (let ((notdone t)
            (retval (point-min)))
        (end-of-line)
      
        (while (and 
		notdone 
		;; Search for a text at beginning of a line
                                        ;xx
                                        ;		(re-search-backward "^[a-z$']" nil t))
		(re-search-backward
                                        ;		 (format "^[%s$']" prolog-lower-case-string)
		 (format "^\\([%s$']\\|[:?]-\\)" prolog-lower-case-string)
		 nil t))
	  (let ((bal (prolog-paren-balance)))
	    (cond
	     ((> bal 0)
	      ;; Start of clause found
	      (progn
		(setq retval (point))
		(setq notdone nil)))
	     ((and (= bal 0)
		   (looking-at
		    (format ".*\\(\\.\\|%s\\|!,\\)[ \t]*\\(%%.*\\|\\)$" 
			    prolog-head-delimiter)))
	      ;; Start of clause found if the line ends with a '.' or
	      ;; a prolog-head-delimiter
	      (progn
		(setq retval (point))
		(setq notdone nil))
	      )
	     (t nil)                  ; Do nothing
	     )))
		
	retval))))

(defun prolog-clause-end (&optional not-allow-methods)
  "Return the position at the end of the current clause.
NOTALLOWMETHODS is obsolete and for backward compatibility."
  (prolog-system-override prolog-clause-end (not-allow-methods)
    (save-excursion
      (beginning-of-line)		; Necessary since we use "^...." for the search
      (if (re-search-forward 
           (format
	    "^\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\.[ \t]*\\(\\|%%.*\\)$"
	    prolog-quoted-atom-regexp prolog-string-regexp)
           nil t)
          (if (and (prolog-in-string-or-comment)
                   (not (eobp)))
              (progn
                (forward-char)
                (prolog-clause-end))
            (point))
        (point)))))

(defun prolog-clause-info ()
  "Return a (name arity) list for the current clause."
  (prolog-system-override prolog-clause-info ()
    (let (predname (arity 0))
      (save-excursion
        (goto-char (prolog-clause-start))
        (let ((op (point)))
          (if (looking-at prolog-atom-char-regexp)
              (progn
                (skip-chars-forward "^ (\\.")
                (setq predname (buffer-substring op (point))))
            (setq predname ""))
          ;; Retrieve the arity
          (if (looking-at prolog-left-paren)
              (let ((endp (save-excursion
                            (prolog-forward-list) (point))))
                (setq arity 1)
                (forward-char 1)        ; Skip the opening paren
                (while (progn
                         (skip-chars-forward "^[({,'\"")
                         (< (point) endp))
                  (if (looking-at ",")
                      (progn
                        (setq arity (1+ arity))
                        (forward-char 1) ; Skip the comma
                        )
                    ;; We found a string, list or something else we want
                    ;; to skip over. Always use prolog-tokenize,
                    ;; parse-partial-sexp does not have a 'skipover mode.
                    (goto-char (nth 5 (prolog-tokenize (point) endp 'skipover))))
                  )))
          ;; [PM] Consider: Error if none found (ie predname is ""), + perhaps a noerr flag
          (list predname arity)
          )))))

(defun prolog-forward-list ()
  "Move the point to the matching right parenthesis."
  (interactive)
  (prolog-system-override prolog-forward-list ()

    (if prolog-use-prolog-tokenizer-flag
        (let ((state (prolog-tokenize (point) (point-max) 'zerodepth)))
          (goto-char (nth 5 state)))
      (forward-list))))

;; NB: This could be done more efficiently!
(defun prolog-backward-list ()
  "Move the point to the matching left parenthesis."
  (interactive)
  (prolog-system-override prolog-backward-list ()
    (if prolog-use-prolog-tokenizer-flag
        (let ((bal 0)
              (paren-regexp (concat prolog-left-paren "\\|" prolog-right-paren))
              (notdone t))
          (while (and notdone (re-search-backward paren-regexp nil t))
            (cond
             ((looking-at prolog-left-paren)
              (if (not (prolog-in-string-or-comment))
                  (setq bal (1+ bal)))
              (if (= bal 0)
                  (setq notdone nil)))
             ((looking-at prolog-right-paren)
              (if (not (prolog-in-string-or-comment))
                  (setq bal (1- bal))))
             )))
      (backward-list))))

(defun prolog-beginning-of-clause ()
  "Move to the beginning of current clause.
If already at the beginning of clause, move to previous clause."
  (interactive)
  (prolog-system-override prolog-beginning-of-clause ()
    (let ((point (point))
          (new-point (prolog-clause-start)))
      (if (and (>= new-point point)
               (> point 1))
          (progn
            (goto-char (1- point))
            (goto-char (prolog-clause-start)))
        (goto-char new-point)
        (skip-chars-forward " \t")))))

; (defun prolog-previous-clause ()
;   "Move to the beginning of the previous clause."
;   (interactive)
;   (forward-char -1)
;   (prolog-beginning-of-clause))

(defun prolog-end-of-clause ()
  "Move to the end of clause.
If already at the end of clause, move to next clause."
  (interactive)
  (prolog-system-override prolog-end-of-clause ()
    (let ((point (point))
          (new-point (prolog-clause-end)))
      (if (and (<= new-point point)
               (not (eq new-point (point-max))))
          (progn
            (goto-char (1+ point))
            (goto-char (prolog-clause-end)))
        (goto-char new-point)))))

; (defun prolog-next-clause ()
;   "Move to the beginning of the next clause."
;   (interactive)
;   (prolog-end-of-clause)
;   (forward-char)
;   (prolog-end-of-clause)
;   (prolog-beginning-of-clause))

(defun prolog-beginning-of-predicate ()
  "Go to the nearest beginning of predicate before current point.
Return the final point or nil if no such a beginning was found."
  (interactive)
  (prolog-system-override prolog-beginning-of-predicate ()
    (let ((op (point))
          (pos (prolog-pred-start)))
      (if pos
          (if (= op pos)
              (if (not (bobp))
                  (progn
                    (goto-char pos)
                    (backward-char 1)
                    (setq pos (prolog-pred-start))
                    (if pos
                        (progn
                          (goto-char pos)
                          (point)))))
            (goto-char pos)
            (point))))))

(defun prolog-end-of-predicate ()
  "Go to the end of the current predicate."
  (interactive)
  (prolog-system-override prolog-end-of-predicate ()
    (let ((op (point)))
      (goto-char (prolog-pred-end))
      (if (= op (point))
          (progn
            (forward-line 1)
            (prolog-end-of-predicate))))))

(defun prolog-insert-predspec ()
  "Insert the predspec for the current predicate."
  (interactive)
  (prolog-system-override prolog-insert-predspec ()
    (let* ((pinfo (prolog-clause-info))
           (predname (nth 0 pinfo))
           (arity (nth 1 pinfo)))
      (if (and predname (not (equal predname ""))) ; [PM] Do not insert if no predspec found
          (insert (format "%s/%d" predname arity))))))

(defun prolog-view-predspec ()
  "Insert the predspec for the current predicate."
  (interactive)
  (prolog-system-override prolog-view-predspec ()
    (let* ((pinfo (prolog-clause-info))
           (predname (nth 0 pinfo))
           (arity (nth 1 pinfo)))
      (message (format "%s/%d" predname arity)))))

(defun prolog-insert-predicate-template ()
  "Insert the template for the current clause.
Returns non-nil if anything was inserted." ; [PM] added meaningful return value
  (interactive)
  (prolog-system-override prolog-insert-predicate-template ()
    (let* ((n 1)
           oldp
           (pinfo (prolog-clause-info))
           (predname (nth 0 pinfo))
           (arity (nth 1 pinfo)))
      ;; [PM] protect buffer from modification if no predname was found
      ;;      Arguably an error could be signalled instead
      (if (and predname (not (equal predname "")))
          (progn
            (insert predname)
            (if (> arity 0)
                (progn
                  (insert "(")
                  (setq oldp (point))
                  (while (< n arity)
                    (insert ",")
                    (setq n (1+ n)))
                  (insert ")")
                  (goto-char oldp)
                  ))
            )))))

(defun prolog-insert-next-clause ()
  "Insert newline and the name of the current clause."
  (interactive)
  (prolog-system-override prolog-insert-next-clause ()
    (insert "\n")
    (prolog-insert-predicate-template)))


(defun prolog-guess-module ()
  "Attempt to get the module from module directive, if present"
  (let* ((ent (or (prolog-find-value-by-system prolog-module-directive-pattern)
                  '(":-\\s-*module(\\s-*'?\\(\\w+\\)'?\\W*" . 1)))
         (pat (car ent))
         (match-no (cdr ent))
         (module                  ; [PM] Find a default
               (save-excursion
                 (save-restriction
                   (widen)
                   (goto-char (point-min))
                   (and
                    (re-search-forward pat 10000 t)
                    (buffer-substring-no-properties
                     (match-beginning match-no)
                     (match-end match-no)))))))
    (and module
         (not (equal module ""))
         module)))


(defun prolog-insert-module-modeline ()
  "Insert a modeline for module specification.
The module will default from any :- module(...) directive, if present."
  (interactive)
  (prolog-system-override prolog-insert-module-modeline ()
    ;; [PM] Make it always add the modeline in the beginning of the buffer
    (if (= (point-min) 1)               ; not if narrowing in effect
        (let* ((module (or (prolog-guess-module) "user"))
               (suffix (format "%s; -*-\n" module)))
          (goto-char (point-min))
          ;; [PM] When adding a mode line we might as well specify the
          ;; mode to use (and forever avoid getting Perl mode).
          (insert (concat "%%% -*- Mode: " 
                          (if (string-match "\\(.+\\)-mode"
                                            (symbol-name major-mode))
                              (upcase-initials (match-string 1 (symbol-name major-mode)))
                            "Prolog")
                          "; Module: " ))
          (save-excursion               ; overkill
            (insert suffix))))))


(defun prolog-comment-region (beg end)
  "Comment the region between BEG and END."
  (interactive "r")
  (comment-region beg end 3))

(defun prolog-uncomment-region (beg end)
  "Uncomment the region between BEG and END."
  (interactive "r")
  (comment-region beg end -3))

(defun prolog-goto-comment-column (&optional nocreate)
  "Move comments on the current line to the correct position.
If NOCREATE is nil (or omitted) and there is no comment on the line, then
a new comment is created."
  (interactive)
  (prolog-system-override prolog-goto-comment-column (nocreate)
    (beginning-of-line)
    (if (or (not nocreate)
            (and
             (re-search-forward 
              (format "^\\(\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\)%% *" 
                      prolog-quoted-atom-regexp prolog-string-regexp)
              (save-excursion (end-of-line) (point)) 'limit)
             (progn
               (goto-char (match-beginning 0))
               (not (eq (prolog-in-string-or-comment) 'txt)))))
        (indent-for-comment))))

(defun prolog-indent-predicate ()
  "*Indent the current predicate."
  (interactive)
  (prolog-system-override prolog-indent-predicate ()
    (indent-region (prolog-pred-start) (prolog-pred-end) nil)))

(defun prolog-indent-buffer ()
  "*Indent the entire buffer."
  (interactive)
  (prolog-system-override prolog-indent-buffer ()
    (indent-region (point-min) (point-max) nil)))

(defun prolog-mark-clause ()
  "Put mark at the end of this clause and move point to the beginning."
  (interactive)
  (prolog-system-override prolog-mark-clause ()
    (let ((pos (point)))
      (goto-char (prolog-clause-end))
      (forward-line 1)
      (beginning-of-line)
      ;; [PM] Was: (set-mark (point))
      (push-mark nil t t)               ; [PM] see mark-paragraph
      (goto-char pos)
      (goto-char (prolog-clause-start)))))

(defun prolog-mark-predicate ()
  "Put mark at the end of this predicate and move point to the beginning."
  (interactive)
  (prolog-system-override prolog-mark-predicate ()
    (let (pos)
      (goto-char (prolog-pred-end))
      (setq pos (point))
      (forward-line 1)
      (beginning-of-line)
      ;; [PM] Was: (set-mark (point))
      (push-mark nil t t)               ; [PM] see mark-paragraph
      (goto-char pos)
      (goto-char (prolog-pred-start)))))

;; Stolen from `cc-mode.el':
(defun prolog-electric-delete (arg)
  "Delete preceding character or whitespace.
If `prolog-hungry-delete-key-flag' is non-nil, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `prolog-hungry-delete-key-flag' is
nil, or point is inside a literal then the function in the variable
`backward-delete-char' is called."
  (interactive "P")
  (if (or (not prolog-hungry-delete-key-flag)
	  arg
	  (prolog-in-string-or-comment))
      (funcall 'backward-delete-char (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
	  (delete-region (point) here)
	(funcall 'backward-delete-char 1)
	))))

;; For XEmacs compatibility (suggested by Per Mildner)
(put 'prolog-electric-delete 'pending-delete 'supersede)
;; [PM] For Emacs (Library delsel)
(put 'prolog-electric-delete 'delete-selection 'supersede)

(defun prolog-electric-dot (arg)
  "Insert dot and newline or a head of a new clause.

If `prolog-electric-dot-flag' is nil, then simply insert dot.
Otherwise::
When invoked at the end of nonempty line, insert dot and newline.
When invoked at the end of an empty line, insert a recursive call to
the current predicate.
When invoked at the beginning of line, insert a head of a new clause
of the current predicate.

When called with prefix argument ARG, insert just dot."
  (interactive "P")
  (prolog-system-override prolog-electric-dot (arg)
    ;; Check for situations when the electricity should not be active
    (if (or (not prolog-electric-dot-flag)
            arg
            (prolog-in-string-or-comment)
            ;; Do not be electrix in a floating point number or an operator
            (save-excursion 
              (not 
               (re-search-backward
                                        ;xx
                                        ;	      "\\(^\\|[])}a-zA-Z_!'0-9]+\\)[ \t]*\\=" nil t)))
                (format "\\(^\\|[])}%s%s_!'0-9]+\\)[ \t]*\\=" 
                        prolog-lower-case-string
                        prolog-upper-case-string)
                nil t)))
            ;; Do not be electric if inside a parenthesis pair.
            (not (= (prolog-region-paren-balance (prolog-clause-start) (point))
                    0))
            )
        (funcall 'self-insert-command (prefix-numeric-value arg))
      (cond
       ;; Beginning of line
       ((bolp)
        (prolog-insert-predicate-template))
       ;; At an empty line with at least one whitespace
       ((and (save-excursion
               (beginning-of-line)
               (looking-at "[ \t]+$"))
             ;; [PM] made successful insertion part of condition.
             (prolog-insert-predicate-template))
        (save-excursion 
          (end-of-line)
          (insert ".\n")))
       ;; Default
       (t
        (insert ".\n"))
       ))))

(defun prolog-electric-underscore ()
  "Replace variable with an underscore.
If `prolog-electric-underscore-flag' is non-nil and the point is
on a variable then replace the variable with underscore and skip
the following comma and whitespace, if any.
If the point is not on a variable then insert underscore."
  (interactive)
  (prolog-system-override prolog-electric-underscore ()
    (if prolog-electric-underscore-flag
        (let (;; [PM] not used
              ;; start
              (oldcase case-fold-search)
              (oldp (point)))
          (setq case-fold-search nil)
                                        ;xx
                                        ;	(skip-chars-backward "a-zA-Z_")
          (skip-chars-backward
           (format "%s%s_" 
                   prolog-lower-case-string 
                   prolog-upper-case-string))

          ;; [PM] not used
          ;; (setq start (point))
          (if (and (not (prolog-in-string-or-comment))
                                        ;xx
                                        ;		 (looking-at "\\<[_A-Z][a-zA-Z_0-9]*\\>"))
                   (looking-at (format "\\<[_%s][%s%s_0-9]*\\>" 
                                       prolog-upper-case-string
                                       prolog-lower-case-string
                                       prolog-upper-case-string)))
              (progn
                (replace-match "_")
                (skip-chars-forward ", \t\n"))
            (goto-char oldp)
            (self-insert-command 1))
          (setq case-fold-search oldcase)
          )
      (self-insert-command 1))
    ))

;; [PM] added suffix arg. Changed calling convention for functor arg.
;;      If this is ever used for anything but Info entries then the
;;      arity parsing should be improved.
;;      Note: When searching for regexps in an Info node we do not
;;      have prolog syntax in effect, in particular word syntax (e.g.,
;;      \> might not be what you want for Prolog.
(defun prolog-find-term (functor arity &optional prefix suffix)
  "Go to the position at the start of the next occurance of a term.
The term is specified with FUNCTOR and ARITY. The optional arguments
PREFIX SUFFIX are the prefix and suffix of the search regexp."
  (prolog-system-override prolog-find-term (functor arity prefix)
    (let* (;; If prefix is not set then use the default "\\<"
           ;; [PM] \< et al is not a good idea if searching in a non
           ;;      prolog-mode buffer (such as Info)
           (prefix (or prefix "\\<"))
           (suffix (or suffix (if (= arity 0) "\\>" ""))) ; [PM]
           ;; [PM] regexp quote used to be done in caller but it
           ;;      should be done here where it is used to build a
           ;;      regexp
           (regexp (regexp-quote functor)))
      ;; [PM] Moved the \\> up and only if no explicit suffix.
      ;;      It never worked since it was only used in Info where
      ;;      ' is a word constituent so looking up docs for unary
      ;;      predicates did not work.
      ;; ;; Build regexp for the search if the arity is > 0
      ;; (if (= arity 0)
      ;;     ;; Add that the functor must be at the end of a word. This
      ;;     ;; does not work if the arity is > 0 since the closing )
      ;;     ;; is not a word constituent.
      ;;     (setq regexp (concat regexp "\\>"))
      (if (> arity 0)
          ;; Arity is > 0, add parens and commas
          ;; [PM] This is bogus in general since compound non-unary args
          ;;      will contain commas as well. It happens to work for the
          ;;      SICStus Info manual
          (let ((i 1))
            (setq regexp (concat regexp "("))
            (while (< i arity)
              (setq regexp (concat regexp ".+,"))
              (setq i (1+ i)))
            (setq regexp (concat regexp ".+)"))))
      
      ;; Search, and return position
      ;; [PM] wrap in prefix suffix here instead
      (if (re-search-forward (concat prefix regexp suffix) nil t)
          (goto-char (match-beginning 0))
        (error "Term not found")))))

(defun prolog-variables-to-anonymous (beg end)
  "Replace all variables within a region BEG to END by anonymous variables."
  (interactive "r")
  (prolog-system-override prolog-variables-to-anonymous (beg end)
    (save-excursion
      (let ((oldcase case-fold-search))
        (setq case-fold-search nil)
        (goto-char end)
        (while (re-search-backward "\\<[A-Z_][a-zA-Z_0-9]*\\>" beg t)
          (progn
            (replace-match "_")
            (backward-char)))
        (setq case-fold-search oldcase)
        ))))


(defun prolog-set-atom-regexps ()
  "Set the `prolog-atom-char-regexp' and `prolog-atom-regexp' variables.
Must be called after `prolog-build-case-strings'."
  (prolog-system-override prolog-set-atom-regexps ()
    (setq prolog-atom-char-regexp
          (format "[%s%s0-9_$]" 
                  prolog-lower-case-string 
                  prolog-upper-case-string))
    (setq prolog-atom-regexp
          (format "[%s$]%s*" 
                  prolog-lower-case-string 
                  prolog-atom-char-regexp))
    ))

(defvar prolog-up-chars ())
(defvar prolog-low-chars ())

;; [PM] in later XEmacsen char finally got its own type
(defalias 'prolog-int-to-char (if (fboundp 'int-to-char)
                                  'int-to-char
                                'identity))


;; [PM]: Not clear if this makes sense for MULE nor SICStus multibyte
;; chars. Update: it does _not_ make sense. See SPRM 11567 below
(defun prolog-build-case-strings ()
  "Set `prolog-upper-case-string' and `prolog-lower-case-string'.
Uses the current case-table for extracting the relevant information."
  (prolog-system-override prolog-build-case-strings ()
    (let ((prolog-up-chars '())
          (prolog-low-chars '())
          (case-table (current-case-table))) ; [PM] broke out
      ;; Use `map-char-table' if it is defined. Otherwise enumerate all
      ;; numbers between 0 and 255. `map-char-table' is probably safer.
      (if (and
           ;; [PM] 4.1 Recent emacsen (e.g. FSF Emacs 23.1.1, recent
           ;; XEmacs) has support for large character sets and has
           ;; changed the semantics of map-char-table in incompatible
           ;; ways. (SPRM 11567).

           ;;
           ;; The right fix is to ensure we use the same case mapping
           ;; as the Prolog but until then we just fall back on a
           ;; simple mapping of the characters in [0..255].
           nil ; [PM] 4.1 Do not call map-char-table anymore
           (and (fboundp 'map-char-table)
                (char-table-p case-table)))
          (map-char-table 'prolog-build-case-strings-helper case-table)
        ;; `map-char-table' was undefined. ([PM] or not a char-table)
        (let ((key 0))
          (while (< key 256)
            (prolog-build-case-strings-helper (prolog-int-to-char key))
            (setq key (1+ key))))
        )
      ;; ;; The strings are single-byte strings
      ;; (setq prolog-upper-case-string up_string)
      ;; (setq prolog-lower-case-string low_string)
      ;; string is not in emacs 19.34
      (setq prolog-upper-case-string (concat (nreverse prolog-up-chars))
            prolog-lower-case-string (concat (nreverse prolog-low-chars)))
      )))

;; [PM] broke out
;;      I do not know how this would work with MULE
(defun prolog-build-case-strings-helper (char &optional ignore)
  ;; char= is not in emacs 19.34
  (let ((case-fold-search nil))         ; make char-equal the same as char=
    (cond
     ((and (char-equal char (downcase char))
           (char-equal char (upcase char)))
      ;; Do nothing if upper and lower case are the same
      )
     ((char-equal char (downcase char))
      ;; The char is lower case
      ;; (setq low_string (format "%s%c" low_string char))
      (setq prolog-low-chars (cons char prolog-low-chars)))
     ((char-equal char (upcase char))
      ;; The char is upper case
      ;; (setq up_string (format "%s%c" up_string char))
      (setq prolog-up-chars (cons char prolog-up-chars))
      ))))


;;-------------------------------------------------------------------
;; Menu stuff (both for the editing buffer and for the inferior
;; prolog buffer)
;;-------------------------------------------------------------------

;; Menu for the prolog editing buffers
(defvar prolog-menu
;  '(list (if (eq prolog-system 'mercury) "Mercury" "Prolog")
  '(cond ((and (eq prolog-system 'sicstus) ; [PM] Hooks for new SICStus stuff
              (boundp 'sicstus-prolog-menu))
         (eval sicstus-prolog-menu))
        ;; Add others here later
        ;; ((and ...) (eval mercury-prolog-menu))
        (t
         (list
          (cond ((eq prolog-system 'eclipse) 
                 "ECLiPSe")
                ((eq prolog-system 'mercury) 
                 "Mercury")
                (t
                 "Prolog"))
	 (if (not (eq prolog-system 'mercury))
	     '("Consult"
	       ["File" prolog-consult-file t]
	       ["Buffer" prolog-consult-buffer t]
	       ["Region" prolog-consult-region t]
	       ["Predicate" prolog-consult-predicate t]))
	 (if (eq prolog-system 'sicstus)
	     '("Compile"
	       ["File" prolog-compile-file t]
	       ["Buffer" prolog-compile-buffer t]
	       ["Region" prolog-compile-region t]
	       ["Predicate" prolog-compile-predicate t]))
	 (if (eq prolog-system 'sicstus)
	     ;; In SICStus, these are pairwise disjunctive,
	     ;; so it's enough with one "off"-command
	     (list "Debugging"
		   ["Debug" prolog-debug-on t]
		   ["Trace" prolog-trace-on t]
		   (if (prolog-atleast-version '(3 . 7))
		       ["Zip" prolog-zip-on t])
		   ["None" prolog-debug-off t]))
	 (if (not (memq prolog-system '(mercury sicstus)))
	     '("Debugging"
	       ["Debug" prolog-debug-on t]
	       ["Debug off" prolog-debug-off t]
	       ["Trace" prolog-trace-on t]
	       ["Trace off" prolog-trace-off t]))
	 (if  (and (eq prolog-system 'sicstus)
		   (prolog-atleast-version '(3 . 7)))
	     '("Source level debugging"
	       ["Enable" prolog-enable-sicstus-sd t]
	       ["Disable" prolog-disable-sicstus-sd t]))
	 (if (not (eq prolog-system 'mercury))
	     ["Run Prolog" run-prolog t])
	 "-"
	 ["Comment region" prolog-comment-region t]
	 ["Uncomment region" prolog-uncomment-region t]
	 ["Insert/move to comment" indent-for-comment t]
	 "-"
	 '("Insert"
	   ["Predicate template" prolog-insert-predicate-template t]
	   ["Next clause head" prolog-insert-next-clause t]
	   ["Predicate spec" prolog-insert-predspec t]
	   ["Module modeline" prolog-insert-module-modeline t])
	 '("Move"
	   ["Beginning of clause" prolog-beginning-of-clause t]
	   ["End of clause" prolog-end-of-clause t]
	   ["Beginning of predicate" prolog-beginning-of-predicate t]
	   ["End of predicate" prolog-end-of-predicate t])
	 '("Fontify"
	   ["Buffer" font-lock-fontify-buffer t])
	 '("Indent"
	   ["Line" prolog-indent-line t]
	   ["Region" indent-region t]
	   ["Predicate" prolog-indent-predicate t]
	   ["Buffer" prolog-indent-buffer t])
	 '("Mark"
	   ["Clause" prolog-mark-clause t]
	   ["Predicate" prolog-mark-predicate t]
	   ["Paragraph" mark-paragraph t])
	 (if (not (eq prolog-system 'mercury))
	     '("Transform"
	       ["All variables in region to '_'"
		prolog-variables-to-anonymous t]))
	 (cond
	  ((eq prolog-system 'sicstus)
	   ["Help on Predicate" prolog-help-on-predicate t])
	  ((eq prolog-system 'swi)
	   '("Help"
	     ["On Predicate" prolog-help-on-predicate t]
	     ["Apropos" prolog-help-apropos t])))
	 )))
  "The definition for the menu in the editing buffers."
  )
  
;; Menu for the inferior prolog buffer
(defvar prolog-inferior-menu
  '(cond ((and (eq prolog-system 'sicstus) ; [PM] Hooks for new SICStus stuff
              (boundp 'sicstus-prolog-inferior-menu))
          (eval sicstus-prolog-inferior-menu))
        ;; Add others here later
        ;; ((and ...) (eval mercury-prolog-inferior-menu))
        (t
         (list "Prolog"
	 (if (eq prolog-system 'sicstus)
	     ;; In SICStus, these are pairwise disjunctive,
	     ;; so it's enough with one "off"-command
	     (list "Debugging"
		   ["Debug" prolog-debug-on t]
		   ["Trace" prolog-trace-on t]
		   (if (prolog-atleast-version '(3 . 7))
		       ["Zip" prolog-zip-on t])
		   ["None" prolog-debug-off t])
	   ;; Not SICStus
	   '("Debugging"
	     ["Debug" prolog-debug-on t]
	     ["Debug off" prolog-debug-off t]
	     ["Trace" prolog-trace-on t]
	     ["Trace off" prolog-trace-off t])
	   )
	 (if  (and (eq prolog-system 'sicstus)
		   (prolog-atleast-version '(3 . 7)))
	     '("Source level debugging"
	       ["Enable" prolog-enable-sicstus-sd t]
	       ["Disable" prolog-disable-sicstus-sd t]))
	 "-"
	 (cond
	  ((eq prolog-system 'sicstus)
	   ["Help on Predicate" prolog-help-on-predicate t])
	  ((eq prolog-system 'swi)
	   '("Help"
	     ["On Predicate" prolog-help-on-predicate t]
	     ["Apropos" prolog-help-apropos t])))
	 "-"
	 ["Interrupt Prolog" comint-interrupt-subjob t]
	 ["Quit Prolog" comint-quit-subjob t]
	 ["Kill Prolog" comint-kill-subjob t]
	 )))
  "The definition for the menu in the inferior prolog buffer."
  )

(defun prolog-make-menu (menu)
  "Return MENU without nil items in it and its sublists."
  (if (listp menu)
      (progn
	(setq menu (delete nil menu))
	(mapcar (function prolog-make-menu) menu))
    menu))

(defun prolog-menu ()
  "Add the menu in the editing buffer."
  (let ((menu (prolog-make-menu (eval prolog-menu))))
    (cond
     ((eq prolog-emacs 'gnuemacs)
      (easy-menu-define prolog-menu-map (current-local-map) "" menu))
     ((eq prolog-emacs 'xemacs)
      (easy-menu-add menu))
     ;; The default
     (t
      (easy-menu-define prolog-menu-map (current-local-map) "" menu))
     )))

(defun prolog-inferior-menu ()
  "Add the menu in the inferior prolog buffer."
  (let ((menu (prolog-make-menu (eval prolog-inferior-menu))))
    (cond
     ((eq prolog-emacs 'gnuemacs)
      (easy-menu-define run-prolog-menu-map (current-local-map) "" menu))
     ((eq prolog-emacs 'xemacs)
      (easy-menu-add menu))
     ;; The default
     (t
      (easy-menu-define run-prolog-menu-map (current-local-map) "" menu))
     )))

(add-hook 'prolog-mode-hook 'prolog-menu)
(add-hook 'prolog-inferior-mode-hook 'prolog-inferior-menu)

(provide 'prolog)

;;; prolog.el ends here

;; Local variables:
;; coding: latin-1
;; checkdoc-arguments-in-order-flag: nil
;; End:
