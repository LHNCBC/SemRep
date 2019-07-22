;; .dir-locals.el for SICStus library source code
;;
;; This ensures that Emacs and SPIDER preserve our formatting conventions.
;; 

(
 (prolog-mode .
	      (
	       ;; Use tabs for indent, for legacy reasons.
	       (indent-tabs-mode . t)
	       (prolog-indent-width . 8)))
 ;; FIXME: Add c-mode variable
 )
