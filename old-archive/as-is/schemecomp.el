;To: unix-emacs@bbn.com
;Date: 7 Feb 89 18:30:46 GMT
;From: Mike Clarkson <mailrus!jarvis.csri.toronto.edu!utgpu!utzoo!yunexus!ists!mike@bbn.com>
;Subject: C-Scheme name completion for GNU Emacs
;
;
;Loading this file gives C-Scheme command completion, so that M-TAB
;completes the word before the cursor to a Scheme command.  I usually
;have this set for any Scheme-mode window, and I define the interactive
;command scheme to set up the shell buffer ready for scheme.
;It also sets up the tags-file-name to the TAGS file in my Scheme source
;directory, so I can do M-. tags searches.
;
;You will have to change the values of tags-file-name and shell-prompt-pattern 
;to suit you site.

;;; scheme-complete.el
;; Mike Clarkson (mike@ists.ists.ca) - January 1989
;;
;; I put something like
;
;; (autoload 'process-send-ca "scheme-complete" "Send a ^C^A in shell mode" t)
;; (autoload 'process-send-cg "scheme-complete" "Send a ^C^G in shell mode" t)
;; (autoload 'scheme-complete-symbol "scheme-complete"
;; 	  "Complete a Scheme command in shell mode" t)
;; 
;; (defun scheme ()
;;   (interactive)
;;   (setq tags-file-name "/usr1/ai/scheme/mit/TAGS")
;;   (setq shell-prompt-pattern "^[0-9]+ .*%[=---]+> ")
;;   (shell)
;;   (define-key shell-mode-map "\C-c\C-a" 'process-send-ca)
;;   (define-key shell-mode-map "\C-c\C-g" 'process-send-cg)
;;   (define-key shell-mode-map "\e\C-i" 'scheme-complete-symbol)
;; )
;; 
;; (setq scheme-mode-hook
;;   '(lambda ()
;;     (define-key scheme-mode-map "\e\C-i" 'scheme-complete-symbol))
;;   )
;;  
;; in my ~/.emacs and say M-x scheme when I want to program
;
; in the shell buffer in scheme.  This gives me scheme command completion,
;;;; and the ability to send ^A or ^G in the shell mode (using ^C^A and ^C^G).
;; It also adds scheme command completion to Scheme mode.

(defun process-send-ca ()
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer)) "\C-a"))

(defun process-send-cg ()
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer)) "\C-g"))

(defun scheme-complete-symbol ()
  "Perform completion on the Scheme symbol preceding point.
That symbol is compared against the symbols that exist in the Scheme
obarray, and any additional characters determined by what is there
are inserted. All symbols with function definitions, values
or properties are considered."
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(backward-sexp 1)
		(while (= (char-syntax (following-char)) ?\')
		  (forward-char 1))
		(point)))
	 (pattern (buffer-substring beg end))
	 (completion (try-completion pattern *scheme-obarray*)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern *scheme-obarray*)))
	     (with-output-to-temp-buffer "*Help*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))


(setq *scheme-obarray*
  (mapcar 'list '(
"%cd" "%exit" "%ge" "%gst" "%in" "%out" "%pwd" "%ve" "%ve-prompt" "%vst"
"&list-to-vector" "&pair-car" "&pair-cdr" "&pair-set-car!"
"&pair-set-cdr!"  "&singleton-element" "&singleton-set-element!"
"&subvector-to-list" "&triple-first" "&triple-second" "&triple-set-first!"
"&triple-set-second!"  "&triple-set-third!"  "&triple-third"
"&typed-pair-cons" "&typed-singleton-cons" "&typed-triple-cons"
"&typed-vector-cons" "&vector-ref" "&vector-size" "&vector-to-list" "*"
"*args*" "*current-input-port*" "*current-output-port*" "*fluid-let-type*"
"*parser-radix*" "*parser-table*" "*proc*" "*rep-base-environment*"
"*rep-base-input-port*" "*rep-base-output-port*" "*rep-base-prompt*"
"*rep-base-syntax-table*" "*rep-current-environment*"
"*rep-current-input-port*" "*rep-current-output-port*"
"*rep-current-prompt*" "*rep-current-syntax-table*" "*rep-error-hook*"
"*rep-keyboard-map*" "*result*" "*the-non-printing-object*"
"*unparser-list-breadth-limit*" "*unparser-list-depth-limit*"
"*unparser-radix*" "+" "-" "-1+" "->pathname" "/" "1+" "2d-get"
"2d-get-alist-x" "2d-get-alist-y" "2d-put!"  "2d-remove!"  "<" "<=" "=" ">"
">=" "abort->nearest" "abort->previous" "abort->top-level"
"abort-to-nearest-driver" "abort-to-previous-driver"
"abort-to-top-level-driver" "abs" "access" "access-components"
"access-environment" "access-name" "access-type" "access\?"  "acos"
"add-event-receiver!"  "add-gc-daemon!"  "add-secondary-gc-daemon!"
"add-system!"  "add-to-population!"  "advice" "advice-package"
"advise-entry" "advise-exit" "and" "angle" "append!"  "append" "apply"
"ascii->char" "asin" "assignment-components"
"assignment-components-with-variable" "assignment-name" "assignment-type"
"assignment-value" "assignment-variable" "assignment\?"  "assoc"
"association-procedure" "assq" "assv" "atan" "beep" "begin"
"bit-string->signed-integer" "bit-string->unsigned-integer"
"bit-string-allocate" "bit-string-and!"  "bit-string-andc!"
"bit-string-append" "bit-string-append-reversed" "bit-string-clear!"
"bit-string-fill!"  "bit-string-length" "bit-string-move!"
"bit-string-movec!"  "bit-string-or!"  "bit-string-ref" "bit-string-set!"
"bit-string-xor!"  "bit-string-zero\?"  "bit-string=\?"  "bit-string\?"
"bit-substring" "bit-substring-find-next-set-bit"
"bit-substring-move-right!"  "bkpt" "block-declaration-text"
"block-declaration\?"  "boolean\?"  "break" "break-both" "break-entry"
"break-exit" "breakpoint" "breakpoint-procedure" "breakpoint-prompt"
"caaaar" "caaadr" "caaar" "caadar" "caaddr" "caadr" "caar" "cadaar"
"cadadr" "cadar" "caddar" "cadddr" "caddr" "cadr"
"call-with-current-continuation" "call-with-input-file"
"call-with-output-file" "canonicalize-input-filename"
"canonicalize-output-filename" "car" "case" "cdaaar" "cdaadr" "cdaar"
"cdadar" "cdaddr" "cdadr" "cdar" "cddaar" "cddadr" "cddar" "cdddar"
"cddddr" "cdddr" "cddr" "cdr" "ceiling" "cell-contents" "cell-type"
"cell\?"  "char->ascii" "char->digit" "char->integer" "char->name"
"char->string" "char-alphabetic\?"  "char-alphanumeric\?"  "char-ascii\?"
"char-bits" "char-bits-limit" "char-ci->integer" "char-ci<=\?"
"char-ci<\?"  "char-ci=\?"  "char-ci>=\?"  "char-ci>\?"  "char-code"
"char-code-limit" "char-downcase" "char-graphic\?"  "char-integer-limit"
"char-lower-case\?"  "char-numeric\?"  "char-ready\?"  "char-set"
"char-set-difference" "char-set-intersection" "char-set-invert"
"char-set-member\?"  "char-set-members" "char-set-predicate"
"char-set-union" "char-set:alphabetic" "char-set:alphanumeric"
"char-set:graphic" "char-set:lower-case" "char-set:not-whitespace"
"char-set:numeric" "char-set:standard" "char-set:upper-case"
"char-set:whitespace" "char-set\?"  "char-standard\?"  "char-upcase"
"char-upper-case\?"  "char-whitespace\?"  "char:newline" "char<=\?"
"char<\?"  "char=\?"  "char>=\?"  "char>\?"  "char\?"  "chars->ascii"
"circular-list" "close-all-open-files" "close-input-port"
"close-output-port" "code->char" "coerce-to-environment"
"combination-components" "combination-operands" "combination-operator"
"combination-size" "combination-type" "combination\?"  "comment-components"
"comment-expression" "comment-text" "comment-type" "comment\?"
"common-lisp-fluid-let!"  "compiled-code-address->block"
"compiled-code-address\?"  "compiled-code-block/environment"
"compiled-procedure-entry" "compiled-procedure-environment"
"compiled-procedure-type" "compiled-procedure\?"  "complex\?"
"compound-procedure-type" "compound-procedure\?"  "cond"
"conditional-alternative" "conditional-components" "conditional-consequent"
"conditional-predicate" "conditional-type" "conditional\?"  "cons" "cons*"
"cons-stream" "console-input-port" "console-output-port"
"continuation-annotation" "continuation-dynamic-state"
"continuation-environment" "continuation-evaluated-object-value"
"continuation-evaluated-object\?"  "continuation-expression"
"continuation-fluid-bindings" "continuation-next-continuation"
"continuation-package" "continuation-reductions" "continuation-return-code"
"continuation-type" "continuation-undefined-environment\?"
"continuation-undefined-expression\?"  "continuation\?"  "continue-rep"
"copy-file" "copy-pathname" "copy-syntax-table" "cos"
"current-dynamic-state" "current-input-port" "current-output-port"
"current-unsyntax-table" "date" "date->string" "debug" "debugger-package"
"declaration-components" "declaration-expression" "declaration-text"
"declaration-type" "declaration\?"  "declare" "deep-fluid-let!"  "define"
"define-macro" "define-syntax" "definition-components" "definition-name"
"definition-type" "definition-value" "definition\?"  "defstruct-package"
"del-assoc!"  "del-assoc" "del-assq!"  "del-assq" "del-assv!"  "del-assv"
"delay" "delay-components" "delay-expression" "delay-type" "delay\?"
"delayed-evaluation-environment" "delayed-evaluation-expression"
"delayed-evaluation-forced\?"  "delayed-evaluation-value" "delayed\?"
"delete!"  "delete" "delete-association-procedure" "delete-file"
"delete-member-procedure" "delq!"  "delq" "delv!"  "delv" "digit->char"
"disable-scan-defines!"  "disjunction-alternative" "disjunction-components"
"disjunction-predicate" "disjunction-type" "disjunction\?"  "disk-restore"
"disk-save" "display" "do" "dump-world" "dynamic-wind" "eighth"
"emacs-interface-package" "empty-stream\?"  "enable-scan-defines!"
"entry-advice" "environment-arguments" "environment-bindings"
"environment-extension-aux-list" "environment-extension-procedure"
"environment-extension\?"  "environment-has-parent\?"
"environment-package" "environment-parent" "environment-procedure"
"environment-type" "environment-warning-hook" "environment\?"  "eof-object"
"eof-object\?"  "eq\?"  "equal\?"  "eqv\?"  "error"
"error-combination-type" "error-from-compiled-code" "error-irritant"
"error-message" "error-procedure" "error-prompt" "error-system" "eval"
"even\?"  "event-distributor\?"  "event:after-restart"
"event:after-restore" "exact->inexact" "exact\?"  "except-last-pair!"
"except-last-pair" "execute-at-new-state-point" "exists-an-inhabitant\?"
"exit-advice" "exp" "expt" "extend-syntax-table" "false" "false-procedure"
"false-type" "false\?"  "fasdump" "fasload" "fifth" "file-exists\?"
"final-segment" "first" "fix:*" "fix:+" "fix:-" "fix:-1+" "fix:1+" "fix:<"
"fix:=" "fix:>" "fix:divide" "fix:gcd" "fix:negative\?"  "fix:positive\?"
"fix:quotient" "fix:remainder" "fix:zero\?"  "fixed-objects-vector-slot"
"floor" "fluid-let" "for-all-inhabitants\?"  "for-all\?"  "for-each"
"force" "format" "fourth" "full-quit" "future\?"
"garbage-collector-package" "gc-flip" "gc-history-mode" "gc-statistics"
"gc-statistics-package" "gcd" "gctime" "general-car-cdr"
"generate-uninterned-symbol" "get-fixed-objects-vector" "hash" "head"
"history-package" "home-directory-pathname" "identify-system"
"identify-world" "identity-procedure" "if" "imag-part"
"implementation-dependencies" "implemented-primitive-procedure\?"
"impurify" "in-package" "in-package-components" "in-package-environment"
"in-package-expression" "in-package-type" "in-package\?"  "inexact->exact"
"inexact\?"  "init-file-pathname" "initial-segment" "input-port-tag"
"input-port\?"  "integer->char" "integer-divide" "integer-divide-quotient"
"integer-divide-remainder" "integer-expt" "integer\?"  "interrupt-mask-all"
"interrupt-mask-gc-ok" "interrupt-mask-none" "interrupt-system"
"keyboard-interrupt-dispatch-table" "lambda" "lambda-body" "lambda-bound"
"lambda-components" "lambda-components*" "lambda-components**"
"lambda-package" "lambda-pattern/name" "lambda-pattern/optional"
"lambda-pattern/required" "lambda-pattern/rest"
"lambda-tag:common-lisp-fluid-let" "lambda-tag:deep-fluid-let"
"lambda-tag:let" "lambda-tag:make-environment"
"lambda-tag:shallow-fluid-let" "lambda-tag:unnamed" "lambda-type"
"lambda\?"  "last-pair" "lcm" "length" "let" "let*" "let-syntax" "letrec"
"lexical-assignment" "lexical-reference" "lexical-unassigned\?"
"lexical-unbound\?"  "lexical-unreferenceable\?"  "list" "list->string"
"list->vector" "list-copy" "list-deletor!"  "list-deletor" "list-ref"
"list-search-negative" "list-search-positive" "list-tail"
"list-transform-negative" "list-transform-positive" "list\?"  "load"
"load-noisily" "load-noisily\?"  "load-system!"  "local-assignment"
"local-declare" "log" "macro" "macro-spreader" "magnitude" "make-access"
"make-assignment" "make-assignment-from-variable" "make-bit-string"
"make-block-declaration" "make-cell" "make-char" "make-combination"
"make-command-loop" "make-comment" "make-conditional" "make-declaration"
"make-definition" "make-delay" "make-disjunction" "make-environment"
"make-event-distributor" "make-false" "make-in-package"
"make-initialized-vector" "make-interned-symbol"
"make-keyboard-interrupt-dispatch-table" "make-lambda" "make-lambda*"
"make-lambda**" "make-list" "make-name-generator" "make-named-tag"
"make-non-pointer-object" "make-null" "make-open-block" "make-pathname"
"make-polar" "make-population" "make-primitive-procedure" "make-quotation"
"make-rectangular" "make-rep" "make-return-address" "make-sequence"
"make-state-space" "make-string" "make-sub-type" "make-symbol"
"make-syntax-table" "make-the-environment" "make-true"
"make-type-dispatcher" "make-unassigned-object" "make-unassigned\?"
"make-unbound-object" "make-unbound\?"  "make-union-type"
"make-unsyntax-table" "make-variable" "make-vector" "map" "map*"
"map-over-population!"  "map-over-population" "map-reference-trap" "mapc"
"mapcan" "mapcan*" "mapcar" "mapcar*" "max" "max-reductions"
"max-subproblems" "measure-interval" "member" "member-procedure" "memq"
"memv" "merge-pathnames" "microcode-error" "microcode-identification-item"
"microcode-return" "microcode-system" "microcode-termination"
"microcode-termination-name" "microcode-type" "microcode-type-name"
"microcode-type-object" "microcode-type-predicate" "min" "modulo"
"name->char" "named-lambda" "negative-list-searcher"
"negative-list-transformer" "negative\?"  "newline" "non-printing-object\?"
"non-reentrant-call-with-current-continuation" "not" "null-continuation\?"
"null-procedure" "null-type" "null\?"  "number->string"
"number-of-external-primitive-procedures"
"number-of-internal-primitive-procedures" "number-of-microcode-errors"
"number-of-microcode-returns" "number-of-microcode-terminations"
"number-of-microcode-types" "number-parser-package" "number-type"
"number-unparser-package" "number\?"  "object-hash" "object-type"
"object-unhash" "odd\?"  "open-block-components" "open-block-type"
"open-block\?"  "open-input-file" "open-output-file" "or" "output-port-tag"
"output-port\?"  "pa" "package/scode-optimizer" "pair\?"  "parse-pathname"
"parser-package" "parser-table-copy" "parser-table-entry"
"pathname->absolute-pathname" "pathname->input-truename"
"pathname->output-truename" "pathname->string" "pathname-absolute\?"
"pathname-as-directory" "pathname-components" "pathname-device"
"pathname-directory" "pathname-directory-path" "pathname-directory-string"
"pathname-extract" "pathname-extract-string" "pathname-name"
"pathname-name-path" "pathname-name-string" "pathname-new-device"
"pathname-new-directory" "pathname-new-name" "pathname-new-type"
"pathname-new-version" "pathname-newest" "pathname-type" "pathname-unparse"
"pathname-unparse-name" "pathname-version" "pathname\?"  "peek-char"
"population\?"  "positive-list-searcher" "positive-list-transformer"
"positive\?"  "pp" "predicate->char-set" "primitive-datum" "primitive-io"
"primitive-procedure-arity" "primitive-procedure-name"
"primitive-procedure-type" "primitive-procedure\?"  "primitive-set-type"
"primitive-type" "primitive-type\?"  "print-gc-statistics"
"printer-history" "procedure-components" "procedure-environment"
"procedure-lambda" "procedure-package" "procedure-type" "procedure\?"
"proceed" "promise-type" "purify" "push-command-hook" "push-command-loop"
"push-rep" "quasiquote" "quit" "quotation-expression" "quotation-type"
"quotation\?"  "quote" "quotient" "random" "randomize" "rational\?"
"raw-continuation->continuation" "raw-continuation\?"  "read" "read-bits!"
"read-char" "read-char-no-hang" "read-eval-print" "read-file" "read-string"
"reader-history" "real-part" "real\?"  "reference-trap-kind"
"reference-trap-kind-name" "reference-trap\?"  "remainder"
"remove-event-receiver!"  "remove-from-population!"  "rename-file"
"rep-base-environment" "rep-base-prompt" "rep-base-syntax-table"
"rep-continuation" "rep-environment" "rep-input-port" "rep-level"
"rep-message-hook" "rep-output-port" "rep-prompt" "rep-prompt-hook"
"rep-state" "rep-syntax-table" "rep-value-hook" "replace-rep!"
"reset-keyboard-interrupt-dispatch-table!"  "return-address-code"
"return-address-name" "return-address\?"  "reverse!"  "reverse" "round"
"runtime" "runtime-system" "save-world" "scan-defines"
"scheme-pretty-printer" "scode-constant\?"  "scode-eval" "scode-quote"
"screen-clear" "second" "sequence" "sequence-actions" "sequence-components"
"sequence-type" "sequence\?"  "set!"  "set-assignment-value!"
"set-assignment-variable!"  "set-car!"  "set-cdr!"  "set-cell-contents!"
"set-comment-expression!"  "set-comment-text!"
"set-current-dynamic-state!"  "set-current-unsyntax-table!"
"set-declaration-expression!"  "set-declaration-text!"
"set-default-gc-safety-margin!"  "set-definition-name!"
"set-definition-value!"  "set-environment-extension-parent!"
"set-interrupt-enables!"  "set-keyboard-interrupt-dispatch-table!"
"set-lambda-body!"  "set-parser-table-entry!"  "set-rep-base-environment!"
"set-rep-base-prompt!"  "set-rep-base-syntax-table!"
"set-rep-environment!"  "set-rep-prompt!"  "set-rep-syntax-table!"
"set-string-length!"  "set-symbol-global-value!"
"set-working-directory-pathname!"  "seventh" "sf"
"sf/add-file-declarations!"  "sf/set-file-syntax-table!"  "sfu\?"
"shallow-fluid-let!"  "signed-integer->bit-string" "simplify-directory"
"sin" "sixth" "sort" "special-name\?"  "sqrt" "standard-rep-message"
"standard-rep-prompt" "stickify-input-filenames" "string->input-port"
"string->list" "string->number" "string->pathname" "string->symbol"
"string->uninterned-symbol" "string-allocate" "string-append"
"string-capitalize!"  "string-capitalize" "string-capitalized\?"
"string-ci<=\?"  "string-ci<\?"  "string-ci=\?"  "string-ci>=\?"
"string-ci>\?"  "string-compare" "string-compare-ci" "string-copy"
"string-downcase!"  "string-downcase" "string-fill!"
"string-find-next-char" "string-find-next-char-ci"
"string-find-next-char-in-set" "string-find-previous-char"
"string-find-previous-char-ci" "string-find-previous-char-in-set"
"string-hash" "string-length" "string-lower-case\?"
"string-match-backward" "string-match-backward-ci" "string-match-forward"
"string-match-forward-ci" "string-maximum-length" "string-null\?"
"string-output-port" "string-pad-left" "string-pad-right"
"string-prefix-ci\?"  "string-prefix\?"  "string-ref" "string-replace!"
"string-replace" "string-set!"  "string-trim" "string-trim-left"
"string-trim-right" "string-upcase!"  "string-upcase" "string-upper-case\?"
"string<=\?"  "string<\?"  "string=\?"  "string>=\?"  "string>\?"
"string\?"  "substring" "substring->list" "substring-capitalized\?"
"substring-ci<\?"  "substring-ci=\?"  "substring-downcase!"
"substring-fill!"  "substring-find-next-char" "substring-find-next-char-ci"
"substring-find-next-char-in-set" "substring-find-previous-char"
"substring-find-previous-char-ci" "substring-find-previous-char-in-set"
"substring-lower-case\?"  "substring-match-backward"
"substring-match-backward-ci" "substring-match-forward"
"substring-match-forward-ci" "substring-move-left!"
"substring-move-right!"  "substring-prefix-ci\?"  "substring-prefix\?"
"substring-replace!"  "substring-replace" "substring-upcase!"
"substring-upper-case\?"  "substring<\?"  "substring=\?"  "subvector->list"
"subvector-fill!"  "subvector-move-left!"  "subvector-move-right!"
"suspend-world" "symbol->pathname" "symbol->string" "symbol-append"
"symbol-global-value" "symbol-hash" "symbol-print-name" "symbol-type"
"symbol\?"  "syntax" "syntax*" "syntax-table-define" "syntax-table-ref"
"syntax-table-shadow" "syntax-table-undefine" "syntax-table\?"
"syntaxer-package" "system-clock" "system-global-environment"
"system-global-syntax-table" "system-hunk3-cons" "system-hunk3-cxr0"
"system-hunk3-cxr1" "system-hunk3-cxr2" "system-hunk3-set-cxr0!"
"system-hunk3-set-cxr1!"  "system-hunk3-set-cxr2!"  "system-list-to-vector"
"system-pair-car" "system-pair-cdr" "system-pair-cons"
"system-pair-set-car!"  "system-pair-set-cdr!"  "system-pair\?"
"system-state-space" "system-subvector-to-list" "system-vector-ref"
"system-vector-set!"  "system-vector-size" "system-vector\?"  "tail" "tan"
"the-empty-stream" "the-environment" "the-environment-type"
"the-environment\?"  "there-exists\?"  "third" "time" "time->string"
"timer-interrupt" "toggle-gc-notification!"  "trace" "trace-both"
"trace-entry" "trace-exit" "transcript-off" "transcript-on"
"transform-type-dispatcher" "translate-to-state-point" "true"
"true-procedure" "true-type" "truncate" "type-object-name"
"type-object-predicate" "type-object-type" "type-object\?"  "type-system"
"unadvise" "unadvise-entry" "unadvise-exit" "unassigned-object\?"
"unassigned-type" "unassigned\?"  "unassigned\?-components"
"unassigned\?-name" "unassigned\?-type" "unassigned\?\?"
"unbound-object\?"  "unbound-reference-trap\?"  "unbound-type" "unbound?"
"unbound\?-components" "unbound\?-name" "unbound\?-type" "unbound\?\?"
"unbreak" "unbreak-entry" "unbreak-exit" "undefined-conditional-branch"
"unhash" "unparse-with-brackets" "unparser-package"
"unparser-special-object-type" "unscan-defines"
"unsigned-integer->bit-string" "unsyntax" "unsyntax-lambda-list"
"unsyntax-table\?"  "unsyntaxer-package" "untrace" "untrace-entry"
"untrace-exit" "user-initial-environment" "user-initial-prompt"
"user-initial-prompt-string" "user-initial-syntax-table" "using-syntax"
"valid-hash-number\?"  "variable-components" "variable-name"
"variable-type" "variable\?"  "vector" "vector-8b-fill!"
"vector-8b-find-next-char" "vector-8b-find-next-char-ci"
"vector-8b-find-previous-char" "vector-8b-find-previous-char-ci"
"vector-8b-ref" "vector-8b-set!"  "vector->list" "vector-cons"
"vector-copy" "vector-eighth" "vector-fifth" "vector-fill!"  "vector-first"
"vector-fourth" "vector-grow" "vector-length" "vector-map" "vector-ref"
"vector-second" "vector-set!"  "vector-seventh" "vector-sixth"
"vector-third" "vector\?"  "wait-interval" "where"
"with-external-interrupts-handler" "with-history-disabled"
"with-input-from-file" "with-input-from-port" "with-input-from-string"
"with-interrupt-mask" "with-interrupts-reduced"
"with-keyboard-interrupt-dispatch-table" "with-new-history"
"with-output-to-file" "with-output-to-port" "with-output-to-string"
"with-output-to-truncated-string" "with-proceed-point"
"with-rep-continuation" "with-scan-defines-disabled"
"with-scan-defines-enabled" "with-standard-proceed-point"
"with-threaded-continuation" "with-unsyntax-table" "within-continuation"
"without-interrupts" "working-directory-package"
"working-directory-pathname" "write" "write-bits!"  "write-char"
"write-line" "write-string" "write-to-string" "zero\?"
)))
;-- 
;Mike Clarkson					mike@ists.UUCP
;Institute for Space and Terrestrial Science	mike@ists.ists.ca
;York University, North York, Ontario,		uunet!mnetor!yunexus!ists!mike
;CANADA M3J 1P3					+1 (416) 736-5611

