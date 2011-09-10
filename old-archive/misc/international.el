; international.el 
; copyright 1993 by Michael Gschwind (mike@vlsivie.tuwien.ac.at)
; bind Alt keys for Umlaute and accents

; LCD Archive Entry:
; international|Michael Gschwind|mike@vlsivie.tuwien.ac.at|
; Bind Alt keys for Umlaute and accents.|
; 19-Jul-1993||~/misc/international.el.Z|

(require 'disp-table)
(require 'iso-insert)
(require 'iso-syntax)

(standard-display-european t)

;; deutsch
; these are letters in german, so make them a one key sequence
(global-set-key [ ?\A-a ] 'insert-a-umlaut)
(global-set-key [ ?\A-o ] 'insert-o-umlaut)
(global-set-key [ ?\A-u ] 'insert-u-umlaut)
(global-set-key [ ?\A-A ] 'insert-A-umlaut)
(global-set-key [ ?\A-O ] 'insert-O-umlaut)
(global-set-key [ ?\A-U ] 'insert-U-umlaut)
(global-set-key [ ?\A-s ] 'insert-ss)

;; allgemein
(global-set-key [ ?\A-m ] 'insert-micro-sign)
(global-set-key [ ?\A-c ] 'insert-cent-sign)

;; espanyol
;; ñ is a letter in espanyol, so make it a one key sequence
(global-set-key [ ?\A-n ] 'insert-n-tilde)
(global-set-key [ ?\A-N ] 'insert-N-tilde)
(global-set-key [ ?\A-! ] 'insert-inverted-exclamation-mark)
(global-set-key [ ?\A-? ] 'insert-inverted-question-mark)

;; français, español, português

(setq Alt-umlaut-map (make-keymap))

(define-key Alt-umlaut-map " " 'insert-diaeresis)
(define-key Alt-umlaut-map "A"  'insert-A-umlaut)
(define-key Alt-umlaut-map "E"  'insert-E-umlaut)
(define-key Alt-umlaut-map "I"  'insert-I-umlaut)
(define-key Alt-umlaut-map "O"  'insert-O-umlaut)
(define-key Alt-umlaut-map "U"  'insert-U-umlaut)
(define-key Alt-umlaut-map "a"  'insert-a-umlaut)
(define-key Alt-umlaut-map "e"  'insert-e-umlaut)
(define-key Alt-umlaut-map "i"  'insert-i-umlaut)
(define-key Alt-umlaut-map "o"  'insert-o-umlaut)
(define-key Alt-umlaut-map "u"  'insert-u-umlaut)
(define-key Alt-umlaut-map "y"  'insert-y-umlaut)

(setq Alt-aigu-map (make-keymap))

(define-key Alt-aigu-map " "   'insert-acute-accent)
(define-key Alt-aigu-map "A"   'insert-A-acute)
(define-key Alt-aigu-map "E"   'insert-E-acute)
(define-key Alt-aigu-map "I"   'insert-I-acute)
(define-key Alt-aigu-map "O"   'insert-O-acute)
(define-key Alt-aigu-map "U"   'insert-U-acute)
(define-key Alt-aigu-map "Y"   'insert-Y-acute)
(define-key Alt-aigu-map "a"   'insert-a-acute)
(define-key Alt-aigu-map "e"   'insert-e-acute)
(define-key Alt-aigu-map "i"   'insert-i-acute)
(define-key Alt-aigu-map "o"   'insert-o-acute)
(define-key Alt-aigu-map "u"   'insert-u-acute)
(define-key Alt-aigu-map "y"   'insert-y-acute)

(setq Alt-cedilla-map (make-sparse-keymap))

(define-key Alt-cedilla-map " "   'insert-cedilla)
(define-key Alt-cedilla-map "C"   'insert-C-cedilla)
(define-key Alt-cedilla-map "c"   'insert-c-cedilla)

(setq Alt-circumflex-map (make-keymap))

(define-key Alt-circumflex-map "1"   'insert-superscript-one)
(define-key Alt-circumflex-map "2"   'insert-superscript-two)
(define-key Alt-circumflex-map "3"   'insert-superscript-three)
(define-key Alt-circumflex-map "A"   'insert-A-circumflex)
(define-key Alt-circumflex-map "E"   'insert-E-circumflex)
(define-key Alt-circumflex-map "I"   'insert-I-circumflex)
(define-key Alt-circumflex-map "O"   'insert-O-circumflex)
(define-key Alt-circumflex-map "U"   'insert-U-circumflex)
(define-key Alt-circumflex-map "a"   'insert-a-circumflex)
(define-key Alt-circumflex-map "e"   'insert-e-circumflex)
(define-key Alt-circumflex-map "i"   'insert-i-circumflex)
(define-key Alt-circumflex-map "o"   'insert-o-circumflex)
(define-key Alt-circumflex-map "u"   'insert-u-circumflex)

(setq Alt-grave-map (make-sparse-keymap))

(define-key Alt-grave-map "A"   'insert-A-grave)
(define-key Alt-grave-map "E"   'insert-E-grave)
(define-key Alt-grave-map "I"   'insert-I-grave)
(define-key Alt-grave-map "O"   'insert-O-grave)
(define-key Alt-grave-map "U"   'insert-U-grave)
(define-key Alt-grave-map "a"   'insert-a-grave)
(define-key Alt-grave-map "e"   'insert-e-grave)
(define-key Alt-grave-map "i"   'insert-i-grave)
(define-key Alt-grave-map "o"   'insert-o-grave)
(define-key Alt-grave-map "u"   'insert-u-grave)

(setq Alt-tilde-map (make-sparse-keymap))

(define-key Alt-tilde-map "A"   'insert-A-tilde)
(define-key Alt-tilde-map "N"   'insert-N-tilde)
(define-key Alt-tilde-map "O"   'insert-O-tilde)
(define-key Alt-tilde-map "a"   'insert-a-tilde)
(define-key Alt-tilde-map "n"   'insert-n-tilde)
(define-key Alt-tilde-map "o"   'insert-o-tilde)
(define-key Alt-tilde-map "~"   'insert-not-sign)
(define-key Alt-tilde-map " "   'insert-not-sign)

; keys for making the various accents in french, spanish and portuguese
(define-key global-map [ ?\A-, ] Alt-cedilla-map)
(define-key global-map [ ?\A-` ] Alt-grave-map)
(define-key global-map [ ?\A-' ] Alt-aigu-map)
(define-key global-map [ ?\A-~ ] Alt-tilde-map)
(define-key global-map [ ?\A-^ ] Alt-circumflex-map)
(define-key global-map [ ?\A-" ] Alt-umlaut-map)
(define-key global-map [ ?\A-8 ] 8859-1-map) ;; faster than C-x 8

(global-set-key [ ?\A-< ] 'insert-angle-quotation-mark-left)
(global-set-key [ ?\A-> ] 'insert-angle-quotation-mark-right)
