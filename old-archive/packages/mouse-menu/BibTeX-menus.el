;;;; BibTeX mode menus
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Mon Oct  3 11:42:40 1988 

(if (eq window-system 'x)
    (require 'x-menus)
  (require 'hci-menus))
(provide 'BibTeX-menus)

(if (eq window-system 'x)
    (defXmenu 'BibTeX-menu
	  '("BibTeX Menu"
	    ("Insert BibTeX entry" 
	     ("Article in Conference Proceedings" bibtex-InProceedings)
	     ("        Article in journal       " bibtex-Article)
	     ("               Book              " bibtex-Book)
	     ("             Booklet             " bibtex-Booklet)
	     ("         Master's Thesis         " bibtex-MastersThesis)
	     ;; Dipl\^{o}me d'etudes Approfondies (French thing sort of like an M.Sc).
	     ;;  ("            DEA Thesis           " bibtex-DEAthesis)
	     ("            Phd. Thesis          " bibtex-PhdThesis)
	     ("         Technical Report        " bibtex-TechReport)
	     ("         Technical Manual        " bibtex-Manual)
	     ("      Conference Proceedings     " bibtex-Proceedings)
	     ("        Chapter in a Book        " bibtex-InBook)
	     ("    Article in a Collection      " bibtex-InCollection)
	     ("           Miscellaneous         " bibtex-Misc)
	     ("            Unpublished          " bibtex-Unpublished)
	     ("              String             " bibtex-string))
	    ("Edit BibTeX entry" 
	     ("            next field      " bibtex-next-field)
	     ("          to end of field   " bibtex-find-it)
	     ("copy similar preceding Field" 
	      call-interactively 'bibtex-pop-previous)
	     ("copy similar following field" 
	       call-interactively 'bibtex-pop-next)
	     ("            remove OPT      " bibtex-remove-OPT)
	     ("           remove quotes    " bibtex-remove-double-quotes)
	     ("          clean up entry    " bibtex-clean-entry))
	    ("Miscellaneous"
	     ("Describe BibTeX mode" describe-mode)
	     ("Describe Key Bindings" describe-bindings)
	     ("Other Menus" x-mouse-other-menus))))

  (defHCImenu BibTeX-menu
    ("BibTeX menu")
    ("Insert BibTeX entry" . BibTeX-insert-menu)
    ("Edit BibTeX entry"   . BibTeX-edit-menu)
    ("Describe BibTeX mode"  describe-mode)
    ("Other Menus"         . other-menus-menu)
    ("Quit"   		 . emacs-quit-menu))
  
  (defHCImenu BibTeX-insert-menu
    ("Article in Conference Proceedings" bibtex-InProceedings)
    ("        Article in journal       " bibtex-Article)
    ("               Book              " bibtex-Book)
    ("             Booklet             " bibtex-Booklet)
    ("         Master's Thesis         " bibtex-MastersThesis)
    ;; Dipl\^{o}me d'etudes Approfondies (French thing sort of like an M.Sc).
    ;;  ("            DEA Thesis           " bibtex-DEAthesis)
    ("            Phd. Thesis          " bibtex-PhdThesis)
    ("         Technical Report        " bibtex-TechReport)
    ("         Technical Manual        " bibtex-Manual)
    ("      Conference Proceedings     " bibtex-Proceedings)
    ("        Chapter in a Book        " bibtex-InBook)
    ("    Article in a Collection      " bibtex-InCollection)
    ("           Miscellaneous         " bibtex-Misc)
    ("            Unpublished          " bibtex-Unpublished)
    ("              String             " bibtex-string))

  (defHCImenu BibTeX-edit-menu
    ("            next field      " bibtex-next-field)
    ("          to end of field   " bibtex-find-it)
    ("copy similar preceding field" call-interactively 'bibtex-pop-previous)
    ("copy similar following field" call-interactively 'bibtex-pop-next)
    ("            remove OPT      " bibtex-remove-OPT)
    ("           remove quotes    " bibtex-remove-double-quotes)
    ("          clean up entry    " bibtex-clean-entry))
  )
