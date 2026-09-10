(defun C:GETTEXT (/ _text-entities _dxf-code-text _dxf-list _text-list _entity-text)
  (setq	_text-entities
		       (ssget
			 '((-4 . "<OR")
			   (0 . "TEXT")
			   (0 . "MTEXT")
			   (0 . "MULTILEADER")
			   (-4 . "OR>")
			  )
		       )
	_text-list     '()
  )

  (_loop-over-set
    _text-entities
    (lambda (entity)
      (setq _dxf-list	   (entget entity)
	    _dxf-code-text
			   (if (= (_get-dxf 0 _dxf-list) "MULTILEADER")
			     304
			     1
			   )

	    _entity-text
			   (_get-dxf _dxf-code-text _dxf-list)
      )

      (if (not (member _entity-text _text-list))
	(progn
	  (setq	_text-list
		 (_add-to-list _text-list _entity-text)
	  )
	)
      )
    )
  )


  (foreach item _text-list (print item))
  (print)
)