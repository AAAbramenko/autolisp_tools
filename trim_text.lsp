(defun C:TRIMTEXT (/ _model _textval _modified) 
  (vl-load-com)
  (setq _model (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
  (vlax-for obj _model 
    (if (= "AcDbText" (vla-get-ObjectName obj)) 
      (progn 
        (setq _textval  (vla-get-TextString obj)
              _modified nil
        )

        (if (wcmatch _textval " *") 
          (setq _textval  (vl-string-left-trim " " _textval)
                _modified T
          )
        )

        (if (wcmatch _textval "* ") 
          (setq _textval  (vl-string-right-trim " " _textval)
                _modified T
          )
        )

        (if _modified 
          (progn 
            (vla-put-TextString obj _textval)
            (print (strcat "Убраны пробелы: " _textval))
          )
        )
      )
    )
  )
  (princ)
) 
