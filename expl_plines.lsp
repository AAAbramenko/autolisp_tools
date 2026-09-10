(defun C:EXPLPLINES (/ _model _counter _coordinates _selection-set) 
  (vl-load-com)

  ;; Получение полилиний из выделенного набора
  (setq _selection-set (ssget 
                         '((-4 . "<OR")
                           (0 . "POLYLINE")
                           (0 . "LWPOLYLINE")
                           (-4 . "OR>")
                          )
                       )
        _count         0
  )

  (_loop-over-set 
    _selection-set
    (lambda (entity) 
      (setq obj (vlax-ename->vla-object entity))

      (if (= "AcDbPolyline" (vla-get-ObjectName obj)) 
        (progn 
          (setq _coordinates (vlax-safearray->list 
                               (vlax-variant-value (vla-get-Coordinates obj))
                             )
          )
          (if (= (vl-list-length _coordinates) 4)  ; Число вершин - 2
            (progn 
              (vla-Explode obj)
              (vla-Delete obj)
              (setq _count (1+ _count))
            )
          )
        )
      )
    )
  )

  (print (strcat "Преобразовано " (itoa _count) " полилиний"))
  (princ)
)