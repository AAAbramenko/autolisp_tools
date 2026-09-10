(defun C:GOTT (/ _counter _list-to-remove _match _removed-text-list _selection-set subcounter _text-list 
               _unique-text-list _vla-obj
              ) 
  ;; Получение текстовых объектов
  (setq _selection-set (ssget 
                         '((-4 . "<OR")
                           (0 . "TEXT")
                           (0 . "MTEXT")
                           (0 . "MULTILEADER")
                           (-4 . "OR>")
                          )
                       )
        _text-list     '()
        _counter       0
  )

  ;; Получение текстовых значений
  (if (null _selection-set) 
    (progn 
      (princ "Выберите текстовые объекты")
      (quit)
    )
  )

  ;; Получение текстовых значений
  (_loop-over-set 
    _selection-set
    (lambda (entity) 
      (setq _text-list (append _text-list 
                               (list (vla-get-TextString (vlax-ename->vla-object entity)))
                       )
      )
    )
  )

  ;; Фильтрация уникальных текстовых значений
  (setq _unique-text-list '())
  (foreach temp-text _text-list 
    (if (null (member temp-text _unique-text-list)) 
      (setq _unique-text-list (append _unique-text-list (list temp-text)))
    )
  )

  ;; Получение списка для удаления повторяющихся объектов
  (setq _match          nil
        _list-to-remove '()
  )
  (foreach unique-element _unique-text-list 
    (foreach text _text-list 
      (if (equal unique-element text) 
        (if _match 
          (progn 
            (setq _list-to-remove (append _list-to-remove (list unique-element))
                  _match          nil
            )
          )
          (setq _match T)
        )
      )
    )
    (setq _match nil)
  )

  (setq _removed-text-list _text-list)
  (foreach item _list-to-remove 
    (setq _removed-text-list (vl-remove item _removed-text-list))
  )

  ;; Сортировка
  (setq _removed-text-list (acad_strlsort _removed-text-list))

  ;; Вывод в консоль
  (if _removed-text-list 
    (progn 
      (foreach item _removed-text-list 
        (princ (strcat "\n" item))
      )
    )
    (print "Неповторяющиеся текстовые примиты отсутствуют")
  )
  (princ)
)