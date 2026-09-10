(defun C:DS (/ _selected _symb-numb +_selection-filter+) 
  (vl-load-com)

  ;; Для выбора объектов перед выполнением команды
  (setq _pickfirst-sysvar (getvar "PICKFIRST"))
  (if (= _pickfirst-sysvar 0) 
    (setvar "PICKFIRST" 1)
  )

  ;; Получение выбранных объектов (многострочный текст, текст или блок)
  (setq +_selection-filter+ '((-4 . "<OR")
                              (0 . "TEXT")
                              (0 . "MTEXT")
                              (0 . "MULTILEADER")
                              (-4 . "<AND")
                              (0 . "INSERT")
                              (66 . 1)
                              (-4 . "AND>")
                              (-4 . "OR>")
                             )
        _selected           (ssget "_I" +_selection-filter+)
  )
  (initget 1 "Начало Конец")
  (setq _delete-pos (getkword "Место удаления [Начало/Конец]: "))
  (initget 6)
  (setq _symb-numb (getint "Количество символов для удаления: ")
        _entities  (if _selected 
                     _selected
                     (ssget +_selection-filter+)
                   )
  )

  ;; Проход по примитивам
  (_loop-over-set 
    _entities
    (lambda (entity) 
      (setq _dxf-data (entget 
                        ;; Выбор атрибута блока если первый выбранный объект блок
                        (if (= (cdr (assoc 0 (entget entity))) "INSERT") 
                          (entnext entity)
                          entity
                        )
                      )
      )

      (_delete-text-symb _dxf-data (= _delete-pos "Начало") _symb-numb)
    )
  )

  (setvar "PICKFIRST" _pickfirst-sysvar)

  (princ)
)