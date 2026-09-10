(defun C:AS (/ +_selection-filter+ _string-pos _appended-string _selected _entities _index _pickfirst-sysvar) 

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
  (setq _string-pos      (getkword "Положение строки: [Начало/Конец]: ")
        _appended-string (getstring T "Строка для дополнения: ")
        _entities        (if _selected 
                           _selected
                           (ssget +_selection-filter+)
                         )
  )
  (_loop-over-set 
    _entities
    (lambda (entity / _dxf-data) 
      (setq _dxf-data (entget entity))
      (if (= (_get-dxf 0 _dxf-data) "INSERT") 
        (setq _dxf-data (entget (entnext entity)))
      )

      (_append-text 
        _dxf-data
        (= _string-pos "Начало")
        _appended-string
      )
    )
  )

  (setvar "PICKFIRST" _pickfirst-sysvar)

  (princ)
)