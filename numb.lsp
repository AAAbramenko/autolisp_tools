(defun C:NUMB (/ _numb_direction _DXF_code_text _entities _entities-data _sorted_entities _entity _entity_DXF 
               _entity_e_text _entity_type _entity_text _index _numbering_type _prefix _selected_entity _step _suffix 
               _value
              ) 

  ;; Запрос параметров для нумерации
  (setq _entities-data   nil
        _selected_entity nil
        _prefix          (getstring T "Префикс: ")
        _value           (getint "Начальное значение: ")
        _step            (getint "Шаг: ")
        _suffix          (getstring T "Суффикс: ")
  )
  (initget 1 "Поочерёдная Групповая")
  ;; Выбор типа нумерации
  (setq _numbering_type (getkword "Тип нумерации: [Поочерёдная/Групповая]: "))
  (if (= _numbering_type "Групповая") 
    (progn  ;; Выбор направления нумерации
           (initget 1 
                    "СВЕРХУВНИЗ СНИЗУВВЕРХ СЛЕВАНАПРАВО СПРАВАНАЛЕВО"
           )
           (setq _numb_direction (getkword 
                                   "Направление групповой автонумерации: [СВЕРХУВНИЗ/СНИЗУВВЕРХ/СЛЕВАНАПРАВО/СПРАВАНАЛЕВО]: "
                                 )
           )
    )
  )

  (if (= _numbering_type "Поочерёдная") 
    ;; Поочерёдная нумерация объектов
    (while T 
      ;; Выбор примитива пользователем
      (setq _entity (car (nentsel "Выберите объект. Для останова нажмите Esc.")))
      ;; Пропуск автонумерации при пустом или ошибочном повторном выборе примитива
      (if (not (or (null _entity) (equal _entity _selected_entity))) 
        (progn 
          (setq _entity_DXF      (entget _entity) ; DXF-данные объекта из набора
                _entity_type     (cdr (assoc 0 _entity_DXF)) ; Считывание типа примитива
                _selected_entity _entity ; Запоминание выбранного примитива
          )
          ;; Уточнение типа примитива
          (if 
            (member _entity_type 
                    '("ATTRIB" "TEXT" "MTEXT" "MULTILEADER")
            )
            (progn 
              (setq _DXF_code_text (if (= _entity_type "MULTILEADER") 
                                     304
                                     1
                                   )
                    _entity_e_text (assoc _DXF_code_text _entity_DXF)
                    _entity_text   (cons _DXF_code_text (strcat _prefix (itoa _value) _suffix))
                    _entity_DXF    (subst _entity_text _entity_e_text _entity_DXF) ; Обновление текста примитива
                    _value         (+ _value _step) ; Увеличение нумерации на шаг
              )
              ;; Обновление примитива
              (entmod _entity_DXF)
            )
          )
        )
      )
    )
    ;; Групповая нумерация объектов
    (progn 
      ;; Создание набора объектов c фильтрацией текста, мультивыноски либо блоков с атрибутами
      (setq _entities (ssget 
                        '((-4 . "<OR")
                          (0 . "TEXT")
                          (0 . "MTEXT")
                          (0 . "MULTILEADER")
                          (-4 . "<AND")
                          (0 . "INSERT")
                          (66 . 1)
                          (-4 . "AND>")
                          (-4 . "OR>")
                         )
                      )
            _index    0
      )

      ;; Формирование списка из DXF-данных объектов
      (repeat (sslength _entities) 
        (setq _entity_type   (cdr (assoc 0 (entget (ssname _entities _index))))
              _entities-data (append _entities-data 
                                     (list 
                                       (entget 
                                         (if (= _entity_type "INSERT") 
                                           (entnext (ssname _entities _index))
                                           ; Если объект блок, то считываются DXF-данные следующего за ним объекта, то есть атрибута
                                           (ssname _entities _index)
                                           ; Если объект текст или мультивыноска, то считываются его DXF-данные
                                         )
                                       )
                                     )
                             )
              _index         (1+ _index)
        )
      )

      ;; Сортировка по координатам
      (cond 
        ( ;; Сортировка по Y
         (member _numb_direction '("СВЕРХУВНИЗ" "СНИЗУВВЕРХ"))
         (setq _sorted_entities (_sort-entities-by-y _entities-data))
        )
        ( ;; Сортировка по X
         (member _numb_direction '("СЛЕВАНАПРАВО" "СПРАВАНАЛЕВО"))
         (setq _sorted_entities (_sort-entities-by-x _entities-data))
        )
      )

      ;; Присвоение значений
      (setq _index 0)
      (repeat (sslength _entities) 
        (cond 
          ((member _numb_direction '("СЛЕВАНАПРАВО" "СНИЗУВВЕРХ"))
           (setq _entity_DXF (nth _index _sorted_entities))
          )
          ((member _numb_direction '("СВЕРХУВНИЗ" "СПРАВАНАЛЕВО"))
           (setq _entity_DXF (nth (- (1- (length _sorted_entities)) _index) 
                                  _sorted_entities
                             )
           )
          )
        )

        (setq _DXF_code_text (if (= _entity_type "MULTILEADER") 
                               304
                               1
                             )
              _entity_e_text (assoc _DXF_code_text _entity_DXF)
              _entity_text   (cons _DXF_code_text (strcat _prefix (itoa _value) _suffix))
              _entity_DXF    (subst _entity_text _entity_e_text _entity_DXF) ; Обновление текста примитива
              _value         (+ _value _step) ; Увеличение нумерации на шаг
              _index         (1+ _index)
        )
        (entmod _entity_DXF) ; Обновление примитива
      )
    )
  )
  (setq _entities nil) ; Обнуление набора примитивов
)