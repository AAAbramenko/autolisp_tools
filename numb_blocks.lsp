(defun C:NUMBB (/ _attribute-name _attributes _attributes-tags-list _numb_direction _numbering_type _index _tag_string 
                _prefix _selection _step _suffix _value _vl-entity _vl-selected-block _block-effective-name _block-set 
                _set-filter _sorted-entities _entities _entities-data
               ) 
  (vl-load-com)

  ;; Устанавливает значение для соответствующего атрибута
  (defun _set_attr_text_value (block_obj attr_tag attr_value /) 
    (foreach attr_item (vlax-safearray->list (vlax-variant-value (vla-getAttributes block_obj))) 
      (if (= (vla-get-TagString attr_item) attr_tag) 
        (vla-put-TextString attr_item attr_value) ; Установка значения атрибута
      )
    )
  )

  ;; Выбор блока
  (while 
    (or (null _selection) 
        (/= (cdr (assoc 0 (entget _selection))) "INSERT")
        (/= (cdr (assoc 66 (entget _selection))) 1)
    ) ; Проверка выбора блока с атрибутами
    (setq _selection (car (entsel "Выберите блок с атрибутами"))) ; Выбор блока, содержащего атрибуты
  )

  ;; Получение атрибутов блока
  (setq _vl-selected-block    (vlax-ename->vla-object _selection)
        _attributes           (vlax-safearray->list (vlax-variant-value (vla-getAttributes _vl-selected-block)))
        _block-effective-name (vla-get-EffectiveName _vl-selected-block)
        _attributes-tags-list '()
  )
  (foreach attribute_item _attributes 
    (setq _attributes-tags-list (_add-to-list _attributes-tags-list (vla-get-TagString attribute_item)))
  )

  ;; Список-фильтр примитивов
  (setq _set-filter (list 
                      (cons -4 "<AND")
                      (cons 0 "INSERT")
                      (cons 2 _block-effective-name)
                      (cons -4 "AND>")
                    )
  )

  ;; Запрос атрибута для нумерации
  (setq _attribute-name (_getkword-initget "Атрибут для нумерации" _attributes-tags-list))

  ;; Запрос параметров для нумерации
  (setq _prefix (getstring T "Префикс: ")
        _value  (getint "Начальное значение: ")
        _step   (getint "Шаг: ")
        _suffix (getstring T "Суффикс: ")
  )

  ;; Выбор типа нумерации
  (setq _numbering_type (_getkword-initget "Тип нумерации" '("Поочерёдная" "Групповая")))

  (if (= _numbering_type "Поочерёдная") 
    ;; Поочерёдная нумерация блоков
    (while T 
      ;; Фильтр определённого блока
      (setq _block-set (ssget "_:S:L" _set-filter))


      ;; Проверка на пустой набор
      (if (not (null _block-set)) 
        (progn 
          (setq _entity (ssname _block-set 0))

          (if (not (or (null _entity) (equal _entity _selected_entity)))  ; Пропуск автонумерации при пустом или ошибочном повторном выборе примитива
            (progn 
              (setq _entity_DXF      (entget _entity) ; DXF-данные объекта из набора
                    _selected_entity _entity ; Запоминание выбранного примитива
              )

              ;; Установка значения атрибута
              (_set_attr_text_value 
                (vlax-ename->vla-object _entity)
                _attribute-name
                (strcat _prefix (itoa _value) _suffix)
              )

              (setq _value (+ _value _step)) ; Увеличение нумерации на шаг
            )
          )
        )
      )
    )

    ;; Групповая нумерация объектов
    (progn 
      ;; Выбор направления групповой нумерации
      (setq _numb_direction (_getkword-initget 
                              "Направление групповой автонумерации"
                              '("СВЕРХУВНИЗ" "СНИЗУВВЕРХ" "СЛЕВАНАПРАВО" "СПРАВАНАЛЕВО")
                            )
      )

      ;; Создание набора объектов c фильтрацией блоков с заданным именем
      (setq _entities (ssget "_:L" _set-filter)
            _index    0
      )

      ;; Формирование списка из DXF-данных объектов
      (repeat (sslength _entities) 
        (setq _entities-data (_add-to-list _entities-data (entget (ssname _entities _index)))
              _index         (1+ _index)
        )
      )

      ;; Сортировка по координатам
      (cond 
        ( ;; Сортировка по Y
         (member _numb_direction '("СВЕРХУВНИЗ" "СНИЗУВВЕРХ"))
         (setq _sorted-entities (_sort-entities-by-y _entities-data))
        )
        ( ;; Сортировка по X
         (member _numb_direction '("СЛЕВАНАПРАВО" "СПРАВАНАЛЕВО"))
         (setq _sorted-entities (_sort-entities-by-x _entities-data))
        )
      )

      ;; Присвоение значений
      (setq _index 0)
      (repeat (sslength _entities) 
        (cond 
          ((member _numb_direction '("СЛЕВАНАПРАВО" "СНИЗУВВЕРХ"))
           (setq _entity_DXF (nth _index _sorted-entities))
          )
          ((member _numb_direction '("СВЕРХУВНИЗ" "СПРАВАНАЛЕВО"))
           (setq _entity_DXF (nth (- (1- (length _sorted-entities)) _index) _sorted-entities))
          )
        )

        ;; Установка значения атрибута
        (setq _vl-entity (vlax-ename->vla-object (cdr (assoc -1 _entity_DXF))))
        (_set_attr_text_value _vl-entity _attribute-name (strcat _prefix (itoa _value) _suffix))
        (setq _value (+ _value _step) ; Увеличение нумерации на шаг
              _index (1+ _index)
        )
      )
    )
  )

  (setq _entities nil) ; Обнуление набора примитивов
)
