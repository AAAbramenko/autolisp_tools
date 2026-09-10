;;; Выравнивание текста/мтекста вдоль линии и размещение через одинаковое расстояние
(defun C:ARRTXT (/ _entities _direction_str _is_vertical_dir _distance _enitites_count _entity _entity_index 
                 _entities_list _insert_point _axis_coord _upd_dxf_data _dxf_point_code
                ) 

  ;; Набор примитивов для упорядочивания, фильтрация текста
  (setq _entities (ssget '((-4 . "<OR") (0 . "TEXT") (0 . "MTEXT") (-4 . "OR>"))))

  ;; Направление упорядочивания
  (initget 1 "Вертикально Горизонтально")
  (setq _direction_str   (getkword "Направление выравнивания? [Вертикально/Горизонтально]: ")
        _is_vertical_dir (= _direction_str "Вертикально")
  )

  ;; Стартовая точка откуда вставлять
  (initget 9)
  (setq _start_ins_point (getpoint "Укажите стартовую точку"))

  ;; Расстояние между примитивами
  (initget 7)
  (setq _distance       (getreal "Расстояние между примитивами:")
        _enitites_count (sslength _entities)
        _entity_index   0
        _entities_list  '()
  )

  ;;  Преобразование набора примитивов в список со списками DXF-данных примитивов
  (repeat _enitites_count 
    (setq _entity        (ssname _entities _entity_index)
          _entities_list (append _entities_list (list (entget _entity)))
          _entity_index  (1+ _entity_index)
    )
  )

  ;;  Сортировка примитивов в списке по координатам
  (setq _entities_list (vl-sort 
                         _entities_list
                         (function 
                           (lambda (ent1 ent2) 
                             (progn 
                               (setq _ins_point_1 (cdr (_get-text-point ent1))
                                     _ins_point_2 (cdr (_get-text-point ent2))
                               )
                               (if _is_vertical_dir 
                                 (> (cadr _ins_point_1) (cadr _ins_point_2))
                                 (< (car _ins_point_1) (car _ins_point_2))
                               )
                             )
                           )
                         )
                       )
  )

  (setq _insert_point _start_ins_point
        _axis_coord   (if _is_vertical_dir (cadr _start_ins_point) (car _start_ins_point))
  )
  (foreach _dxf_data _entities_list 
    (progn 
      (_upd-ins-point _dxf_data _insert_point)
      (if _is_vertical_dir 
        ;;  Вертикальное упорядочивание
        (setq _axis_coord   (- _axis_coord _distance)
              _insert_point (LM:SubstNth _axis_coord 1 _insert_point)
        )
        ;;  Горизонтальное упорядочивание
        (setq _axis_coord   (+ _axis_coord _distance)
              _insert_point (LM:SubstNth _axis_coord 0 _insert_point)
        )
      )
    )
  )
)
 
;;; https://lee-mac.com/substn.html
(defun LM:SubstNth (new_item pos lst / idx) 
  (setq idx -1)
  (mapcar '(lambda (x) (if (= (setq idx (1+ idx)) pos) new_item x)) lst)
)