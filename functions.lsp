;;; Вспомогательные функции

;;; Функция получения минимального элемента в списке
(defun _get-min-element (X) (car (vl-sort X '<)))

;;; Функция получения максимального элемента в списке
(defun _get-max-element (X) (car (vl-sort X '>)))

;;; Создаёт слой, если он не существует.
;;; Если слой существует, устанавливает его как текущий.
(defun _add-layer (layer-name)
  (if (not (tblsearch "LAYER" layer-name))
    (command "._layer" "_M" layer-name "")
  )
  (setvar "CLAYER" layer-name)
)

;;; Добавляет элемент в список (на нулевую позицию).
(defun _add-to-list (lst element)
  (append lst (list element))
)

;;; Заменяет вхождение в строке.
(defun _string-subst (new old string / _pos)
  (setq _pos -1)
  (while (/= _pos nil)
    (setq string (vl-string-subst new old string))
    (setq _pos (vl-string-search old string))
  )
  string
)

;;; Объединяет список строк с указанным разделителем в одну строку
(defun _str-join (strings separator / _len _str _index)
  (setq	_len   (vl-list-length strings)
	_str   (car strings)
	_index 1
  )

  (repeat (- _len 1)
    (setq _str	 (strcat _str separator (nth _index strings))
	  _index (1+ _index)
    )
  )
  _str
)

;;; Применяет текстовый стиль к атрибутам блока, если объект является блоком с атрибутами
(defun _apply-text-style-to-block (object text_style)
  (if
					;Если объект определение или вхождение блока
    (or
      (= (vla-get-ObjectName object) "AcDbBlockReference")
      (= (vla-get-ObjectName object) "AcDbBlockTableRecord")
    )
     (vlax-for sub_object object
					; Если объект, входящий в состав блока является атрибутом, то применяется стиль к атрибутам
       (if (= (vla-get-Name sub_object) "AcDbAttributeDefinition")
	 (progn
	   (vla-put-StyleName sub_object text_style)
	   (vla-put-ScaleFactor block_subitem 1.0)
	 )
					; Повторный вызов функции для обработки вложенных объектов
	 (_apply-text-style-to-block sub_object text_style)
       )
     )
  )
)

;;; Получение DXF-данных по коду из списка с DXF-данными
(defun _get-dxf	(dxf_code dxf_list)
  (cdr (assoc dxf_code dxf_list))
)

;;; Добавляет текст к строке объекта TEXT/MTEXT/MULTILEADER/ATTDEF
;;; pos=T - добавляет text-val  в начало текста
;;; pos=nil - добавляет text-val в конец текста
(defun _append-text (dxf-data pos text-val / _dxf-text _curr-txt-val _new-txt-val _new-dxf-text	_txt-dxf-code
		     _entity-type)
  (setq	_entity-type  (_get-dxf 0 dxf-data)
	_txt-dxf-code (cond
			((or (= _entity-type "TEXT")
			     (= _entity-type "MTEXT")
			     (= _entity-type "ATTRIB")
			 )
			 1
			)
			((= _entity-type "MULTILEADER")
			 304
			)
		      )
  )

  (if _txt-dxf-code

    (progn

      (setq _dxf-text	  (assoc _txt-dxf-code dxf-data)
	    _curr-txt-val (cdr _dxf-text)
	    _new-txt-val  (if pos
			    (strcat text-val _curr-txt-val)
			    (strcat _curr-txt-val text-val)
			  )
	    _new-dxf-text (cons _txt-dxf-code _new-txt-val)
	    ;; Обновление текста объекта
	    _dxf-data	  (subst _new-dxf-text _dxf-text _dxf-data)
      )

      (entmod _dxf-data)
    )
  )
)

;;; Удаляет символы в начале или в конце текстового примитива
;;; _pos=T - добавляет text-val  в начало текста
;;; _pos=nil - добавляет text-val в конец текста
(defun _delete-text-symb (dxf-data	  pos		  symb-numb	  /		  _dxf-text
			  _curr-txt-val	  _new-txt-val	  _new-dxf-text	  _curr-txt-len	  _entity-type
			  _txt-dxf-code
			 )
  (setq	_entity-type  (_get-dxf 0 dxf-data)
	_txt-dxf-code (cond
			((or (= _entity-type "TEXT")
			     (= _entity-type "MTEXT")
			     (= _entity-type "ATTRIB")
			 )
			 1
			)
			((= _entity-type "MULTILEADER")
			 304
			)
		      )
  )

  (setq	_dxf-text     (assoc _txt-dxf-code dxf-data)
	_curr-txt-val (cdr _dxf-text)
	_curr-txt-len (strlen _curr-txt-val)
  )

  (if
    ;; Если количество удаляемых символов больше, чем длина строки, то текстовый примтитив пропускается
    (< symb-numb
       _curr-txt-len
    )
     (progn
       (setq _new-txt-val
	      (if pos
		(substr _curr-txt-val (+ symb-numb 1) _curr-txt-len)
		(substr _curr-txt-val 1 (- _curr-txt-len symb-numb))
	      )
       )

       (setq _new-dxf-text (cons _txt-dxf-code _new-txt-val)
	     ;; Обновление текста объекта
	     _dxf-data	   (subst _new-dxf-text _dxf-text _dxf-data)
       )

       (entmod _dxf-data)
     )
  )
)


;;; Обновление точки вставки примитива
(defun _upd-ins-point (dxf_data	ins_point / _new_dxf _curr_ins_point _new_ins_point)
  (setq	_dxf_point_code	(car (_get-text-point dxf_data))
	_curr_ins_point	(assoc _dxf_point_code dxf_data)
	_new_ins_point	(cons _dxf_point_code ins_point)
	dxf_data	(subst _new_ins_point _curr_ins_point dxf_data)
  )
  (entmod dxf_data)
)

;;; Возвращает точку вставки для примитива TEXT, поскольку точка зависит от ориентации текста
;;; Возвращается точечная пара, где левый элемент - DXF-код точки, правый - значение точки
(defun _get-text-point (dxf_data / _dxf_point_code _curr_ins_point)
  (if (= (_get-dxf 0 dxf_data) "MTEXT")
    (assoc 10 dxf_data)
    (progn
      (setq				; Выбор DXF-кода точки вставки в зависимости от выравнивания текста
	_dxf_point_code
	 (if
	   (and	(= (_get-dxf 72 dxf_data) 0)
		(= (_get-dxf 73 dxf_data) 0)
	   )
	    10
	    11
	 )
      )
      (assoc _dxf_point_code dxf_data)
    )
  )
)

;;; Обновляет текстовый стиль атрибутов для вхождений блоков
(defun _apply-attrs-block-ref (block-ref text-style)
  (if
    (and
      (= (vla-get-ObjectName block-ref) "AcDbBlockReference")
      (= (vla-get-HasAttributes block-ref) :vlax-true)
    )
     (foreach attr
		   (vlax-safearray->list
		     (vlax-variant-value (vla-GetAttributes block-ref))
		   )
       (vla-put-StyleName attr text-style)
       (vla-put-ScaleFactor attr 1.0)
     )
  )
)

;;; Возвращает для vla-объекта точку вставки в виде списка
(defun _get-insert-point-as-list (obj)
  (vlax-safearray->list
    (vlax-variant-value (vla-get-InsertionPoint obj))
  )
)

;;; Функция получения точек прямоугольника, описанного вокруг динамического блока
;;; Основа функции взята здесь http://forum.dwg.ru/showpost.php?p=480876&postcount=120
(defun _get_bounding_box_dynblock (vla-obj	  document	 /		_block_items_list
				   _points_list	  _insertion_point		_min_point     _max_point
				   _mins_list	  _maxs_list	 _mins_x	_mins_y	       _min_x
				   _min_y	  _max_x	 _max_y
				  )
  (vlax-for item
		 (vla-item
		   (vla-get-Blocks document)
		   (vla-get-Name vla-obj)
		 )
    (if	(equal (vla-get-Visible item) :vlax-true)
      (setq _block_items_list (cons item _block_items_list)) ; List of items
    )
  )

  (setq	_insertion_point (_get-insert-point-as-list vla-obj)
	_points_list	 (vl-remove
			   nil
			   (mapcar
			     '(lambda (item / _min_point _max_point)
				(if
				  (not
				    (vl-catch-all-error-p
				      (vl-catch-all-apply
					'(lambda ()
					   (vla-getBoundingBox item '_min_point '_max_point)
					 )
				      )
				    )
				  )
				   (list
				     (cons "min" (vlax-safearray->list _min_point))
				     (cons "max" (vlax-safearray->list _max_point))
				   )
				)
			      )
			     _block_items_list
			   )
			 )
	_mins_list	 '()
	_maxs_list	 '()
  )

  (foreach item_points _points_list
    (setq _mins_list (append _mins_list (list (cdr (assoc "min" item_points))))
	  _maxs_list (append _maxs_list (list (cdr (assoc "max" item_points))))
    )
  )

  (setq	_mins_x	'()
	_mins_y	'()
	_maxs_x	'()
	_maxs_y	'()
  )

  (foreach min_point _mins_list
    (setq _mins_x (append _mins_x (list (car min_point))))
    (setq _mins_y (append _mins_y (list (cadr min_point))))
  )

  (foreach max_point _maxs_list
    (setq _maxs_x (append _maxs_x (list (car max_point))))
    (setq _maxs_y (append _maxs_y (list (cadr max_point))))
  )

  (setq	_min_x	 (apply 'min _mins_x)
	_min_y	 (apply 'min _mins_y)
	_max_x	 (apply 'max _maxs_x)
	_max_y	 (apply 'max _maxs_y)
	_x_scale (vla-get-XEffectiveScaleFactor vla-obj)
	_y_scale (vla-get-YEffectiveScaleFactor vla-obj)
	_min_x	 (* _min_x _x_scale)
	_min_y	 (* _min_y _y_scale)
	_max_x	 (* _max_x _x_scale)
	_max_y	 (* _max_y _y_scale)
  )

  (setq	_ins_x (car _insertion_point)
	_ins_y (cadr _insertion_point)
	_ins_z (caddr _insertion_point)
  )

  (list
    (list				;min
      (+ _ins_x _min_x)
      (+ _ins_y _min_y)
      _ins_z
    )
    (list				;max
      (+ _ins_x _max_x)
      (+ _ins_y _max_y)
      _ins_z
    )
  )
)

;;; Выполняет проход по набору примитивов ent-set, полученному функцией ssget
;;; Вызывает аргумент callback для каждого примитива из набора ent-set
(defun _loop-over-set (ent-set callback / _len _index)
  (if ent-set
    (progn
      (setq _len   (sslength ent-set)
	    _index 0
      )
      (repeat _len
	(callback (ssname ent-set _index))
	(setq _index (1+ _index))
      )
    )
  )
)

;;; Выводит список
;;; TODO Проверить обработку точечных пар
(defun _print-list (lst / _print-list-indent)
  (defun _print-list-indent (lst indent)
    (foreach item lst
      (if (listp item)
	(_print-list-indent item (strcat "  " indent))
	(progn
	  (print)
	  (princ indent)
	  (princ item)
	)
      )
    )
  )

  (if (listp lst)
    (_print-list-indent lst "")
    (print lst)
  )
  (princ)
)

;;; Сортировка списка DXF-данных по X-координате в порядке возрастания
(defun _sort-entities-by-x (entities-data)
  (vl-sort entities-data
	   (function
	     (lambda (entity1 entity2)
	       (< (cadr (assoc 10 entity1)) (cadr (assoc 10 entity2)))
	     )
	   )
  )
)

;;; Сортировка списка DXF-данных по Y-координате в порядке возрастания
(defun _sort-entities-by-y (entities-data)
  (vl-sort entities-data
	   (function
	     (lambda (entity1 entity2)
	       (< (caddr (assoc 10 entity1)) (caddr (assoc 10 entity2)))
	     )
	   )
  )
)