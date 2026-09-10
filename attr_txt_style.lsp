(defun C:ATTRTXTSTYLE (/ _application _active-document _attribute _block _block-ref _blocks _model-space 
                       _name-style-pair _object _selected-text-style _temp _text-style _text-style-name _text-styles 
                       _text-style-key _text-styles-keys _text-styles-keys-names _text-styles-keys-list
                      ) 

  ;; Обновляет текстовый стиль атрибутов для вхождений блоков
  (defun _apply-attrs-block-ref (block-ref text-style /) 
    (if 
      (and 
        (= (vla-get-ObjectName block-ref) "AcDbBlockReference")
        (= (vla-get-HasAttributes block-ref) :vlax-true)
      )
      (foreach attr (vlax-safearray->list (vlax-variant-value (vla-GetAttributes block-ref))) 
        (vla-put-StyleName attr text-style)
        (vla-put-ScaleFactor attr 1.0)
      )
    )
  )

  (vl-load-com)
  (setq _application            (vlax-get-acad-object)
        _active-document        (vla-get-ActiveDocument _application)
        _blocks                 (vla-get-Blocks _active-document)
        _model-space            (vla-get-ModelSpace _active-document)
        _text-styles            (vla-get-TextStyles _active-document)
        _text-styles-keys-names '()
        _text-styles-keys-list  '()
  )

  ;; Создание пар ключ-значение, где ключом является имя текстового стиля без пробелов и в верхнем регистре
  ;; для удобства обработки функцией getkword.
  (vlax-for _text-style _text-styles 
    (setq _text-style-name        (vla-get-Name _text-style)
          _text-style-key         (strcase (_string-subst "" " " _text-style-name))
          _text-styles-keys-names (_add-to-list 
                                    _text-styles-keys-names
                                    (cons _text-style-key _text-style-name)
                                  )
          _text-styles-keys-list  (_add-to-list 
                                    _text-styles-keys-list
                                    _text-style-key
                                  )
    )
  )

  ;; Выбор текстового стиля пользователем.
  (initget 1 (_str-join _text-styles-keys-list " "))
  (setq _selected-text-style (cdr 
                               (assoc 
                                 (getkword 
                                   (strcat "Выберите текстовый стиль для применения [" 
                                           (_str-join _text-styles-keys-list "/")
                                           "]: "
                                   )
                                 )
                                 _text-styles-keys-names
                               )
                             )
  )

  ;; Определения блоков (для смены стиля атрибутов блоков, вложенных в другие блоки).
  (vlax-for block _blocks 
    ; Если объект не пространство модели и содержит семейства других объектов
    (if (/= (vla-get-Name block) "*Model_Space") 
      (if (/= (vla-get-Count block) 0) 
        (vlax-for block_subitem block 
          (if (= (vla-get-ObjectName block_subitem) "AcDbAttributeDefinition") 
            (progn 
              (vla-put-StyleName block_subitem _selected-text-style)
              (vla-put-ScaleFactor block_subitem 1.0)
            )
            (_apply-attrs-block-ref block_subitem _selected-text-style)
          )
        )
      )
    )
  )

  ;; Блоки из пространства модели (вхождения)
  (vlax-for object _model-space 
    (_apply-attrs-block-ref object _selected-text-style)
  )

  ;; Регенерация чертежа.
  (command "_.REGEN")

  (princ)
)
