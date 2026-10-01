(defun C:ATTRTXTSTYLE (/ _application _active-document _block _blocks _model-space _selected-text-style _text-style 
                       _text-style-name _text-styles _dcl-id
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
  (setq _application       (vlax-get-acad-object)
        _active-document   (vla-get-ActiveDocument _application)
        _blocks            (vla-get-Blocks _active-document)
        _model-space       (vla-get-ModelSpace _active-document)
        _text-styles       (vla-get-TextStyles _active-document)
        _text-styles-names '()
  )

  ;; Список доступных текстовых стилей
  (vlax-for text-style _text-styles
    (setq _text-styles-names (_add-to-list _text-styles-names (vla-get-Name text-style)))
    )
  (setq _text-styles-names (acad_strlsort _text-styles-names))

  ;; Диалог для выбора текстового стиля
  (setq _dcl-id (load_dialog "attr_txt_style.dcl"))
  (new_dialog "attr_txt_style_dlg" _dcl-id)
  (start_list "txt_styles_list")
  (mapcar 'add_list _text-styles-names)
  (end_list)
  (action_tile "accept" 
               "(progn	(setq _selected-text-style (nth (atoi (get_tile \"txt_styles_list\")) _text-styles-names)) (done_dialog 1))"
  )
  (action_tile "cancel" "(done_dialog 0)")
  (start_dialog)
  (unload_dialog _dcl-id)

  (if _selected-text-style 
    (progn 
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
    )
  )

  (princ)
)
