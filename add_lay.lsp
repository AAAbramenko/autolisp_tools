(defun C:ADDLAY (/ _layout _layouts _frame-index _frames-points _object _paper-space _user-layer _active-document 
                 _application _first-layout-index _new-layouts-names _plot-configs _sysvar-layout-create-viewport 
                 _frames-groups _has-match _user-tab _old-error
                ) 
  (vl-load-com)

  (setq _old-error *error*)

  ;; Состояния системных переменных
  (setq _user-tab   (getvar "CTAB")
        _user-layer (getvar "CLAYER")
  )

  ;; Переменная показывает доступность системной переменной LAYOUTCREATEVIEWPORT и поддержку метода vla-get-LayoutCreateViewport
  ;; nil - переменная недоступна, программа запущена в nanoCAD
  ;; По состоянию на 2025 в nanoCAD отсутствует поддержка системной переменнной LAYOUTCREATEVIEWPORT и данного метода,
  ;; автоматически созданный видовой экран удаляется.
  (setq _sysvar-layout-create-viewport (getvar "LAYOUTCREATEVIEWPORT"))

  ;; -----------------
  ;; Восстановление изначального состояния
  ;; -----------------
  (defun _restore-state () 
    (setvar "CTAB" _user-tab)
    (setvar "CLAYER" _user-layer)
    (if _sysvar-layout-create-viewport 
      (setvar "LAYOUTCREATEVIEWPORT" 
              _sysvar-layout-create-viewport
      )
    )
    (setq *error* _old-error)
  )

  ;; -----------------
  ;; Обработка ошибок
  ;; -----------------
  (defun _error-handler (msg) 
    ;; Сообщение об ошибке
    (if 
      (member msg 
              '("console break" "Function cancelled" "Функция отменена" "quit / exit abort" "завершить / выйти прервать" 
                "Функция прервана."
               )
      )
      (princ "\nПрерывание команды")
      (princ 
        (strcat "\ERRNO # " (itoa (getvar "ERRNO")) ": " msg "\n")
      )
    )

    ;; Восстановление изначальных настроек и состояния чертежа
    (_restore-state)
    (princ)
  )
  (setq *error* _error-handler)

  ;; -----------------
  ;; Функция группировки рамок в порядке слева-направо сверху вниз
  ;; -----------------
  (defun _order-frames-points (frames-list / _frames-list _frames-groups _found-group) 
    ;; Получает максимальную Y-координату рамки
    ;; Принимает на вход список с ключом вида ((X1 Y1 Z1) (X1 Y1 Z1))
    (defun _get-max-Y-coord (frame /) 
      (max (cadr (car frame)) (cadr (cadr frame)))
    )

    ;; Проверяет пересечение рамок по Y-координатам, то есть одна из точек одной из рамок
    ;; по вертикальной оси находится между двумя точками другой рамки
    (defun _check-frames-Y-intersection (frame1 frame2 / y11 y12 y21 y22 y1max y1min) 
      (setq y11   (cadr (car frame1))
            y12   (cadr (cadr frame1))
            y21   (cadr (car frame2))
            y22   (cadr (cadr frame2))
            y1max (max y11 y12)
            y1min (min y11 y12)
      )
      (or (and (<= y2max y1max) (>= y2max y1min)) 
          (and (<= y21 y1max) (>= y22 y1min))
      )
    )

    ;; Проверяет, входит ли рамка в какую-то группу рамок (на основании пересечения Y-координат)
    ;; Возвращает группу, к которой подходит рамка
    (defun _check-frame-in-group (frame frames-groups / _index _len _found-group _group _frame-group) 
      (setq _index       0
            _len         (length frames-groups)
            _found-group nil
            _group       nil
      )


      ;; Цикл по группам
      (while (and (< _index _len) (not _found-group)) 
        (setq _group (nth _index frames-groups))

        ;; Цикл по рамкам внутри группы
        (setq _sub-index 0
              _sub-len   (length _group)
        )
        (while (and (< _sub-index _sub-len) (not _found-group)) 
          (setq _frame-group (nth _sub-index _group))

          (if (_check-frames-Y-intersection _frame-group frame) 
            (setq _found-group _group)
          )
          (setq _sub-index (1+ _sub-index))
        )
        (setq _index (1+ _index))
      )

      _found-group
    )

    ;; Сортировка рамок в порядке убывания максимальной Y-координаты
    (setq _frames-list (vl-sort frames-list 
                                (function 
                                  (lambda (frame1 frame2 /) 
                                    (> (_get-max-Y-coord frame1) (_get-max-Y-coord frame2))
                                  )
                                )
                       )
    )

    ;; Проход по рамкам для выделения групп
    (setq _frames-groups '())
    (foreach frame _frames-list 
      ;; Проверка, подходит ли рамка в группу рамок
      (setq _found-group (_check-frame-in-group frame _frames-groups))
      (if _found-group 
        ;; Если группа найдена, то в неё добавляется рамка
        (setq _frames-groups (subst (_add-to-list _found-group frame) 
                                    _found-group
                                    _frames-groups
                             )
        )

        ;; Если группа не найдена, то создаётся новая группа с рамкой
        (setq _frames-groups (_add-to-list _frames-groups (list frame)))
      )
    )

    ;; Сортирует рамки внутри групп по центральной X-координате
    (defun _sort-frames-by-center-X (frames-groups /) 
      ;; Получает центральную X-координату рамки
      (defun _get-center-X-coord (frame /) 
        (/ (+ (car (car frame)) (caar (cdr frame))) 2)
      )

      (mapcar 
        (function 
          (lambda (group) 
            (vl-sort group 
                     (function 
                       (lambda (frame1 frame2) 
                         (< (_get-center-X-coord frame1) 
                            (_get-center-X-coord frame2)
                         )
                       )
                     )
            )
          )
        )
        frames-groups
      )
    )

    ;; Сортировка рамок внутри групп по X-координате центральной точки рамки
    (_sort-frames-by-center-X _frames-groups)
  )

  ;; -----------------
  ;; Функция получения формата и ориентации листа в зависимости от размеров
  ;; -----------------
  (defun _get-plot-config (width height / _format_dims_name _stand_formats _index _stand_format _numb_of_stand_formats) 
    (setq _stand_formats         (list 
                                   '((1189 . 841) . "A0 L")
                                   '((841 . 1189) . "A0 P")
                                   '((1189 . 1682) . "A0x2 P")
                                   '((1682 . 1189) . "A0x2 L")
                                   '((1189 . 2523) . "A0x3 P")
                                   '((2523 . 1189) . "A0x3 L")
                                   '((594 . 841) . "A1 P")
                                   '((841 . 594) . "A1 L")
                                   '((1783 . 841) . "A1x3 L")
                                   '((841 . 1783) . "A1x3 P")
                                   '((2378 . 841) . "A1x4 L")
                                   '((841 . 2378) . "A1x4 P")
                                   '((420 . 594) . "A2 P")
                                   '((594 . 420) . "A2 L")
                                   '((1261 . 594) . "A2x3 L")
                                   '((594 . 1261) . "A2x3 P")
                                   '((1682 . 594) . "A2x4 L")
                                   '((594 . 1682) . "A2x4 P")
                                   '((2102 . 594) . "A2x5 L")
                                   '((594 . 2102) . "A2x5 P")
                                   '((297 . 420) . "A3 P")
                                   '((420 . 297) . "A3 L")
                                   '((420 . 891) . "A3x3 P")
                                   '((891 . 420) . "A3x3 L")
                                   '((1189 . 420) . "A3x4 L")
                                   '((420 . 1189) . "A3x4 P")
                                   '((1486 . 420) . "A3x5 L")
                                   '((420 . 1486) . "A3x5 P")
                                   '((1783 . 420) . "A3x6 L")
                                   '((420 . 1783) . "A3x6 P")
                                   '((2080 . 420) . "A3x7 L")
                                   '((420 . 2080) . "A3x7 P")
                                   '((210 . 297) . "A4 P")
                                   '((297 . 210) . "A4 L")
                                   '((297 . 630) . "A4x3 P")
                                   '((630 . 297) . "A4x3 L")
                                   '((297 . 841) . "A4x4 P")
                                   '((841 . 297) . "A4x4 L")
                                   '((1051 . 297) . "A4x5 L")
                                   '((297 . 1051) . "A4x5 P")
                                   '((1261 . 297) . "A4x6 L")
                                   '((297 . 1261) . "A4x6 P")
                                   '((1471 . 297) . "A4x7 L")
                                   '((297 . 1471) . "A4x7 P")
                                   '((1682 . 297) . "A4x8 L")
                                   '((297 . 1682) . "A4x8 P")
                                   '((1892 . 297) . "A4x9 L")
                                   '((297 . 1892) . "A4x9 P")
                                 )
          _index                 0
          _stand_format          nil
          _error                 2.5
          _numb_of_stand_formats (length _stand_formats)
    )

    (while 
      (and 
        (null _stand_format)
        (< _index _numb_of_stand_formats)
      )
      (setq _format_dims_name (nth _index _stand_formats)
            _format_width     (caar _format_dims_name)
            _format_height    (cdar _format_dims_name)
      )
      (if 
        (and 
          (< (abs (- width _format_width)) _error)
          (< (abs (- height _format_height)) _error)
        )
        (setq _stand_format (cdr _format_dims_name))
      )
      (setq _index (1+ _index))
    )
    (princ _stand_format)
  )

  ;; -----------------
  ;; Создаёт видовой экран на вкладке листа и задаёт параметры печати в зависимости от размера видового экрана
  ;; -----------------
  (defun _create-viewport-on-layout (frame paper-space sysvar-layout-create-viewport / _point1 _point2 _point1x _point1y 
                                     _point2x _point2y _viewport-height _viewport-width _viewport _format-name 
                                     _plot-config _plot-config-name
                                    ) 





    (if (not sysvar-layout-create-viewport) 
      ;; Удаление видового экрана из пространства листа при его автоматическом создании
      (vlax-for item paper-space 
        (if (= (vla-get-ObjectName item) "AcDbViewport") 
          (vl-catch-all-error-p 
            (vl-catch-all-apply 'vla-Delete (list item))
          )
        )
      )

      ;; Отключения создания видовых экранов на новых листах
      (if (= _sysvar-layout-create-viewport 1) 
        (setvar "LAYOUTCREATEVIEWPORT" 0)
      )
    )

    (setq _point1          (car frame)
          _point2          (cadr frame)
          _point1x         (car _point1)
          _point1y         (cadr _point1)
          _point2x         (car _point2)
          _point2y         (cadr _point2)
          _viewport-height (abs (- _point1y _point2y))
          _viewport-width  (abs (- _point1x _point2x))
          _viewport        (vla-AddPViewport 
                             paper-space
                             (vlax-3D-point 
                               (list (/ _viewport-width 2) 
                                     (/ _viewport-height 2)
                               )
                             )
                             _viewport-width
                             _viewport-height
                           )
    )
    (vla-Display _viewport :vlax-true)
    (vla-put-MSpace _active-document :vlax-true)
    (vla-ZoomCenter 
      _application
      (vlax-3D-point 
        (list (/ (+ _point1x _point2x) 2) 
              (/ (+ _point1y _point2y) 2)
        )
      )
      1.0
    )
    (vla-put-MSpace _active-document :vlax-false)
    (vla-put-StandardScale _viewport acVp1_1)
    (vla-put-DisplayLocked _viewport "-1")
    (vla-put-Layer _viewport "Defpoints")

    ;; Установка параметров печати
    (setq _format-name (_get-plot-config _viewport-width _viewport-height))
    (if _format-name 
      (progn 
        (setq _plot-config-name (strcat "PDF " _format-name)
              _plot-config      (vl-catch-all-apply 
                                  'vla-Item
                                  (list _plot-configs _plot-config-name)
                                )
        )
        (if (not (vl-catch-all-error-p _plot-config)) 
          (vla-CopyFrom 
            (vla-get-ActiveLayout _active-document)
            _plot-config
          )
        )
      )
    )

    ;; Зумирование вкладки листа по центру видового экрана
    (vla-ZoomExtents _application)
  )

  ;; -----------------
  ;; Преобразование набора примитивов в список
  ;; -----------------
  (defun _ss-to-list (ss / _i _lst) 
    (if ss 
      (repeat (setq _i (sslength ss)) 
        (setq _lst (cons (ssname ss (setq _i (1- _i))) _lst))
      )
    )
    _lst
  )

  ;; -----------------
  ;; Получает рамки (координаты точек) из выбранных пользователем в пространстве модели объектов
  ;; -----------------
  (defun _get-frames-from-entities (active-document / _object _frames-layer _frames-entities _formats-number 
                                    _frames-points _total-num-layouts +max-num-layouts+ _frame
                                   ) 
    (setq +max-num-layouts+ 255) ; Предельное количество вкладок листов в AutoCAD

    ;; Получение слоя с рамками
    (initget 6)
    (while (null _object) 
      (setq _object (car 
                      (entsel "Выберите объект для определения слоя с рамками")
                    )
      )
    )

    ;; Получение примитивов рамок, провеирка на возможное превышение предельного количества вкладок листов
    (setq _total-num-layouts (+ +max-num-layouts+ 1)) ; Предельное количество вкладок листов в AutoCAD + 1
    (while (> _total-num-layouts +max-num-layouts+) 
      ;; Получение набора рамок (форматов)
      (setq _frames-layer      (cdr (assoc 8 (entget _object)))
            _frames-entities   (ssget (list (cons 8 _frames-layer)))
            _formats-number    (sslength _frames-entities)
            _total-num-layouts (+ (length (layoutlist)) _formats-number) ; Итоговое количество листов в чертеже
      )

      (if (> _total-num-layouts +max-num-layouts+) 
        (alert 
          (strcat "Превышено максимальное число вкладок листов в чертеже (" 
                  +max-num-layouts+
                  "). Уменьшите количество рамок чертежей"
          )
        )
      )
    )

    ;; Получение точек прямоугольника, описывающего выделенные объекты
    (setq _frames-points '()
          _frame-index   0
    )

    (mapcar 
      (function 
        (lambda (frame-entity / _frame-obj) 
          (setq _frame-obj (vlax-ename->vla-object frame-entity))

          (if 
            ;; Если объект является динамическим блоком, то получение рамки при помощи функции _get_bounding_box_dynblock
            (and (= (vla-get-ObjectName _frame-obj) "AcDbBlockReference") 
                 (= (vla-get-IsDynamicBlock _frame-obj) :vlax-true)
            )

            (list 
              (_get_bounding_box_dynblock 
                _frame
                active-document
              )
            )

            (progn 
              (vla-GetBoundingBox 
                _frame-obj
                '_min-point
                '_max-point
              )
              (list (vlax-safearray->list _min-point) 
                    (vlax-safearray->list _max-point)
              )
            )
          )
        )
      )

      (_ss-to-list _frames-entities)
    )
  )

  ;; Переключение на вкладку модели
  (setvar "CTAB" "Model")
  (setq _user-layer      (getvar "CLAYER")
        _application     (vlax-get-acad-object)
        _active-document (vla-get-ActiveDocument _application)
        _layouts         (vla-get-Layouts _active-document)
        _plot-configs    (vla-get-PlotConfigurations _active-document)
  )

  ;; Получение рамок чертежей из выбранных пользователем объектов
  (setq _frames-points (_get-frames-from-entities _active-document))

  ;; Группировка рамок в группы по горизонтали
  ;; Группа рамок, находящихся на одной горизонтальной оси, определяется через пересечения Y-координат точек рамок
  (setq _frames-groups (_order-frames-points _frames-points))

  ;; Запросы имён и номеров листов
  ;; TODO: возможно сделать обособленной функцией с возвратом _new-layouts-names
  (while 
    ((lambda (/ _matched-layouts-names _name-prefix _name-suffix _first-layout-index _frame-index _new-name) 

       (initget 1)
       (setq _name-prefix (getstring T "Префикс имени листа:")
             _name-suffix (getstring T "Суффикс имени листа:")
       )
       (initget 6)
       (setq _first-layout-index    (getint "Номер первого листа:")
             _new-layouts-names     '()
             _frame-index           0
             _matched-layouts-names '()
             _new-name              nil
       )

       (repeat (length _frames-points) 
         (setq _new-name          (strcat _name-prefix 
                                          (itoa (+ _first-layout-index _frame-index))
                                          _name-suffix
                                  )
               _new-layouts-names (_add-to-list _new-layouts-names _new-name)
               _frame-index       (1+ _frame-index)
         )
         (if (member _new-name (layoutlist)) 
           (setq _matched-layouts-names (_add-to-list _new-name))
         )
       )
       ;; Уведомление о наличии совпадения имён листов с существующими
       (if _matched-layouts-names 
         (progn 
           (if (= (vl-list-length _matched-layouts-names) 1) 
             (alert 
               (strcat "Лист с именем " 
                       (car _matched-layouts-names)
                       " уже присутствует в чертеже. Задайте другое имя"
               )
             )
             (alert 
               (strcat "Листы с именами " 
                       (_str-join _matched-layouts-names ", ")
                       " уже присутствуют в чертеже. Задайте другие имена"
               )
             )
           )
           T
         )
         nil
       )
     ) 
    )
  )

  ;; Добавление слоя Defpoints для размещения на нём видовых экранов
  (_add-layer "Defpoints")

  ;; Проход по группам рамок, создание листов и видовых экранов на листах
  (setq _frame-index 0)
  (foreach group _frames-groups 
    (foreach frame group 
      (setq _sheet-name (nth _frame-index _new-layouts-names)
            _layout     (vla-Add _layouts _sheet-name)
      )

      (setvar "CTAB" _sheet-name)
      (setq _paper-space (vla-get-PaperSpace _active-document))

      ;; Создание видового экранана вкладке листа
      (_create-viewport-on-layout 
        frame
        _paper-space
        _sysvar-layout-create-viewport
      )

      (setq _frame-index (1+ _frame-index))
    )
  )

  ;; Восстановление изначальных настроек и состояния чертежа
  (_restore-state)
)