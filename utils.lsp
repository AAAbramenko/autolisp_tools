(defun c:PRDXF (/ _selected _dxf-data) 

  (while (not _selected) 
    (setq _selected (entsel))
  )

  (setq _dxf-data (entget (car _selected) '("*")))
  (foreach item _dxf-data (print item))
  (print)
)

(defun c:DMPO (/ _selected) 
  (vl-load-com)

  (while (not _selected) 
    (setq _selected (entsel))
  )

  (vlax-dump-object (vlax-ename->vla-object (car _selected)))
)