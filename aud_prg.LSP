(defun C:AUDPRG (/) 
  (command "_AUDIT" "_Y")
  (command "_-PURGE" "_ALL" "*" "_N")
)

(defun C:PRG (/) 
  (command "_-PURGE" "_ALL" "*" "_N")
)

(defun C:NCAUDPRG (/) 
  (command "_AUDIT" "_Y" "_N")
  (command "_-PURGE" "_ALL" "*" "_N")
)