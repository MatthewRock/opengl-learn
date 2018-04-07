(in-package :opengl-learn)

;; (defmacro with-* (name form)
;;   (labels ((create-with-name (name)
;;              (intern (format nil "~:@(with-~A~)" name))))
;;     `(defmacro ,(create-with-name name) ((name init) &body body)
;;        (let ((,name ,init))))))

(defmacro with-program ((name init) &body body)
  `(let ((,name ,init))
    (unwind-protect (gl:delete-program ,name)
      ,@body)))
