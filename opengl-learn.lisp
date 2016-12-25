;;;; opengl-learn.lisp

(in-package #:opengl-learn)

;;; "opengl-learn" goes here. Hacks and glory await!

(defconstant +screen-width+ 1280)
(defconstant +screen-height+ 720)
(defconstant +screen-title+ "OpenGL Tutorial")

(define-condition shader-error (error)
  ((text :initarg :text :initform "" :reader shader-error/text))
  (:report (lambda (condition stream)
             (format stream "~A signalled: ~%~A~%"
                     (class-name (class-of condition))(shader-error/text condition)))))

(define-condition shader-compilation-error (shader-error)
  ((file :initarg :file :initform "" :reader shader-compilation-error/file)))

(defmethod print-object :before ((condition shader-compilation-error) stream)
  (format stream "During compilation of file ~A:~%" (shader-compilation-error/file condition)))

(define-condition shader-linking-error (shader-error) ())

(defun main ()
  (sdl2:with-everything (:window
                         (win :w +screen-width+
                              :h +screen-height+
                              :title +screen-title+
                              :flags '(:shown :opengl :resizable))
                         :gl gl-context)
    (let ((vao (gl:create-vertex-array)))
      (gl:bind-vertex-array vao)
      (gl:with-gl-array (g-vertex-buffer-data :float :count 9)
        ;;Populate the array
        (loop
           with tempar = #(-1.0 -1.0 0.0
                           1.0 -1.0 0.0
                           0.0 1.0 0.0)
           for i from 0 below (length tempar) do
             (setf (gl:glaref g-vertex-buffer-data i) (aref tempar i)))
        (let ((vertex-buffer (gl:create-vertex-array)))
          (gl:bind-buffer :array-buffer vertex-buffer)
          (gl:buffer-data :array-buffer :static-draw g-vertex-buffer-data)
          (let ((program (load-shaders #P"~/Programming/Lisp/opengl-learn/triangle.vertexshader"
                                       #P"~/Programming/Lisp/opengl-learn/triangle.fragmentshader")))
            (sdl2:with-event-loop ()
              (:quit () t)
              (:idle ()
                     (gl:clear :color-buffer-bit :depth-buffer-bit)
                     (gl:use-program program)
                     (gl:enable-vertex-attrib-array 0)
                     (gl:bind-buffer :array-buffer vertex-buffer)
                     (gl:vertex-attrib-pointer 0 3 :float :false 0 (cffi:null-pointer))
                     (gl:draw-arrays :triangles 0 3)
                     (gl:disable-vertex-attrib-array 0)
                     (sdl2:gl-swap-window win)))))))))

(defun load-shaders (vertex-file-path fragment-file-path)
  (let ((vertex-shader (load-shader vertex-file-path :vertex-shader))
        (fragment-shader (load-shader fragment-file-path :fragment-shader))
        (program (gl:create-program)))
    (gl:attach-shader program vertex-shader)
    (gl:attach-shader program fragment-shader)
    (gl:link-program program)
    (unless (gl:get-program program :link-status)
      (error 'shader-linking-error :text (gl:get-program-info-log program)))
    (gl:detach-shader program fragment-shader)
    (gl:detach-shader program vertex-shader)
    (gl:delete-shader fragment-shader)
    (gl:delete-shader vertex-shader)
    program))

(defun load-shader (path type)
  "Load shader found in PATH of type TYPE"
  (assert (or (eql type :vertex-shader)
              (eql type :fragment-shader)))
  (let ((shader (gl:create-shader type))
        (shader-file (load-file-to-string path)))
    (gl:shader-source shader shader-file)
    (gl:compile-shader shader)
    (unless (gl:get-shader shader :compile-status)
      (error 'shader-compilation-error
             :text (gl:get-shader-info-log shader)
             :file path))
    shader))

(defun load-file-to-string (path)
  "Load file at PATH into string."
  (format nil "~{~A~%~}" (load-file-to-lists path)))

(defun load-file-to-lists (path)
  "Load file at PATH into list, each line being separate string."
  (with-open-file (in path)
    (loop for line = (read-line in nil 'eof nil)
       while (not (eql line 'eof)) collect line)))


;;One of the possible problems:

;;[package opengl-learn]

; file: /home/malice/Programming/Lisp/opengl-learn/opengl-learn.lisp
; in: DEFUN MAIN
;     (SDL2:WITH-EVENT-LOOP NIL
;       (:QUIT T)
;       (:IDLE (CL-OPENGL-BINDINGS:ENABLE-VERTEX-ATTRIB-ARRAY 0)
;        (CL-OPENGL-BINDINGS:BIND-BUFFER :ARRAY-BUFFER OPENGL-LEARN::VERTEX-BUFFER)
;        (CL-OPENGL-BINDINGS:VERTEX-ATTRIB-POINTER 0 3 :FLOAT :FALSE 0
;                                                  (CFFI-SYS:NULL-POINTER))
;        (CL-OPENGL-BINDINGS:DRAW-ARRAYS :TRIANGLES 0 3)
;        (CL-OPENGL-BINDINGS:DISABLE-VERTEX-ATTRIB-ARRAY 0)))
; --> WHEN IF PROGN SDL2:IN-MAIN-THREAD LET LAMBDA FUNCTION LET
; --> UNWIND-PROTECT FLET BLOCK MULTIPLE-VALUE-BIND MULTIPLE-VALUE-CALL
; --> BLOCK SB-C::%WITHIN-CLEANUP RETURN-FROM SDL2:WITH-SDL-EVENT
; --> PLUS-C:C-LET LET MACROLET SYMBOL-MACROLET UNWIND-PROTECT FLET
; --> BLOCK MULTIPLE-VALUE-BIND MULTIPLE-VALUE-CALL BLOCK
; --> SB-C::%WITHIN-CLEANUP RETURN-FROM PROGN LOOP BLOCK
; --> SB-LOOP::LOOP-BODY TAGBODY PROGN LOOP BLOCK LET
; --> SB-LOOP::LOOP-BODY TAGBODY LET* CASE LET COND IF IF IF PROGN
; ==>
;   (LET ((0
;          (PLUS-C:C-REF #:SDL-EVENT-66 SDL2-FFI:SDL-EVENT :USER
;                        CL-OPENGL-BINDINGS:ENABLE-VERTEX-ATTRIB-ARRAY)))
;     (CL-OPENGL-BINDINGS:BIND-BUFFER :ARRAY-BUFFER OPENGL-LEARN::VERTEX-BUFFER)
;     (CL-OPENGL-BINDINGS:VERTEX-ATTRIB-POINTER 0 3 :FLOAT :FALSE 0
;                                               (CFFI-SYS:NULL-POINTER))
;     (CL-OPENGL-BINDINGS:DRAW-ARRAYS :TRIANGLES 0 3)
;     (CL-OPENGL-BINDINGS:DISABLE-VERTEX-ATTRIB-ARRAY 0))
;
; caught ERROR:
;   0 is not a symbol, and cannot be used as a variable.
;


;; Fix:
;; No () in sdl2:event-loop clauses
