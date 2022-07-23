;; SPDX-License-Identifier: LGPL-3.0-or-later
;; Copyright (C) 2022 Massimo Zaniboni <mzan@dokmelody.org>

(ql:quickload :alexandria)        ;; common CL extensions
(ql:quickload :trivial-types)     ;; common types
(ql:quickload :defstar)           ;; add type annotations
(ql:quickload :iterate)           ;; better loop macro
(ql:quickload :str)               ;; common string manipulation functions
(ql:quickload :let-plus)          ;; extend "let"
(ql:quickload :array-operations)  ;; rich management of arrays
(ql:quickload :sdl2)              ;; portable graphics
(ql:quickload :cl-opengl)         ;; opengl support
(ql:quickload :cffi)              ;; C foreign functional interface

(defpackage :snow
  (:use :cl :defstar :trivial-types :iterate :sdl2 :cffi)
  (:import-from :cl-opengl)
  )

(in-package :snow)

; FACT it is better using a surface apart because the surface of a window can be accelerated
; and emulated as a streaming texture
;
; FACT I try with this paradigm:
; - YES: macro injecting piece of code
; - YES: inlined functions
; - YES: stream-like API with internal state
; - YES: cached data in slot, that are fast accessible
; - NO: generic functions for simple data
; - NO: complex CLOS hierarchies and abstractions
; TODO deinit resources like surface at the end of the object
; TODO use only a surface dimension and calculate the dimension of the griglia accordingly

(defmacro with-surface (name create &body body)
  `(let ((,name ,create))
      (unwind-protect (progn ,@body) (sdl2:free-surface ,name))))

(defclass snow ()

  ((dim
    :documentation "x and y of the grid, go from 0 to (- (grid-dim self) 1)."
    :initarg :dim
    :type integer)

   (max-id
    :documentation "The maximum id of a cell inside the grid."
    :type integer
    :initform 0
    )

   (center-id
    :documentation "The position of the center cell."
    :type integer
    :initform 0)

   (screen-width
    :documentation "The width of the grid on the screen."
    :type integer
    :initform 0)

   (screen-height
    :documentation "The height of the grid on the screen."
    :type integer
    :initform 0)

   (sdl-surface
    :initform nil
    :documentation "Current SDL surface with the content of the grid."
   )

   (surface-max-pos
    :documentation "The maximum offset where to write in the surface."
    :type integer
    :initform 0)

   (sdl-fmt
    :initform nil
    :documentation "Current SDL pixel format of sdl-surface."
   )

   (color-R
    :documentation "Current color, R compontent."
    :type integer
    :initform 0)

   (color-G
    :documentation "Current color, G component."
    :type integer
    :initform 0)

   (color-B
    :documentation "Current color, B component."
    :type integer
    :initform 0)
   )

  (:documentation "
     An hexagonal grid.

      Every cell has 6 neighbours.
      A cell becomes alive if it has an odd number of live neighbours.
      A live cell, never dies."))

(defun* (snow-screen-to-dim -> integer) ((screen-width integer) (screen-height integer))
  (min (floor screen-width 2)
       (floor screen-height 2)))

(defmethod initialize-instance :after ((obj snow) &key)
  (with-slots (dim) obj
    (setf (slot-value obj 'max-id) (1- (* dim dim)))
    (setf (slot-value obj 'center-id) (+ (floor (* dim dim) 2) (floor dim 2)))

    (setf (slot-value obj 'screen-width) (* dim 2))
    (setf (slot-value obj 'screen-height) (* dim 2))

    (with-slots (screen-width screen-height) obj
      (setf (slot-value obj 'sdl-surface) (sdl2:create-rgb-surface screen-width screen-height 32))
      (setf (slot-value obj 'sdl-fmt) (sdl2:surface-format (slot-value obj 'sdl-surface)))
      (setf (slot-value obj 'surface-max-pos) (1- (* screen-width screen-height)))

      (with-rects
          ((rect1 0 0 screen-width screen-height))
          (sdl2:fill-rect (slot-value obj 'sdl-surface) rect1 (sdl2:map-rgb (slot-value obj 'sdl-fmt) 0 0 0)))
    )))

(defgeneric* (snow-next-color! -> :void) ((obj snow))
  (:documentation "After next!, use a new color for the cells."))

(defmethod snow-next-color! ((obj snow))
  (setf (slot-value obj 'color-R) (random 255))
  (setf (slot-value obj 'color-G) (random 255))
  (setf (slot-value obj 'color-B) (random 255)))

(defun* snow-current-color ((obj snow))
  "Calculate the current color.

   NOTE: this can be slow, so cache the result whenever possible."
  (with-slots (sdl-fmt color-R color-G color-B) obj
    (sdl2:map-rgb sdl-fmt color-R color-G color-B)))

(declaim (inline snow-draw-live-cell))
(defun snow-draw-live-cell (id color
                            dim
                            screen-width surface-max-pos pxs)
  "Draw in a rather fast way a live cell on the SDL surface.
   Hence, many params must be passed for avoiding a slow access to object properties."

  ; A snow cell is an hexagon with this shape
  ;
  ;          *
  ;         ***
  ;         ***
  ;          *
  (let* ((t1 (floor id dim))
         (offset-x (mod t1 2))
         (screen-x (+ offset-x (* 2 (mod id dim))))
         (screen-y (* 2 t1))
         (py1 (+ screen-x (* screen-width screen-y)))
         (py2 (+ py1 screen-width))
         (py3 (+ py2 screen-width))
         (py4 (+ py3 screen-width)))

        (macrolet ((dp (p)
                     `(when (and (>= ,p 0) (<= ,p surface-max-pos))
                        (setf (mem-aref pxs :uint32 ,p) color))))

           (dp (1+ py1))     ; centered *

           (dp py2)          ; middle ***
           (dp (+ 1 py2))
           (dp (+ 2 py2))

           (dp py3)          ; middle ***
           (dp (+ 1 py3))
           (dp (+ 2 py3))

           (dp (1+ py4))    ; centered *
          )))

(defgeneric* (next! -> :void) ((obj snow))
  (:documentation "Advance the calculation of sdl-surface."))

(defun* (snow-do-on-neighbours-of -> :void) ((dim integer) (max-id integer) (id integer) f)
  "Execute f on all the neighbours of the cell."
  (let* ((g dim)
         (gx (mod id g))
         (h  (mod gx 2))
         (-h (- h))
         (c-up (- id g h))
         (c-down (+ id g -h))

         (id1 (1- id))
         (id2 (1+ id))
         (id3 c-up)
         (id4 (1+ c-up))
         (id5 c-down)
         (id6 (1+ c-down)))

   (flet ((ff (cell-id)
             (when (and (>= cell-id 0) (<= cell-id max-id))
                (funcall f cell-id))))

     (ff id1)
     (ff id2)
     (ff id3)
     (ff id4)
     (ff id5)
     (ff id6))

     nil
  ))

(defclass snow-counter (snow)
  (
   (neighbours
    :documentation "Count the neighbours of every cell.")

   (new-births
    :documentation "New birthed cells to process.")

   (new-births-count
    :type integer
    :initform 0)

   (active-cells
    :documentation "Cells that can became alive in next iteration, and that must be checked.")

   (active-cells-count
    :type integer
    :initform 0)

 )

  (:documentation "Calculate snow using an array counting the neighbours."))

(declaim (inline snow-counter-add-new-active-cell))
(defun* (snow-counter-add-new-active-cell -> :void) ((obj snow-counter) (i integer))
  (with-slots (max-id active-cells active-cells-count) obj
    (when (and (>= i 0) (<= i max-id))
      (setf (aref active-cells active-cells-count) i))

      (incf (slot-value obj 'active-cells-count))))

(declaim (inline snow-counter-add-new-birth))
(defun* (snow-counter-add-new-birth -> :void) ((obj snow-counter) (i integer))
  (with-slots (max-id new-births new-births-count) obj
    (when (and (>= i 0) (<= i max-id))
      (setf (aref new-births new-births-count) i))

      (incf (slot-value obj 'new-births-count))))

(defmethod initialize-instance :after ((obj snow-counter) &key)
  (with-slots (dim max-id center-id) obj
    (setf (slot-value obj 'neighbours)
          (make-array (1+ max-id)
                      :adjustable nil
                      :element-type '(integer 0 6)
                      :initial-element 0))

    (setf (slot-value obj 'new-births)
          (make-array (floor max-id 8)
                      :adjustable t
                      :element-type 'integer))

    (setf (slot-value obj 'active-cells)
          (make-array (floor max-id 4)
                      :adjustable t
                      :element-type 'integer))

    (snow-counter-add-new-birth obj center-id)
    ))

; TODO
(defun test2 ()
  (let* ((s (make-instance 'snow-counter :dim 100)))
    (format t "active-cells ~a ~%" (slot-value s 'screen-width))
    )
)

(defun* (snow-counter-process-new-births -> :void) ((obj snow-counter))
  (with-slots (new-births new-births-count
               dim max-id neighbours
               screen-width
               surface-max-pos
               sdl-surface) obj
    (iter (with color = (snow-current-color obj))
          (with pxs = (sdl2:surface-pixels sdl-surface))
          (for i from 0 below new-births-count)
          (for j = (aref new-births i))
          (after-each
             (snow-draw-live-cell j color
                                  dim
                                  screen-width surface-max-pos
                                  pxs)

             (snow-do-on-neighbours-of dim max-id j
               (lambda (k)
                  (when (= 1 (incf (aref neighbours k)))
                    ; this is the first time this neighbour has a live cell near him,
                    ; so now it must be monitored.
                    (snow-counter-add-new-active-cell obj k)
                 )))))

   (setf (slot-value obj 'new-births-count) 0)))

(defun* (snow-counter-process-active-cells -> :void) ((obj snow-counter))
  (with-slots (active-cells active-cells-count neighbours) obj
    (iter (with last-i = (1- active-cells-count))
          (for i first 0 then (1+ i))
          (while (<= i last-i))
          (for j = (aref active-cells i))
          (for c = (aref neighbours j))
          (for new-born? = (oddp c))
          (for stable? = (= c 6))
          (when new-born? (snow-counter-add-new-birth obj j))
          (when (or new-born? stable?)
                ; This cell can never change state in future, so remove from active cells.
                ; Take the last active cell on the queue and put in the position of this cell,
                ; for reducing the size of the queue, and for removing this cell.
                ; Do not move i pointer, because now we have a new value on it.
                (when (< i last-i)
                  (setf (aref active-cells i) (aref active-cells last-i))
                  (decf i))

                (decf last-i)
                (decf active-cells-count))
          (finally
             (setf (slot-value obj 'active-cells-count) active-cells-count)))))

(defmethod next! ((obj snow-counter))
    (snow-counter-process-new-births obj)
    (snow-counter-process-active-cells obj)
    (snow-next-color! obj))

(defun* (main -> :void) ()
  "Test graphics."

  (sdl2:with-init (:everything)
    (multiple-value-bind (_1 w h _2)
      (sdl2:get-current-display-mode 0)

      (sdl2:with-window (win :title "Snow 0.1" :w w :h h :flags '(:shown :maximized))

      (multiple-value-bind (win-width win-height)
        (sdl2:get-window-size win)

        (let* ((dim (snow-screen-to-dim win-width win-height))
               (snow (make-instance 'snow-counter :dim dim)))

      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
          (sdl2:blit-surface (slot-value snow 'sdl-surface) nil (sdl2:get-window-surface win) nil)
          (sdl2:update-window win)
          (next! snow)
          (sdl2:delay 1))
        )))))))

(main)
