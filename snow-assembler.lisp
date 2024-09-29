;; SPDX-License-Identifier: LGPL-3.0-or-later
;; Copyright (C) 2022 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :snow-assembler
  (:use :cl :defstar :trivial-types :iterate :sdl2 :cffi)
  (:import-from :cl-opengl)
  (:import-from :trivial-benchmark)
  (:import-from :random-state)
  (:export #:main))

(in-package :snow-assembler)

(defmacro with-surface (name create &body body)
  `(let ((,name ,create))
      (unwind-protect (progn ,@body) (sdl2:free-surface ,name))))

(defclass snow ()

  ((dim
    :documentation "x and y of the grid, go from 0 to (- (grid-dim self) 1)."
    :initarg :dim
    :type fixnum)

   (max-id
    :documentation "The maximum id of a cell inside the grid."
    :type fixnum
    :initform 0
    )

   (center-id
    :documentation "The position of the center cell."
    :type fixnum
    :initform 0)

   (screen-width
    :documentation "The width of the grid on the screen."
    :type fixnum
    :initform 0)

   (screen-height
    :documentation "The height of the grid on the screen."
    :type fixnum
    :initform 0)

   (sdl-surface
    :initform nil
    :documentation "Current SDL surface with the content of the grid.")

   (rnd
    :initform (random-state:make-generator :mersenne-twister-32 627))

   (surface-max-pos
    :documentation "The maximum offset where to write in the surface."
    :type fixnum
    :initform 0)

   (sdl-fmt
    :initform nil
    :documentation "Current SDL pixel format of sdl-surface.")

   (color-R
    :documentation "Current color, R compontent."
    :type (integer 0 255)
    :initform 0)

   (color-G
    :documentation "Current color, G component."
    :type (integer 0 255)
    :initform 0)

   (color-B
    :documentation "Current color, B component."
    :type (integer 0 255)
    :initform 0))

  (:documentation "
     An hexagonal grid.

      Every cell has 6 neighbours.
      A cell becomes alive if it has an odd number of live neighbours.
      A live cell, never dies."))

(defun* (snow-screen-to-dim -> fixnum) ((screen-width fixnum) (screen-height fixnum))
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
          (sdl2:fill-rect (slot-value obj 'sdl-surface) rect1 (sdl2:map-rgb (slot-value obj 'sdl-fmt) 0 0 0))))))

(defgeneric* (snow-next-color! -> :void) ((obj snow))
  (:documentation "After next!, use a new color for the cells."))

(defgeneric* (real-time-pause-between-frames -> :integer) ((obj snow) &key (expected-duration-in-seconds 5))
  (:documentation "The pause between frames, in internal-units-per-second."))

(defmethod real-time-pause-between-frames ((obj snow) &key (expected-duration-in-seconds 5))
  (with-slots (dim) obj
  (let ((duration (* expected-duration-in-seconds internal-time-units-per-second)))
        (floor dim duration))))

(defmethod snow-next-color! ((obj snow))
  (with-slots (rnd) obj
    (setf (slot-value obj 'color-R) (random-state:random-int rnd 0 255))
    (setf (slot-value obj 'color-G) (random-state:random-int rnd 0 255))
    (setf (slot-value obj 'color-B) (random-state:random-int rnd 0 255))))

(defun* snow-current-color ((obj snow))
  "Calculate the current color.

   NOTE: this can be slow, so cache the result whenever possible."
  (with-slots (sdl-fmt color-R color-G color-B) obj
    (sdl2:map-rgb sdl-fmt color-R color-G color-B)))

(declaim (inline snow-draw-live-cell))

(defun* snow-draw-live-cell ((id fixnum) color
                             (dim fixnum)
                             (screen-width fixnum) (surface-max-pos fixnum) pxs)
  "Draw in a rather fast way a live cell on the SDL surface.
   Hence, many params must be passed for avoiding a slow access to object properties."

  (declare (optimize (speed 3) (debug 0) (safety 0)))

  ; A snow cell is an hexagon with this shape
  ;
  ;          *
  ;         ***
  ;         ***
  ;          *
  (*let ((t1 fixnum (floor id dim))
         (offset-x fixnum (if (zerop (mod t1 2)) 1 0))
         (screen-x fixnum (+ offset-x (* 2 (mod id dim))))
         (screen-y fixnum (* 2 t1))
         (py1 fixnum (+ screen-x (the fixnum (* screen-width screen-y))))
         (py2 fixnum (+ py1 screen-width))
         (py3 fixnum (+ py2 screen-width))
         (py4 fixnum (+ py3 screen-width)))

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

(defgeneric* (next! -> :void) ((obj snow) &key (graphics  t))
  (:documentation "Advance the calculation of sdl-surface."))

(defgeneric* (end? -> :bool) ((obj snow))
  (:documentation "t if next! has reached a fixed point."))

(declaim (inline snow-do-on-neighbours-of))
(defun* (snow-do-on-neighbours-of -> :void) ((dim fixnum) (max-id fixnum) (id fixnum) (f function))
  "Execute f on all the neighbours of the cell."
  (declare (optimize (speed 3) (debug 0) (safety 0)))

  (*let (
         (gx fixnum (floor id dim))
         (o fixnum (if (zerop (mod gx 2)) 0 -1))
         (c-up fixnum (+ (- id dim) o))
         (c-down fixnum (+ id dim o))

         (id1 fixnum (1- id))
         (id2 fixnum (1+ id))
         (id3 fixnum c-up)
         (id4 fixnum (1+ c-up))
         (id5 fixnum c-down)
         (id6 fixnum (1+ c-down)))

   (flet* (( (ff -> :void) ((cell-id fixnum) &key ((x fixnum) 0) ((y fixnum) 0))
             (when (and (>= cell-id 0) (<= cell-id max-id))
                (funcall f cell-id :x x :y y))
             nil))

     (ff id1 :x -1 :y 0)
     (ff id2 :x 1 :y 0)
     (ff id3 :x 0 :y -1)
     (ff id4 :x 1 :y -1)
     (ff id5 :x 0 :y 1)
     (ff id6 :x 1 :y 1))

     nil))

;; --------------------------------------------------

(defclass snow-counter (snow)
  (
   (live-cells
    :documentation "The live-cells."
    :type (simple-array 'bit 1))

   (neighbours
    :documentation "Count the neighbours of every cell."
    :type (simple-array '(integer 0 6) 1))

   (new-births
    :documentation "New birthed cells to process."
    :type (array 'fixnum 1))

   (new-births-count
    :type fixnum
    :initform 0)

   (active-cells
    :documentation "Cells to monitor because theirs inputs from which they depend is changed."))

  (:documentation "Calculate snow using an array counting the neighbours."))

(declaim (inline snow-counter-add-new-birth))

(defun* (snow-counter-add-new-birth -> :void) ((obj snow-counter) (i fixnum))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-slots (max-id live-cells new-births new-births-count) obj
    (declare (fixnum max-id new-births-count))
    (declare (type (simple-array bit 1) live-cells))
    (declare (type (array fixnum 1) new-births))
    (when (and (>= i 0) (<= i max-id))
      (setf (aref live-cells i) 1)
      (setf (aref new-births new-births-count) i))
      (incf (the fixnum (slot-value obj 'new-births-count)))))

(defmethod initialize-instance :after ((obj snow-counter) &key)
  (with-slots (dim max-id center-id) obj
    (setf (slot-value obj 'live-cells)
          (make-array (1+ max-id)
                      :adjustable nil
                      :element-type 'bit
                      :initial-element 0))

    (setf (slot-value obj 'neighbours)
          (make-array (1+ max-id)
                      :adjustable nil
                      :element-type '(integer 0 6)
                      :initial-element 0))

    (setf (slot-value obj 'new-births)
          (make-array (floor max-id 8)
                      :adjustable t
                      :element-type 'fixnum))

    (setf (slot-value obj 'active-cells) (make-hash-table :size (floor max-id 10)))

    (snow-counter-add-new-birth obj center-id)))

(defun* (snow-counter-process-new-births -> :void) ((obj snow-counter) &key (graphics t))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-slots (new-births new-births-count
               active-cells
               dim max-id neighbours
               screen-width
               surface-max-pos
               sdl-surface) obj
    (declare (fixnum new-births-count dim max-id screen-width surface-max-pos))
    (declare (type (simple-array (integer 0 6) 1) neighbours))

    (iter (with color = (snow-current-color obj))
          (with pxs = (when graphics (sdl2:surface-pixels sdl-surface)))
          (for i from 0 below new-births-count)
          (for j = (aref new-births i))
          (declare (fixnum i j))
          (after-each
             (when graphics
               (snow-draw-live-cell j color
                                    dim
                                    screen-width surface-max-pos
                                    pxs))

             (snow-do-on-neighbours-of dim max-id j
               (lambda* ((k fixnum) &key ((x fixnum) 0) ((y fixnum) 0))
                        ; TODO investigate, sometime there are more than 6 neighbours
                        (*let ((v (integer 0 6) (the (integer 0 6) (aref neighbours k))))
                          (when (< v 6)
                            (setf (aref neighbours k) (the (integer 0 6) (1+ v)))
                            (setf (gethash k active-cells) nil)))
                        nil)))

           (finally
             (setf (slot-value obj 'new-births-count) 0)))))

(defun* (snow-counter-process-active-cells -> :void) ((obj snow-counter))
  (declare (optimize (speed 3) (debug 0) (safety 0)))

  (with-slots (live-cells active-cells neighbours) obj
    (declare (type (simple-array bit 1) live-cells))
    (declare (type (simple-array (integer 0 6) 1) neighbours))

    (iter
          (for (j _nil) in-hashtable active-cells)
          (for c = (the (integer 0 6) (aref neighbours j)))
          (declare (type (integer 0 6) c))
          (for new-born? = (and (oddp c) (zerop (aref live-cells j))))
          (when new-born? (snow-counter-add-new-birth obj j))
          (finally
             (clrhash active-cells)))))

(defmethod next! ((obj snow-counter) &key (graphics t))
    (declare (optimize (speed 3) (debug 0) (safety 0)))
    (snow-counter-process-new-births obj :graphics graphics)
    (snow-counter-process-active-cells obj)
    (snow-next-color! obj))

(defmethod end? ((obj snow-counter))
  (zerop (slot-value obj 'new-births-count)))

;; --------------------------------------------

(defclass snow-counter2 (snow-counter)
  (
    (rnd2
    :initform (random-state:make-generator :mersenne-twister-32 152)))

  (:documentation "Calculate snow using an array counting the neighbours."))

(defun* (snow-counter2-process-active-cells -> :void) ((obj snow-counter2))
  (with-slots (live-cells active-cells neighbours rnd2) obj
    (iter
          (for (j _nil) in-hashtable active-cells)
          (for c = (aref neighbours j))
          (for new-born? = (and (oddp c)
                                (not (= 1 (random-state:random-int rnd2 0 1024)))
                                (zerop (aref live-cells j))))
          (when new-born? (snow-counter-add-new-birth obj j))
          (finally
             (clrhash active-cells)))))

(defun* (snow-counter2-process-new-births -> :void) ((obj snow-counter2) &key (graphics t))
  (with-slots (new-births new-births-count
               active-cells
               dim max-id neighbours
               screen-width
               surface-max-pos
               sdl-surface) obj
    (iter (with color = (snow-current-color obj))
          (with pxs = (when graphics (sdl2:surface-pixels sdl-surface)))
          (for i from 0 below new-births-count)
          (for j = (aref new-births i))
          (after-each
             (when graphics
               (snow-draw-live-cell j color
                                    dim
                                    screen-width surface-max-pos
                                    pxs))

             (snow-do-on-neighbours-of dim max-id j
               (lambda (k &key (x 0) (y 0))
                 (incf (aref neighbours k))
                 (setf (gethash k active-cells) nil))))

           (finally
             (setf (slot-value obj 'new-births-count) 0)))))

(defmethod next! ((obj snow-counter2) &key (graphics t))
    (snow-counter2-process-new-births obj :graphics graphics)
    (snow-counter2-process-active-cells obj)
    (snow-next-color! obj))

;; --------------------------------------------

(defun main (&key (benchmark nil) (graphics t))
  "Main function"

  (sdl2:with-init (:everything)
    (multiple-value-bind (_1 w h _2)
      (sdl2:get-current-display-mode 0)

      (sdl2:with-window (win :title "Snow 0.1" :w w :h h :flags '(:shown :maximized))

      (multiple-value-bind (win-width win-height)
        (sdl2:get-window-size win)

      ; NOTE: erase the physical window, and not the "logical one".
      (with-rects
          ((rect1 0 0 win-width win-height))
        (let* ((srf (sdl2:get-window-surface win))
               (fmt (sdl2:surface-format srf)))
           (sdl2:fill-rect srf rect1 (sdl2:map-rgb fmt 0 0 0))))

        (let* ((dim (snow-screen-to-dim win-width win-height))
               (snow (make-instance 'snow-counter :dim dim))
               (frame-delay (real-time-pause-between-frames snow))
               (fps-scale (/ internal-time-units-per-second 1000))
               (time1 nil)
               (time2 nil))

      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
          (setf time1 (get-internal-real-time))
          (sdl2:blit-surface (slot-value snow 'sdl-surface) nil (sdl2:get-window-surface win) nil)
          (sdl2:update-window win)
          (cond
            ((end? snow)
             (when benchmark (sdl2:push-quit-event)))
            (t (next! snow :graphics graphics)))
          (setf time2 (get-internal-real-time))
          (unless benchmark (sdl2:delay (let* ((dt (- time2 time1))
                                               (remaining-pause (max 0 (- frame-delay dt))))
                                               (floor remaining-pause fps-scale))))))))))))

(main)

