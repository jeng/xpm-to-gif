;;;; Copyright © 2006 Jeremy English <jhe@jeremyenglish.org>
;;;;
;;;; Permission to use, copy, modify, distribute, and sell this software and its
;;;; documentation for any purpose is hereby granted without fee, provided that
;;;; the above copyright notice appear in all copies and that both that
;;;; copyright notice and this permission notice appear in supporting
;;;; documentation.  No representations are made about the suitability of this
;;;; software for any purpose.  It is provided "as is" without express or
;;;; implied warranty.
;;;;
;;;; Created: 18-December-2006

;;;; Code for working with parsed xpm data

(in-package :xpm-to-gif)

(defclass xpm-reader (xpm-parser)  ())

(defmethod pixel-key ((xpm xpm-reader) x y)
  (elt (data xpm) (+ (* y (width xpm)) x)))

(defmethod color ((xpm xpm-reader) key)
  (gethash key (color-table xpm)))

(defmethod mono ((xpm xpm-reader) key)
  (gethash key (mono-table xpm)))

(defmethod grayscale-four ((xpm xpm-reader) key)
  (gethash key (level-four-grayscale-table xpm)))

(defmethod grayscale ((xpm xpm-reader) key)
  (gethash key (grayscale-table xpm)))

(defmethod symbolic ((xpm xpm-reader) key)
  (gethash key (symbolic-table xpm)))

(defun color-type (color)
  (unless (> (length color) 0) (error "Invalid color"))
  (let ((lk (elt color 0)))
    (cond
      ((char= lk #\#) 'rgb)
      ((char= lk #\%) 'hsv)
      (t 'name))))

(defun parse-color-string (color type first-char)
  (unless (>= (length color) 7)
    (error (format nil "Invalid ~a color. Bad length" type)))
  (let ((lk (elt color 0)))
    (unless (char= lk first-char)
      (error
       (format nil "Invalid ~a color. First char is ~a not ~a."
               type lk first-char))))
  (values
   (read-from-string
    (format nil "#x~a" (subseq color 1 3)))
   (read-from-string
    (format nil "#x~a" (subseq color 3 5)))
   (read-from-string
    (format nil "#x~a" (subseq color 5 7)))))

(defun parse-rgb (color)
  (parse-color-string color "RGB" #\#))

(defun parse-hsv (color)
  (parse-color-string color "HSV" #\%))

;;Taken from:
;;http://www.fractalconcept.com/fcweb/download/examples.lisp
(defun hsv->rgb (h s v)
  "channels are in range [0..1]"
  (if (eql 0 s)
      (list v v v)
      (let* ((i (floor (* h 6)))
             (f (- (* h 6) i))
             (p (* v (- 1 s)))
             (q (* v (- 1 (* s f))))
             (t_ (* v (- 1 (* s (- 1 f)))))
             (hint (mod i 6)))
        (case hint
          (0 (values v t_ p))
          (1 (values q v p))
          (2 (values p v t_))
          (3 (values p q v))
          (4 (values t_ p v))
          (5 (values v p q))))))

(defun make-xpm-reader (file-name)
  (let ((xpm (make-instance 'xpm-reader)))
    (parse-xpm-file xpm file-name)
    xpm))
