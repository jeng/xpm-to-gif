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

(defun parse-rgb (color)
  (unless (= (length color) 7) (error "Invalid RGB color. Paid length"))
  (let ((lk (elt color 0)))
    (unless (char= lk #\#)
      (error
       (format nil "Invalid RGB color. First char is ~a not #." lk))))
  (values
   (read-from-string
    (format nil "#x~a" (subseq color 1 3)))
   (read-from-string
    (format nil "#x~a" (subseq color 3 5)))
   (read-from-string
    (format nil "#x~a" (subseq color 5 7)))))


(defun make-xpm-reader (file-name)
  (let ((xpm (make-instance 'xpm-reader)))
    (parse-xpm-file xpm file-name)
    xpm))
