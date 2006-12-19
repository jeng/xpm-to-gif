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
;;;;
;;;; Reference: http://koala.ilog.fr/ftp/pub/xpm/xpm-3-paper.ps.gz

(in-package :xpm-to-gif)

(defclass xpm-parser ()
  ((variable-name :accessor variable-name
                  :initarg :variable-name)
   (color-table :accessor color-table
                :initarg :color-table)
   (mono-table :accessor mono-table
                :initarg :mono-table)
   (symbolic-table :accessor symbolic-table
                   :initarg :symbolic-table)
   (level-four-grayscale-table :accessor level-four-grayscale-table
                               :initarg :level-four-grayscale-table)
   (grayscale-table :accessor grayscale-table
                    :initarg :grayscale-table)
   (data :accessor data
         :initarg :data)
   (width :accessor width
          :initarg :width)
   (height :accessor height
           :initarg :height)
   (number-colors :accessor number-colors
                  :initarg :number-colors)
   (characters-per-pixel :accessor characters-per-pixel
                        :initarg :characters-per-pixel)
   (x-hotspot :accessor x-hotspot
              :initarg :x-hotspot)
   (y-hotspot :accessor y-hotspot
              :initarg :y-hotspot))
  (:default-initargs
    :variable-name ""
    :color-table (make-hash-table :test #'equal)
    :mono-table (make-hash-table :test #'equal)
    :symbolic-table (make-hash-table :test #'equal)
    :level-four-grayscale-table (make-hash-table :test #'equal)
    :grayscale-table (make-hash-table :test #'equal)
    :data nil
    :width 0
    :height 0
    :number-colors 0
    :characters-per-pixel 0
    :x-hotspot nil
    :y-hotspot nil)
  (:documentation
   "Parse an xpm file. Color table is a hash that lets you look up the
rgb color for a character.  Data is a character stream of the raw xpm data"))

(defun expected (s)
  (error (format nil "~a expected" s)))

(defun match (reader x)
  (if (char= x (sym reader))
      (next reader)
      (expected (format nil "~a" x))))

(defun is-white (char)
  (if (characterp char)
      (char<= char #\space)
      nil))

(defun skip-white (reader)
  (loop while (is-white (sym reader))
        do (next reader)))

(defun xpm-number (reader)
  (if (digit-char-p (sym reader))
      (let ((w (collect-until
                reader
                #'(lambda (char)
                    (not
                     (digit-char-p char))))))
        (skip-white reader)
        (parse-integer w))
      (error (format nil "Invalid number. Symbol = ~s" (sym reader)))))

(defun xpm-color-name (reader)
  (if (or (alpha-char-p (sym reader))
          (char= (sym reader) #\_)
          (char= (sym reader) #\#)
          (char= (sym reader) #\%))
      (let ((w (collect-until
                reader
                #'(lambda (char)
                    (not
                     (or (alphanumericp char)
                         (char= char #\_)
                         (char= char #\#)
                         (char= char #\%)))))))
        (skip-white reader)
        w)
      (error (format nil "Invalid color name. Symbol = ~s" (sym reader)))))

(defun xpm-word (reader)
  (if (or (alpha-char-p (sym reader)) (char= (sym reader) #\_))
      (let ((w (collect-until
                reader
                #'(lambda (char)
                    (not
                     (or (alphanumericp char)
                         (char= char #\_)))))))
        (skip-white reader)
        w)
      (error (format nil "Invalid word. Symbol = ~s" (sym reader)))))

(defmethod xpm-comment ((xpm xpm-parser) reader)
  (match reader #\/)
  (match reader #\*)
  (collect-until
   reader
   #'(lambda (char) (char= char #\*)))
  (match reader #\*)
  (match reader #\/))

(defmethod xpm-declaration ((xpm xpm-parser) reader)
  (let ((char (xpm-word reader)))
    (if (string-equal char "char")
        (progn
          (match reader #\*)
          (skip-white reader)
          (let ((var (xpm-word reader)))
            (setf (variable-name xpm) var))
          (collect-until
           reader
           #'(lambda (char) (char= char #\{)))
          (match reader #\{))
        (error "Invalid xpm file. The declaration is wrong"))))

(defmethod xpm-values ((xpm xpm-parser) reader)
  (match reader #\")
  (skip-white reader)
  (setf (width xpm) (xpm-number reader))
  (setf (height xpm) (xpm-number reader))
  (setf (number-colors xpm) (xpm-number reader))
  (setf (characters-per-pixel xpm) (xpm-number reader))
  (unless (char= #\" (sym reader))
    (setf (x-hotspot xpm) (xpm-number reader)))
  (unless (char= #\" (sym reader))
    (setf (y-hotspot xpm) (xpm-number reader)))
  (match reader #\")
  (skip-white reader)
  (match reader #\,)
  (skip-white reader))

(defmethod pixel-reader ((xpm xpm-parser) reader)
  (let* ((idx 0)
         (chars
          (collect-until
           reader
           #'(lambda (char)
               (let ((b (= idx (characters-per-pixel xpm))))
                 (incf idx)
                 b)))))
    chars))

(defmethod parse-color-pair ((xpm xpm-parser) hash-key reader)
  (let* ((key (xpm-word reader))
         (color (xpm-color-name reader))
         (hash
          (cond
            ((string= key "c") (color-table xpm))
            ((string= key "s") (symbolic-table xpm))
            ((string= key "g4") (level-four-grayscale-table xpm))
            ((string= key "g") (grayscale-table xpm))
            ((string= key "m") (mono-table xpm))
            (t (error "Invalid color key.")))))
    (setf (gethash hash-key hash) color))
  (skip-white reader))

(defmethod parse-color-line ((xpm xpm-parser) reader)
  (match reader #\")
  (let* ((chars (pixel-reader xpm reader)))
    (format t "chars ~S" chars)
    (skip-white reader)
    (loop until (char= #\" (sym reader)) do
          (parse-color-pair xpm chars reader))
    (match reader #\")
    (skip-white reader)
    (match reader #\,)
    (skip-white reader)))

(defmethod xpm-colors ((xpm xpm-parser) reader)
  (loop for i from 1 to (number-colors xpm) do
        (when (char= (sym reader) #\/) (xpm-comment xpm reader))
        (parse-color-line xpm reader)))

(defmethod xpm-pixels ((xpm xpm-parser) reader)
  (match reader #\")
  (setf (data xpm) (make-array (1+ (* (width xpm) (height xpm)) )))
  (let ((idx 0))
    (loop until (null (sym reader)) do
          (if (char= (sym reader) #\")
              (progn
                (match reader #\")
                (skip-white reader)
                (if (char= (sym reader) #\})
                    (progn
                      (match reader #\})
                      (skip-white reader)
                      (match reader #\;))
                    (progn
                      (match reader #\,)
                      (skip-white reader)
                      (match reader #\"))))
              (let ((pixel (pixel-reader xpm reader)))
                (setf (elt (data xpm) idx) pixel)
                (incf idx))))
    (format t "idx = ~a" idx)))

(defmethod xpm-string ((xpm xpm-parser) reader)
  (xpm-values xpm reader)
  (when (char=  #\/ (sym reader)) (xpm-comment xpm reader))
  (xpm-colors xpm reader)
  (when (char=  #\/ (sym reader)) (xpm-comment xpm reader))
  (xpm-pixels xpm reader))

(defmethod parse-xpm-file ((xpm xpm-parser) file-name)
    (with-open-file (stream file-name)
      (let ((reader (define-stream-reader stream)))
        (loop until (null (sym reader)) do
              (skip-white reader)
              (cond
                ((char= (sym reader) #\/) (xpm-comment xpm reader))
                ((char= (sym reader) #\") (xpm-string xpm reader))
                (t
                 (let ((s (xpm-word reader)))
                   (if (string-equal s "static")
                       (xpm-declaration xpm reader)
                       (error "Invalid xpm file.")))))))))

