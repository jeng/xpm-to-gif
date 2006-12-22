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
              :initarg :y-hotspot)
   (string-state :accessor string-state
                 :initarg :string-state)
   (color-count :accessor color-count
                :initarg :color-count)
   (data-index :accessor data-index
               :initarg :data-index))
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
    :color-count 0
    :string-state 'values
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

;; Color names can be seperated by a space. This means we can not use
;; a simple single lookahead parser.
(defun xpm-color-name (reader)
  (if (or (alpha-char-p (sym reader))
          (char= (sym reader) #\_)
          (char= (sym reader) #\#)
          (char= (sym reader) #\%))
      (let ((name "")
            (prev-char #\0))
        (loop do
              (when (char= (sym reader) #\") (return))
              (when (is-white prev-char)
                (let ((c (sym reader)))
                  (when (or (char= c #\c)
                            (char= c #\s)
                            (char= c #\g)
                            (char= c #\m))
                    (next reader)
                    (if (or (char= (sym reader) #\space)
                            (and (char= c #\g) (char= (sym reader) #\4)))
                        (progn
                          (undo-next reader)
                          (return))
                        (undo-next reader)))))
              (setf prev-char (sym reader))
              (setf name (format nil "~a~a" name (sym reader)))
              (next reader))
        (string-trim '(#\space) name))
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
  (let ((old (sym reader)))
    (loop until (and (char= old #\*) (char= (sym reader) #\/)) do
          (setf old (sym reader))
          (next reader)))
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
  (match reader #\"))

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

(defmethod xpm-colors ((xpm xpm-parser) reader)
  (match reader #\")
  (let* ((chars (pixel-reader xpm reader)))
    (skip-white reader)
    (loop until (char= #\" (sym reader)) do
          (parse-color-pair xpm chars reader))
    (match reader #\")))

(defmethod xpm-pixels ((xpm xpm-parser) reader)
  (match reader #\")
  (loop until (char= (sym reader) #\") do
        (let ((pixel (pixel-reader xpm reader)))
          (setf (elt (data xpm) (data-index xpm)) pixel)
          (incf (data-index xpm))))
  (match reader #\"))

(defmethod xpm-string ((xpm xpm-parser) reader)
  (cond
    ((equal (string-state xpm) 'values)
     (xpm-values xpm reader)
     (setf (data xpm) (make-array (* (width xpm) (height xpm))))
     (setf (string-state xpm) 'colors)
     (setf (color-count xpm) (number-colors xpm)))
    ((equal (string-state xpm) 'colors)
     (xpm-colors xpm reader)
     (decf (color-count xpm))
     (when (zerop (color-count xpm))
       (setf (string-state xpm) 'pixel)
       (setf (data-index xpm) 0)))
    ((equal (string-state xpm) 'pixel)
     (xpm-pixels xpm reader))))

(defmethod xpm-main ((xpm xpm-parser) reader)
  (loop until (or (null (sym reader)) (char= (sym reader) #\})) do
        (skip-white reader)
        (cond
          ((char= (sym reader) #\/) (xpm-comment xpm reader))
          ((char= (sym reader) #\") (xpm-string xpm reader))
          ((char= (sym reader) #\,) (match reader #\,))
          (t
           (let ((s (xpm-word reader)))
             (if (string-equal s "static")
                 (xpm-declaration xpm reader)
                 (error "Invalid xpm file.")))))
        (skip-white reader)))

(defmethod parse-xpm-file ((xpm xpm-parser) file-name)
  (setf (string-state xpm) 'values)
    (with-open-file (stream file-name)
      (let ((reader (define-stream-reader stream)))
        (xpm-main xpm reader))))

(defmethod parse-xpm-string ((xpm xpm-parser) str)
  (let ((reader (define-string-reader str)))
    (xpm-main xpm reader)))

#|

(defmacro test-xpm-parser (str xpm-name reader-name &body body)
  `(let ((,xpm-name (make-instance 'xpm-parser))
         (,reader-name (define-string-reader ,str)))
    ,@body))

(defun test-xpm-declaration ()
  (test-xpm-parser (format nil "/* XPM */~%static char *foo[] = {") xpm reader
    (xpm-main xpm reader)
    (assert (string= (variable-name xpm) "foo"))))

(defun test-xpm-values ()
  (test-xpm-parser "\"50 100 5 2\"" xpm reader
    (xpm-string xpm reader)
    (assert (= (width xpm) 50))
    (assert (= (height xpm) 100))
    (assert (= (number-colors xpm) 5))
    (assert (= (characters-per-pixel xpm) 2))
    (assert (null (x-hotspot xpm)))
    (assert (null (y-hotspot xpm))))

  (test-xpm-parser "\"50 100 5 2 3 4\"" xpm reader
    (xpm-string xpm reader)
    (assert (= (width xpm) 50))
    (assert (= (height xpm) 100))
    (assert (= (number-colors xpm) 5))
    (assert (= (characters-per-pixel xpm) 2))
    (assert (= (x-hotspot xpm) 3))
    (assert (= (y-hotspot xpm) 4))))

(defun test-xpm-colors ()
  (test-xpm-parser
      (format nil "\"# c #456677 s yow \",~%/*funky stuff*/\"^ c #ffeeff m #eeeeee\"/* is here*/,~%/* MOre stuff here */\"+ c %213466\"~%\":      c sandy brown\"~%\"   s None  c None\"") xpm reader
    (setf (string-state xpm) 'colors)
    (setf (number-colors xpm) 5)
    (setf (color-count xpm) (number-colors xpm))
    (setf (characters-per-pixel xpm) 1)
    (xpm-main xpm reader)
    (assert (string= (gethash "#" (color-table xpm)) "#456677"))
    (assert (string= (gethash "#" (symbolic-table xpm)) "yow"))
    (assert (string= (gethash "^" (color-table xpm)) "#ffeeff"))
    (assert (string= (gethash "^" (mono-table xpm)) "#eeeeee"))
    (assert (string= (gethash "+" (color-table xpm)) "%213466"))
    (assert (string= (gethash ":" (color-table xpm)) "sandy brown"))
    (assert (string= (gethash " " (color-table xpm)) "None"))
    (assert (equal (string-state xpm) 'pixel))
    (assert (zerop (color-count xpm)))))

(defun test-xpm-file ()
  (test-xpm-parser
      (format nil
"/* XPM */
static char *noname[] = {
/* width height ncolors chars_per_pixel */
\"9 13 2 1\",
/* colors */
\"` c None    s ledbg\",
\"a c #CA1E1C s ledfg\",
/* pixels */
\"`````````\",
\"````aaaaa\",
\"```a````a\",
\"```a````a\",
\"``a````a`\",
\"``a````a`\",
\"`````````\",
\"`a````a``\",
\"`a````a``\",
\"a````a```\",
\"a````a```\",
\"aaaaa````\",
\"`````````\"
};") xpm reader
    (let ((data "`````````````aaaaa```a````a```a````a``a````a```a````a```````````a````a```a````a``a````a```a````a```aaaaa`````````````"))
      (xpm-main xpm reader)
      (print (variable-name xpm))
      (assert (string= (variable-name xpm) "noname"))
      (assert (= (width xpm) 9))
      (assert (= (height xpm) 13))
      (assert (= (number-colors xpm) 2))
      (assert (= (characters-per-pixel xpm) 1))
      (assert (string= (gethash "`" (color-table xpm)) "None"))
      (assert (string= (gethash "a" (color-table xpm)) "#CA1E1C"))
      (assert (string= (gethash "`" (symbolic-table xpm)) "ledbg"))
      (assert (string= (gethash "a" (symbolic-table xpm)) "ledfg"))
      (print (length (data xpm)))
      (print (* (width xpm) (height xpm)))
      (assert (= (length (data xpm)) (* (width xpm) (height xpm))))
      (loop for pixel across (data xpm)
            for i from 0 do
            (assert (string=
                     pixel
                     (format nil "~a" (elt data i))))))))

(defun test-xpm-comments ()
  (test-xpm-parser "/**/" xpm reader
    (xpm-main xpm reader))
  (test-xpm-parser "/********/" xpm reader
    (xpm-main xpm reader)))

|#
