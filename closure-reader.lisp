;;;Copyright © 2006 Jeremy English <jhe@jeremyenglish.org>
;;;
;;;Permission to use, copy, modify, distribute, and sell this software and its
;;;documentation for any purpose is hereby granted without fee, provided that
;;;the above copyright notice appear in all copies and that both that
;;;copyright notice and this permission notice appear in supporting
;;;documentation.  No representations are made about the suitability of this
;;;software for any purpose.  It is provided "as is" without express or
;;;implied warranty.
;;;
;;;Created: 05-October-2006

(in-package :xpm-to-gif)

(defmacro define-reader (source reader peeker unreader)
  (let ((sym (gensym))
        (prev-sym (gensym)))
    `(let ((,prev-sym nil)
           (,sym (,reader ,source)))
      (list
       ;Get the current symbol
       (lambda () ,sym)
       ;Move to the next symbol
       (lambda ()
         (setf ,prev-sym ,sym)
         (setf ,sym (,reader ,source)))
       ;Return the source
       (lambda () ,source)
       ;Peek the source
       (lambda () (,peeker ,source))
       ;unread the source
       (lambda ()
         (when (null ,prev-sym) (error "No previous symbol found"))
         (setf ,sym ,prev-sym)
         (setf ,prev-sym nil)
         (,unreader ,sym ,source))))))

(defun define-list-reader (ls)
  (define-reader ls pop car push))

(defun define-stream-reader (stream)
  (define-reader stream read-char-end-of-file-nil peek-char unread-char))

(defun define-string-reader (string)
  (define-stream-reader
      (make-string-input-stream string)))

(defun read-char-end-of-file-nil (stream)
  "When reading end of file from a stream just return nil don't throw
an error."
  (handler-case (read-char stream)
    (end-of-file (c) nil)))

(defun sym (reader)
  (funcall (first reader)))

(defun next (reader)
  (funcall (second reader)))

(defun source (reader)
  (funcall (third reader)))

(defun peek (reader)
  (funcall (fourth reader)))

(defun undo-next (reader)
  (funcall (fifth reader)))

(defun collect-until (reader until-function)
  "Collect all characters into a string until the until-function
returns false or we run of characters"
  (coerce
   (loop until (or (null (sym reader))
                   (funcall until-function (sym reader)))
         collecting (sym reader) do
         (next reader)) 'string))