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

(in-package :xpm-to-gif)

(defun rgb-color-string (color)
  "Returns a color string if one is found for this color.  If the color
passed is none then none is returned.  If a color cannot be found we
return nil."
  (if (string-equal color "none")
      color
      (let* ((color-type (color-type color))
             (color-string
              (if (equal color-type 'rgb)
                  color
                  (if (equal color-type 'name)
                      (x11-rgb-string color)
                      nil))))
        color-string)))

(defun xpm-to-canvas (xpm color-table &key (none-color 0))
  "Takes an xpm reader and return a skippy canvas and color-table."
  (let ((canvas (make-canvas :height (height xpm) :width (width xpm))))
    (loop for y from 0 to (1- (height xpm)) do
          (loop for x from 0 to (1- (width xpm)) do
                (let ((color
                       (rgb-color-string (color xpm (pixel-key xpm x y)))))
                  (if (equal (color-type color) 'rgb)
                      (multiple-value-bind (r g b)
                          (parse-rgb color)
                        (let ((c-index
                               (ensure-color (rgb-color r g b)
                                             color-table)))
                          (setf (pixel-ref canvas x y) c-index)))
                    (when (string-equal color "none")
                      (let ((c-index
                             (ensure-color none-color
                                           color-table)))
                      (setf (pixel-ref canvas x y) c-index)))))))
    (values canvas color-table)))

(defun xpm-file-to-canvas (file-name color-table &key (none-color 0))
  "Takes a gif file name, color-table and a optional none-color.  The
none-color is the color you want to use for xpm colors set to None. A
skippy canvas is returned."
  (xpm-to-canvas
   (make-xpm-reader file-name) color-table :none-color none-color))

(defun xpm-file-to-gif (input-file output-file &key (none-color 0))
  "Takes the name of a gif input file, xpm output file and a optional
none-color. The none-color is the color you want to use for xpm colors
set to None."
  (multiple-value-bind (canvas color-table)
      (xpm-file-to-canvas input-file
                          (make-color-table)
                          :none-color none-color)
    (let ((data-stream
           (make-data-stream
            :color-table color-table
            :width (width canvas)
            :height (height canvas))))
      (add-image (canvas-image canvas) data-stream)
      (output-data-stream data-stream output-file))))

