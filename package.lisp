;;;; Copyright  2006 Jeremy English <jhe@jeremyenglish.org>
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

(defpackage "XPM-TO-GIF"
  (:documentation "Loads an xpm file to skippy canvas.")
  (:use "COMMON-LISP" "SKIPPY")
  (:export
   #:xpm-file-to-gif
   #:xpm-to-canvas
   #:xpm-file-to-canvas
   #:color
   #:mono
   #:grayscale-four
   #:grayscale
   #:symbolic
   #:color-type
   #:parse-rgb
   #:parse-hsv
   #:hsv->rgb
   #:make-xpm-reader
   #:x11-rgb-string))
