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

(defsystem xpm-to-gif
  :name "xpm-to-gif"
  :author "Jeremy English"
  :licence "BSD"
  :description "Converts and xpm file to a gif file."
  :depends-on (skippy)
  :serial t
  :components ((:file "package")
               (:file "closure-reader")
               (:file "x11-rgb")
               (:file "xpm-parser")
               (:file "xpm-reader")
               (:file "xpm-to-gif")))

