;; Copyright (c) 2025, April & May
;; SPDX-License-Identifier: 0BSD

;; Pure-Lisp SVG renderer.

;; The source code can be separated to - major parts, splitted with #\Page
;; 1. Utilities and a partial implementation of LispWorks APIs
;; 2. CSS parser
;; 3. SVG `path` data parser
;; 4. Gradient painting server implementation
;; 5. Main SVG parser & renderer

;; Check README.md for usage and details.

(defpackage trivial-svg
  (:use #:cl #:string-case #:split-sequence)
  (:import-from #:alexandria #:clamp #:copy-hash-table #:lerp #:ensure-gethash #:with-unique-names #:when-let #:if-let)
  (:import-from #:serapeum #:push-end #:parse-float #:defalias)
  (:import-from #:uiop #:string-prefix-p)
  (:export
   rad-to-deg deg-to-rad hex-to-spec get-font-file
   css-parse-url css-parse-angel css-parse-color css-parse-length
   css-parse-a-number css-parse-transforms css-parse-numeric-color css-parse-all-angels-from-string
   css-parse-all-length-from-string css-parse-all-numbers-from-string
   create-renderer draw-svg-from-string)
  )

(in-package trivial-svg)


;; Utilities and a partial implementation of LispWorks APIs

(declaim (type double-float rad-to-deg-conversion-factor 2pi pi-by-2))

;; GP:2PI, GP:PI-BY-2
(defconstant 2pi (* pi 2))
(defconstant pi-by-2 (/ pi 2))

(defconstant rad-to-deg-conversion-factor (/ 180.0d0 pi)
  "Factor used to convert radiants to degrees by multiplication.")

(defun rad-to-deg (radians) (* radians rad-to-deg-conversion-factor))

(defun deg-to-rad (degree) (/ degree rad-to-deg-conversion-factor))

(defmacro with-nth (bindings list &body body)
  (with-unique-names (lst)
    `(let* ((,lst ,list))
       (symbol-macrolet
           ,(loop for i from 0
                  for var in bindings
                  collect `(,var (nth ,i ,lst)))
         ,@body))))

;; Why I'm prefer using CL's convention `NFOO` but not Scheme's `FOO!` XD...
(defun nmerge-tables (table &rest tables)
  "Merge values of hash-tables in TABLES into TABLE. TABLE will be modified.

From serapeum's `merge-tables!`"
  (declare (optimize (speed 3)))
  (reduce (lambda (ht1 ht2)
            (maphash (lambda (k v)
                       (setf (gethash k ht1) v))
                     ht2)
            ht1)
          tables
          :initial-value table))

;; HARLEQUIN-COMMON-LISP:STRING-TRIM-WHITESPACE
(defalias string-trim-whitespace #'serapeum:trim-whitespace)

;; LISPWORKS:STRING-APPEND
(defalias string-append #'serapeum:string+)

;; GRAPHICS-PORTS:RECTANGLE-BIND
(defmacro rectangle-bind ((x y w h) rect &body body)
  (with-unique-names (r)
    `(let* ((,r ,rect)
            (,x (first ,r))
            (,y (second ,r))
            (,w (third ,r))
            (,h (fourth ,r)))
       ,@body)))

;; Color

;; COLOR:MAKE-RGB, COLOR:MAKE-HSV
(defun make-rgb (r g b &optional (a 1.0))
  (vector :rgb r g b a))
(defun make-hsv (h s v &optional (a 1.0))
  (vector :hsv h s v a))

;; COLOR:ENSURE-RGB
(defun ensure-rgb (color)
  (case (aref color 0)
    (:hsv (let* ((h (rad-to-deg (mod (aref color 1) 2pi)))
                 (s (aref color 2))
                 (v (aref color 3))
                 (c (* s v))
                 (x (* c (- 1 (abs (1- (mod (/ h 60) 2))))))
                 (m (- v c))
                 (rp (cond ((or (and (<= 0 h) (< h 60))
                                (and (<= 300 h) (< h 360)))
                            c)
                           ((or (and (<= 60 h) (< h 120))
                                (and (<= 240 h) (< h 300)))
                            x)
                           (t 0)))
                 (gp (cond ((or (and (<= 0 h) (< h 60))
                                (and (<= 180 h) (< h 240)))
                            x)
                           ((and (<= 60 h) (< h 180))
                            c)
                           (t 0)))
                 (bp (cond ((or (and (<= 120 h) (< h 180))
                                (and (<= 300 h) (< h 360)))
                            x)
                           ((and (<= 180 h) (< h 300))
                            c)
                           (t 0))))
            (make-rgb (+ rp m) (+ gp m) (+ bp m) (aref color 4))))
    (t color)))

;; LW RGB color accessors
(defun color-red (color)
  (declare (inline color-red))
  (aref (ensure-rgb color) 1))
(defun color-green (color)
  (declare (inline color-green))
  (aref (ensure-rgb color) 2))
(defun color-blue (color)
  (declare (inline color-blue))
  (aref (ensure-rgb color) 3))
(defun color-alpha (color)
  (declare (inline color-alpha))
  (aref color 4))

(defun hex-to-spec (hex)
  "Convert hexdecimal color string to LW color-spec."
  (setq hex (string-trim '(#\# #\Space) hex))
  (let ((hex-list (case (length hex)
                    (3 (map 'list #'string hex))
                    (6 (loop for i from 0 to 4 by 2 collect (subseq hex i (+ 2 i))))))
        (deno (if (< (length hex) 6) 15.0 255.0)))
    (when hex-list
      (apply #'make-rgb
             (mapcar (lambda (str) (/ (parse-integer str :radix 16) deno))
                     hex-list)))))

;; Transform

;; Notes that these transforms are in List (conform with LW) but not
;; Vector (like in Vecto).

;; GRAPHICS-PORTS:MAKE-TRANSFORM
(defun make-transform (&optional (a 1) (b 0) (c 0) (d 1) (e 0) (f 0))
  (declare (inline make-transform))
  (list a b c d e f))

(defalias copy-transform #'copy-list)

;; Inspired by VECTO

(defun mult (m1 m2)
  (declare (inline mult))
  (destructuring-bind (a b c d e f)
      m1
    (destructuring-bind (a* b* c* d* e* f*)
        m2
      (make-transform (+ (* a a*)
                         (* b c*))
                      (+ (* a b*)
                         (* b d*))
                      (+ (* c a*)
                         (* d c*))
                      (+ (* c b*)
                         (* d d*))
                      (+ (* e a*)
                         (* f c*)
                         e*)
                      (+ (* e b*)
                         (* f d*)
                         f*)))))

;; GRAPHICS-PORTS:PREMULTIPLY-TRANSFORMS
(defun premultiply-transforms (transform1 transform2)
  (destructuring-bind (a b c d e f) transform2
    (destructuring-bind (a* b* c* d* e* f*) transform1
      (with-nth (a1 b1 c1 d1 e1 f1) transform1
        (setf a1 (+ (* a a*) (* b c*))
              b1 (+ (* a b*) (* b d*))
              c1 (+ (* c a*) (* d c*))
              d1 (+ (* c b*) (* d d*))
              e1 (+ (* e a*) (* f c*) e*)
              f1 (+ (* e b*) (* f d*) f*)))))
  transform1)

;; GRAPHICS-PORTS:POSTMULTIPLY-TRANSFORMS
(defun postmultiply-transforms (transform1 transform2)
  (destructuring-bind (a b c d e f) transform1
    (destructuring-bind (a* b* c* d* e* f*) transform2
      (with-nth (a1 b1 c1 d1 e1 f1) transform1
        (setf a1 (+ (* a a*) (* b c*))
              b1 (+ (* a b*) (* b d*))
              c1 (+ (* c a*) (* d c*))
              d1 (+ (* c b*) (* d d*))
              e1 (+ (* e a*) (* f c*) e*)
              f1 (+ (* e b*) (* f d*) f*)))))
  transform1)

;; GRAPHICS-PORTS:APPLY-SCALE, GRAPHICS-PORTS:APPLY-TRANSLATION,
;; GRAPHICS-PORTS:APPLY-ROTATION, GRAPHICS-PORTS:APPLY-ROTATION-AROUND-POINT
(defun apply-translation (transform dx dy)
  (declare (inline apply-translation))
  (postmultiply-transforms transform (make-transform 1 0 0 1 dx dy)))

(defun apply-scale (transform sx sy)
  (declare (inline apply-scale))
  (postmultiply-transforms transform (make-transform sx 0 0 sy 0 0)))

(defun apply-rotation (transform theta)
  (declare (inline apply-rotation))
  (postmultiply-transforms
   transform
   (let ((cos (cos theta))
         (sin (sin theta)))
    (make-transform cos sin (- sin) cos 0 0))))

(defun apply-rotation-around-point (transform theta x y)
  (declare (inline apply-rotation-around-point))
  (apply-translation transform (- x) (- y))
  (apply-rotation transform theta)
  (apply-translation transform x y))

;; Fonts

;; GP:GET-CHAR-WIDTH, GP:GET-FONT-HEIGHT, similar function using vecto & zpb-ttf
(defun get-char-width (char loader size)
  (let* ((scale (vecto::loader-font-scale size loader))
         (glyph (zpb-ttf:find-glyph char loader)))
    (* (zpb-ttf:advance-width glyph) scale)))

(defun get-char-height (char loader size)
  (let* ((scale (vecto::loader-font-scale size loader))
         (glyph (zpb-ttf:find-glyph char loader)))
    (* (zpb-ttf:advance-height glyph) scale)))

;; A small utility to find font file within system-installed fonts,
;; using font family name. Require fc-list on Linux.

(defvar *default-font-family*
  #+mswindows "C:/Windows/Fonts/Arial.ttf"
  #+darwin "/System/Library/Fonts/HelveticaNeue.ttc"
  #+(and unix (not darwin)) "Liberation Sans")

(defun get-font-file (&optional family-or-file)
  (if family-or-file 
    (when (or (not (or (pathname-directory family-or-file)
                       (pathname-type family-or-file)))
              (not (probe-file family-or-file)))
      #+mswindows
      (or (find-if (lambda (file)
                     (member (pathname-type file) '("ttf" "ttc" "otf" "otc")
                             :test #'string=))
                   (directory (make-pathname :name family-or-file :type :wild :defaults #P"C:/Windows/Fonts/")))
          *default-font-family*)
      #+darwin
      (or (find-if (lambda (file)
                     (member (pathname-type file) '("ttf" "ttc" "otf" "otc")
                             :test #'string=))
                   (directory (make-pathname :name family-or-file :type :wild :defaults #P"/System/Library/Fonts/")))
          *default-font-family*)
      #+(and unix (not darwin))
      (let ((fc-list (mapcar (lambda (str) (first (split-sequence #\: str)))
                             (split-sequence
                              #\Newline 
                              (with-output-to-string (*standard-output*)
                                (uiop:run-program "fc-list" :output t))))))
        (or (find (string-append "/" family-or-file ".") fc-list :test #'search)
            (find family-or-file fc-list :test #'search)
            (find *default-font-description* fc-list :test #'search)
            (error "Cannot find available font. Please give a pathname to :FAMILY"))))
    *default-font-family*))

(defun find-matching-font (family-or-file)
  (if family-or-file 
    (when (or (not (or (pathname-directory family-or-file)
                       (pathname-type family-or-file)))
              (not (probe-file family-or-file)))
      #+mswindows
      (find-if (lambda (file)
                 (member (pathname-type file) '("ttf" "ttc" "otf" "otc")
                         :test #'string=))
               (directory (make-pathname :name family-or-file :type :wild :defaults #P"C:/Windows/Fonts/")))
      #+darwin
      (find-if (lambda (file)
                 (member (pathname-type file) '("ttf" "ttc" "otf" "otc")
                         :test #'string=))
               (directory (make-pathname :name family-or-file :type :wild :defaults #P"/System/Library/Fonts/**/")))
      #+(and unix (not darwin))
      (let ((fc-list (mapcar (lambda (str) (first (split-sequence #\: str)))
                             (split-sequence
                              #\Newline 
                              (with-output-to-string (*standard-output*)
                                (uiop:run-program "fc-list" :output t))))))
        (or (find (string-append "/" family-or-file ".") fc-list :test #'search)
            (find family-or-file fc-list :test #'search))))
    *default-font-family*))


;; (Partial) CSS parser

(defun css-parse-a-number (str &optional (start 0))
  (declare (type vector str) (type fixnum start)
           ;(:explain :types)
           )
  "Parsing a CSS number gracefully out of STR, starting from START.

Return the first number it met, and the end position of this number.
Return NIL if there isn't a number at START."
  ;; This function will be used heavily, so should be as fast as it can...
  (check-type str string)
  (let ((char-arr (make-array 8 :element-type 'character :fill-pointer 0 :adjustable t))
        (i start)
        (len (length str))
        has-dot
        has-exp)
    (declare (type vector char-arr exp)
             (type fixnum i len))
    (tagbody
     start
     (if (= i len) (go end))
     (let ((c (char str i)))
       (cond ((member c '(#\+ #\-))
              (if (= i start)
                (vector-push-extend c char-arr)
                (go end)))
             ((char= c #\.)
              (if has-dot (go end)
                (progn
                  (setq has-dot t)
                  (vector-push-extend c char-arr))))
             ((char-equal c #\e)
              (if has-exp (go end)
                (progn
                  (setq has-exp t)
                  (vector-push-extend c char-arr))))
             ((digit-char-p c)
              (vector-push-extend c char-arr))
             (t (go end))))
     (setq i (1+ i))
     (go start)
     end)
    (if (find-if #'digit-char-p char-arr)
      (values (parse-float char-arr :type 'double-float) i)
      nil)))

(defun css-parse-all-numbers-from-string (str)
  "Parse all CSS format numbers in STR, return as a vector."
  (declare (optimize (safety 0))
           (type string str))
  (let ((i 0)
        (len (length str))
        (numbers (make-array 15 :element-type 'double-float :fill-pointer 0 :adjustable t)))
    (declare (type fixnum i len))
    (tagbody
     start
     (if (= i len) (go end) nil)
     (let ((c (char str i)))
       (if (or (digit-char-p c) (member c '(#\+ #\-)))
         (multiple-value-bind (num idx)
             (css-parse-a-number str i)
           (if num
             (progn
               (vector-push-extend num numbers)
               (setq i idx)
               (go start))
             (go end)))
         (progn
           (setq i (1+ i))
           (go start))))
     end)
    numbers))

;; CSS length-percentage

(defun css-parse-length (state str &optional (width-or-height :width) viewport-w viewport-h parent-w parent-h)
  "Parse a CSS format <length-percentage> to corresponding pixels,
based on current graphics port, CSS viewport and element's parent."
  ;; FIXME: Not fully tested
  (unless viewport-w (setq viewport-w (vecto::width state)))
  (unless viewport-h (setq viewport-h (vecto::height state)))
  (unless parent-w (setq parent-w viewport-w))
  (unless parent-h (setq parent-h viewport-h))
  (when str
    (if (alpha-char-p (char str 0)) 0
      (multiple-value-bind (len len-end) (css-parse-a-number str)
        (declare (type fixnum len-end viewport-w viewport-h parent-w parent-h)
                 (type double-float len))
        (let* ((unit (subseq str len-end))
               (font (vecto::font-loaders state))
               (size (vecto::size (vecto::font state))))
          (* len
             (string-case (unit)
               ;; abs https://www.w3.org/TR/css-values/#absolute-lengths
               ("cm"   37.79527559055118D0) ;(/ 96d0 2.54d0)
               ("mm"   3.7795275590551185D0) ;(/ 96d0 25.4d0)
               ("Q"    0.9448818897637794D0) ;(/ 96d0 2.54d0 40d0)
               ("in"   96d0)
               ("pt"   1.3333333333333333D0) ;(coerce 4/3 'double-float)
               ("pc"   16d0)
               ("px"   1d0)
               ;; rel https://www.w3.org/TR/css-values/#relative-lengths
               ("em"   size)
               ("rem"  size)
               ("ex"   (let ((bbox (vecto:string-bounding-box "x" size font)))
                         (+ (zpb-ttf:ymin bbox) (zpb-ttf:ymax bbox))))
               ("rex"  (let ((bbox (vecto:string-bounding-box "x" size font)))
                         (+ (zpb-ttf:ymin bbox) (zpb-ttf:ymax bbox))))
               ("cap"  (let ((bbox (vecto:string-bounding-box "O" size font)))
                         (+ (zpb-ttf:ymin bbox) (zpb-ttf:ymax bbox))))
               ("ch"   (let ((bbox (vecto:string-bounding-box "0" size font)))
                         (+ (zpb-ttf:xmin bbox) (zpb-ttf:xmax bbox))))
               ("rch"  (let ((bbox (vecto:string-bounding-box "0" size font)))
                         (+ (zpb-ttf:xmin bbox) (zpb-ttf:xmax bbox))))
               ("ic"   (let ((bbox (vecto:string-bounding-box "　" size font)))
                         (+ (zpb-ttf:xmin bbox) (zpb-ttf:xmax bbox))))
               ("ric"  (let ((bbox (vecto:string-bounding-box "　" size font)))
                         (+ (zpb-ttf:xmin bbox) (zpb-ttf:xmax bbox))))
               ;; FIXME: not precise value
               ("lh"   (let ((bbox (vecto:string-bounding-box "M" size font)))
                         (+ (zpb-ttf:ymin bbox) (zpb-ttf:ymax bbox))))
               ("rlh"  (let ((bbox (vecto:string-bounding-box "M" size font)))
                         (+ (zpb-ttf:ymin bbox) (zpb-ttf:ymax bbox))))
                 
               ("vw"   (/ viewport-w 100d0))
               ("vi"   (/ viewport-w 100d0))
               ("vh"   (/ viewport-h 100d0))
               ("vb"   (/ viewport-h 100d0))
               ("vmin" (min (/ viewport-w 100d0) (/ viewport-h 100d0)))
               ("vmax" (max (/ viewport-w 100d0) (/ viewport-h 100d0)))
               ("%"    (if (eq width-or-height :width)
                         (/ parent-w 100)
                         (/ parent-h 100)))
               (t      1d0))))))))

(defparameter *css-length-percentage-scanner*
  (ppcre:create-scanner
   "-?((\\d+(\\.\\d+)?)|(\\.\\d+))([eE]\\d+)?(?:cm|mm|Q|in|pt|pc|px|r?em|r?ex|cap|r?ch|r?ic|r?lh|vw|vi|vh|vb|vmin|vmax|%)?"))

(defun css-parse-all-length-from-string (state str &optional (width-or-height :width) viewport-w viewport-h parent-w parent-h)
  "Parse all CSS <length-percentage> from STR and convert them to pixels."
  (mapcar (lambda (sub)
            (css-parse-length state sub width-or-height viewport-w viewport-h parent-w parent-h))
          (ppcre:all-matches-as-strings *css-length-percentage-scanner* str)))

;; CSS color

(defvar *css-color-keywords* (make-hash-table :test #'equalp :size 149)
  "A map of CSS basic color keywords, from name to LW color spec.

https://www.w3.org/TR/css-color-3/#html4")

(dolist (i '(("aliceblue" "#F0F8FF")       ("antiquewhite" "#FAEBD7")      ("aqua" "#00FFFF")                 ("aquamarine" "#7FFFD4")
             ("azure" "#F0FFFF")           ("beige" "#F5F5DC")             ("bisque" "#FFE4C4")               ("black" "#000000")
             ("blanchedalmond" "#FFEBCD")  ("blue" "#0000FF")              ("blueviolet" "#8A2BE2")           ("brown" "#A52A2A")
             ("burlywood" "#DEB887")       ("cadetblue" "#5F9EA0")         ("chartreuse" "#7FFF00")           ("chocolate" "#D2691E")
             ("coral" "#FF7F50")           ("cornflowerblue" "#6495ED")    ("cornsilk" "#FFF8DC")             ("crimson" "#DC143C")
             ("cyan" "#00FFFF")            ("darkblue" "#00008B")          ("darkcyan" "#008B8B")             ("darkgoldenrod" "#B8860B")
             ("darkgray" "#A9A9A9")        ("darkgreen" "#006400")         ("darkgrey" "#A9A9A9")             ("darkkhaki" "#BDB76B")
             ("darkmagenta" "#8B008B")     ("darkolivegreen" "#556B2F")    ("darkorange" "#FF8C00")           ("darkorchid" "#9932CC")
             ("darkred" "#8B0000")         ("darksalmon" "#E9967A")        ("darkseagreen" "#8FBC8F")         ("darkslateblue" "#483D8B")
             ("darkslategray" "#2F4F4F")   ("darkslategrey" "#2F4F4F")     ("darkturquoise" "#00CED1")        ("darkviolet" "#9400D3")
             ("deeppink" "#FF1493")        ("deepskyblue" "#00BFFF")       ("dimgray" "#696969")              ("dimgrey" "#696969")
             ("dodgerblue" "#1E90FF")      ("firebrick" "#B22222")         ("floralwhite" "#FFFAF0")          ("forestgreen" "#228B22")
             ("fuchsia" "#FF00FF")         ("gainsboro" "#DCDCDC")         ("ghostwhite" "#F8F8FF")           ("gold" "#FFD700")
             ("goldenrod" "#DAA520")       ("gray" "#808080")              ("green" "#008000")                ("greenyellow" "#ADFF2F")
             ("grey" "#808080")            ("honeydew" "#F0FFF0")          ("hotpink" "#FF69B4")              ("indianred" "#CD5C5C")
             ("indigo" "#4B0082")          ("ivory" "#FFFFF0")             ("khaki" "#F0E68C")                ("lavender" "#E6E6FA")
             ("lavenderblush" "#FFF0F5")   ("lawngreen" "#7CFC00")         ("lemonchiffon" "#FFFACD")         ("lightblue" "#ADD8E6")
             ("lightcoral" "#F08080")      ("lightcyan" "#E0FFFF")         ("lightgoldenrodyellow" "#FAFAD2") ("lightgray" "#D3D3D3")
             ("lightgreen" "#90EE90")      ("lightgrey" "#D3D3D3")         ("lightpink" "#FFB6C1")            ("lightsalmon" "#FFA07A")
             ("lightseagreen" "#20B2AA")   ("lightskyblue" "#87CEFA")      ("lightslategray" "#778899")       ("lightslategrey" "#778899")
             ("lightsteelblue" "#B0C4DE")  ("lightyellow" "#FFFFE0")       ("lime" "#00FF00")                 ("limegreen" "#32CD32")
             ("linen" "#FAF0E6")           ("magenta" "#FF00FF")           ("maroon" "#800000")               ("mediumaquamarine" "#66CDAA")
             ("mediumblue" "#0000CD")      ("mediumorchid" "#BA55D3")      ("mediumpurple" "#9370DB")         ("mediumseagreen" "#3CB371")
             ("mediumslateblue" "#7B68EE") ("mediumspringgreen" "#00FA9A") ("mediumturquoise" "#48D1CC")      ("mediumvioletred" "#C71585")
             ("midnightblue" "#191970")    ("mintcream" "#F5FFFA")         ("mistyrose" "#FFE4E1")            ("moccasin" "#FFE4B5")
             ("navajowhite" "#FFDEAD")     ("navy" "#000080")              ("oldlace" "#FDF5E6")              ("olive" "#808000")
             ("olivedrab" "#6B8E23")       ("orange" "#FFA500")            ("orangered" "#FF4500")            ("orchid" "#DA70D6")
             ("palegoldenrod" "#EEE8AA")   ("palegreen" "#98FD98")         ("paleturquoise" "#AFEEEE")        ("palevioletred" "#DB7093")
             ("papayawhip" "#FFEFD5")      ("peachpuff" "#FFDAB9")         ("peru" "#CD853F")                 ("pink" "#FFC0CD")
             ("plum" "#DDA0DD")            ("powderblue" "#B0E0E6")        ("purple" "#800080")               ("red" "#FF0000")
             ("rosybrown" "#BC8F8F")       ("royalblue" "#4169E1")         ("saddlebrown" "#8B4513")          ("salmon" "#FA8072")
             ("sandybrown" "#F4A460")      ("seagreen" "#2E8B57")          ("seashell" "#FFF5EE")             ("sienna" "#A0522D")
             ("silver" "#C0C0C0")          ("skyblue" "#87CEEB")           ("slateblue" "#6A5ACD")            ("slategray" "#708090")
             ("slategrey" "#708090")       ("snow" "#FFFAFA")              ("springgreen" "#00FF7F")          ("steelblue" "#4682B4")
             ("tan" "#D2B48C")             ("teal" "#008080")              ("thistle" "#D8BFD8")              ("tomato" "#FF6347")
             ("turquoise" "#40E0D0")       ("saddlebrown" "#8B4513")       ("violet" "#EE82EE")               ("wheat" "#F5DEB3")
             ("white" "#FFFFFF")           ("whitesmoke" "#F5F5F5")        ("yellow" "#FFFF00")               ("yellowgreen" "#9ACD32")))
  (setf (gethash (first i) *css-color-keywords*)
        (hex-to-spec (second i))))

(setf (gethash "transparent" *css-color-keywords*) (make-rgb 0.0 0.0 0.0 0.0))

;; For scanning valid arguments of the numeric color function
;; e.g. rgb(255, 0, 0)
(defparameter *css-color-numeric-regexp* (ppcre:create-scanner "[\\d\\-\\.%]+"))

(defun css-parse-numeric-color (str)
  "Parse a CSS numerical color value to LW color spec.

https://www.w3.org/TR/css-color-3/#numerical"
  (let ((params (ppcre:all-matches-as-strings *css-color-numeric-regexp* str)))
    (flet ((parse-num (str)
             (let ((num (parse-integer str :junk-allowed t)))
               (if (eql (char str (1- (length str))) #\%)
                 (/ (clamp num 0 100) 100)
                 (/ (clamp num 0 255) 255)))))
      (let ((func (cond ((string-prefix-p "rgb" str) #'make-rgb)
                        ((string-prefix-p "hsl" str) #'make-hsv))))
        (destructuring-bind (x y z) (mapcar #'parse-num (subseq params 0 3))
          (if (= (length params) 4)
            (let ((alpha (clamp (parse-float (nth 3 params)) 0.0 1.0)))
              (funcall func x y z alpha))
            (funcall func x y z)))))))

(defun css-parse-color (str)
  "Parse a valid CSS color to LW color spec."
  (unless (or (null str) (member str '("none" "auto") :test #'equalp))
    (if (eql (char str 0) #\#)
      (hex-to-spec str)
      (if (or (string-prefix-p "rgb" str) (string-prefix-p "hsl" str))
        (css-parse-numeric-color str)
        (gethash str *css-color-keywords*)))))

;; CSS angle

(defun css-parse-angel (str &optional (start 0))
  "Parse a CSS <angel> to radians from START of the STR.

Return the radians and the end of parsing.

https://www.w3.org/TR/css3-values/#angles"
  (let ((len (length str)))
    (multiple-value-bind (num end-pos) (css-parse-a-number str start)
      (unless (null num)
        (cond ((search "grad" str :start2 end-pos :end2 (min len (+ end-pos 4)))
               (values (deg-to-rad (* num 0.9d0)) (+ end-pos 4)))
              ((search "rad" str :start2 end-pos :end2 (min len (+ end-pos 3)))
               (values num (+ end-pos 3)))
              ((search "turn" str :start2 end-pos :end2 (min len (+ end-pos 4)))
               (values (* num 2pi) (+ end-pos 4)))
              ((search "deg" str :start2 end-pos :end2 (min len (+ end-pos 3)))
               (values (deg-to-rad num) (+ end-pos 3)))
              (t (values (deg-to-rad num) end-pos)))))))

(defun css-parse-all-angels-from-string (str)
  "Parse a CSS <angel> to radians in STR, return as a vector"
  (let ((i 0)
        (len (length str))
        (numbers (make-array 15 :element-type 'double-float :fill-pointer 0 :adjustable t)))
    (declare (type fixnum i len))
    (tagbody
     start
     (if (= i len) (go end) nil)
     (let ((c (char str i)))
       (if (or (digit-char-p c) (member c '(#\+ #\-)))
         (multiple-value-bind (num idx)
             (css-parse-angel str i)
           (if num
             (progn
               (vector-push-extend num numbers)
               (setq i idx)
               (go start))
             (go end)))
         (progn
           (setq i (1+ i))
           (go start))))
     end)
    numbers))

;; CSS transform

(defparameter *transform-scanner*
  (ppcre:create-scanner "(matrix|scale(?:X|Y)?|translate(?:X|Y)?|rotate|skew(?:X|Y)?)\\((?:.|\\s)+?\\)")
  "Scanner for searching CSS <transform-function>s.

https://www.w3.org/TR/css-transforms-1/#transform-functions")

(defun css-parse-transforms (state str &optional viewport-w viewport-h parent-w parent-h)
  "Parse a CSS transform property value to a list of TRANSFORMs.

https://www.w3.org/TR/css-transforms-1/#transform-property"
  (let (transforms)
    (ppcre:do-scans (match-start match-end fname-starts fname-ends
                                 *transform-scanner* str)
      (declare (ignore match-start))
      (setq fname-starts (aref fname-starts 0)
            fname-ends (aref fname-ends 0))
      (let* ((fname (subseq str fname-starts fname-ends))
             (args-str (subseq str (1+ fname-ends) (1- match-end)))
             (args (cond ((member fname '("rotate" "skew" "skewX" "skewY") :test #'string=)
                          (css-parse-all-angels-from-string args-str))
                         ((member fname '("translate" "translateX" "translateY") :test #'string=)
                          (split-sequence-if (lambda (c) (member c '(#\, #\Space))) args-str
                                             :remove-empty-subseqs t))
                         (t (css-parse-all-numbers-from-string args-str)))))
        (if (string= fname "matrix")
          (push-end (apply #'make-transform (coerce args 'list)) transforms)
          (let ((trans (make-transform)))
            (string-case (fname)
              ("scale" (apply-scale trans (aref args 0) (aref args (if (= (length args) 1) 0 1))))
              ("scaleX" (apply-scale trans (aref args 0) 1))
              ("scaleY" (apply-scale trans 1 (aref args 0)))
              ("translate" (apply-translation
                            trans
                            (css-parse-length state (first args) :width viewport-w viewport-h parent-w parent-h)
                            (css-parse-length state (or (second args) (first args)) :height viewport-w viewport-h parent-w parent-h)))
              ("translateX" (apply-translation
                             trans
                             (css-parse-length state (first args) :width viewport-w viewport-h parent-w parent-h)
                             1))
              ("translateY" (apply-translation
                             trans 1
                             (css-parse-length state (first args) :height viewport-w viewport-h parent-w parent-h)))
              ("rotate" (apply-rotation trans (aref args 0)))
              ("skew"
               (setf (nth 1 trans) (tan (aref args 0)))
               (setf (nth 2 trans) (if (= (length args) 1) 0 (tan (aref args 0)))))
              ("skewX" (setf (nth 1 trans) (tan (aref args 0))))
              ("skewY" (setf (nth 2 trans) (tan (aref args 0)))))
            (push-end trans transforms)))))
    transforms))

;; CSS url

(defparameter *css-url-scanner*
  (ppcre:create-scanner "(?:url\\(\"(.+)\"\\))|(?:url\\('(.+)'\\))|(?:url\\((.+)\\))"))

(defun css-parse-url (str root-node)
  "Return the element targeted by t§he URL expression inside STR."
  (multiple-value-bind (whole arr)
      (ppcre:scan-to-strings *css-url-scanner* str)
    (declare (ignore whole))
    (let ((url (or (aref arr 0) (aref arr 1) (aref arr 2))))
      (if (or (null url) (not (eql (char url 0) #\#)))
        (error "LW-SVG only support ID url selector.")
        (plump-dom:get-element-by-id root-node (subseq url 1))))))

;; CSS `style` parser

(defun css-parse-style-properties (str)
  "Parse CSS style content STR to a hash-table"
  (setq str (string-trim-whitespace str))
  (let ((table (make-hash-table :test #'equalp)))
    (mapcar (lambda (str)
              (destructuring-bind (name val)
                  (mapcar #'string-trim-whitespace (split-sequence #\: str :remove-empty-subseqs t))
                (setf (gethash name table) val)))
            (mapcar #'string-trim-whitespace (split-sequence #\; str :remove-empty-subseqs t)))
    table))

(defmacro css-parse-class (node)
  `(split-sequence #\Space (plump:attribute ,node "class") :remove-empty-subseqs t))

(defparameter *css-class-name-scanner*
  (ppcre:create-scanner "[A-Za-z][A-Za-z0-9\\-_]*"))

(defparameter *css-id-scanner*
  (ppcre:create-scanner "[A-Za-z][A-Za-z0-9\\-_:\\.]*"))

(defparameter *css-attribute-selector-scanner*
  (ppcre:create-scanner "\\[([A-Za-z]+)((?:~|\\|)?=)?([A-Za-z]+)?\\]"))

(defun css-parse-a-selector (str)
  "Parse one CSS selector from a CSS selector list (separated by #\,)

Return a function that accept one argument PLUMP:NODE, which will
return a non-nil value if the node conforms the selector."
  (let ((index 0)
        (len (length str))
        funcs)
    (tagbody
     start
     (let ((first-char (char str index)))
       (case first-char
         (#\. (multiple-value-bind (start end)
                  (ppcre:scan *css-class-name-scanner* str :start index)
                (let ((cla (subseq str start end)))
                  (push (lambda (node) (member cla (css-parse-class node) :test #'string=)) funcs))
                (setq index end)))
         (#\# (multiple-value-bind (start end)
                  (ppcre:scan *css-id-scanner* str :start index)
                (let ((id (subseq str start end)))
                  (push (lambda (node) (equal id (plump:attribute node "id"))) funcs))
                (setq index end)))
         (#\[ (multiple-value-bind (start end rs re)
                  (ppcre:scan *css-attribute-selector-scanner* str :start index)
                (declare (ignore start))
                (let ((attr (subseq str (aref rs 0) (aref re 0)))
                      (op (when (aref rs 1)
                            (subseq str (aref rs 1) (aref re 1))))
                      (val (when (aref rs 2)
                             (string-trim '(#\") (subseq str (aref rs 2) (aref re 2))))))
                  (push
                   (if op
                     (case (char op 0)
                       (#\= (lambda (node) (equal (plump:attribute node attr) val)))
                       (#\~ (lambda (node)
                              (member val (split-sequence #\Space (plump:attribute node attr))
                                      :test #'equal)))
                       (#\| (lambda (node)
                              (let ((x (plump:attribute node attr)))
                                (or (equal x val)
                                    (and (stringp x)
                                         (string-prefix-p (string-append x "-") val)))))))
                     (lambda (node) (plump:attribute node attr)))
                   funcs))
                (setq index end)))
         (#\Space (let ((prev-func (pop funcs))
                        (sub-func (css-parse-a-selector (subseq str (1+ index)))))
                    (push
                     (lambda (node)
                       (and (loop for parent = (plump:parent node) then (plump:parent parent)
                                  until (or (plump:root-p parent) (null parent))
                                  thereis (funcall prev-func parent))
                            (funcall sub-func node)))
                     funcs)
                    (setq index len)))
         (t (if (alpha-char-p first-char)
              (let* ((end (or (position-if-not #'alpha-char-p str :start index) len))
                     (name (subseq str index end)))
                (push (lambda (node) (equal name (plump:tag-name node))) funcs)
                (setq index end))
              (setq index len)))))
     (if (< index len)
       (go start)))
    (lambda (node)
      (every (lambda (func) (funcall func node)) funcs))))

(defun css-parse-selectors (str)
  "Parse a CSS selector list.

Return a function that accept one argument PLUMP:NODE, which will
return a non-nil value if the node conforms the selector."
  (let ((selectors (mapcar #'css-parse-a-selector
                           (mapcar #'string-trim-whitespace
                                   (split-sequence #\, str :remove-empty-subseqs t)))))
    (lambda (node)
      (some (lambda (func) (funcall func node)) selectors))))

(defparameter *css-style-block-scanner*
  (ppcre:create-scanner "([^\\{\\}]+?)\\{([^\\{\\}]+?)\\}"))

(defun css-parse-style-element (node)
  "Giving a `<style>` element, return an alist which has the function
of the CSS selector as CAR, and a hash-table of corresponding
properties as CDR."
  (let ((str (string-trim-whitespace (plump:text node)))
        result)
    (ppcre:do-scans (ms me rs re *css-style-block-scanner* str)
      (when (every #'identity rs)
        (let ((selector (css-parse-selectors (subseq str (aref rs 0) (aref re 0))))
              (props (css-parse-style-properties (subseq str (aref rs 1) (aref re 1)))))
          (push (cons selector props) result))))
    result))


;; Deal with SVG path data

;; TODO: https://svgwg.org/svg2-draft/implnote.html#ArcCorrectionOutOfRangeRadii
(defun convert-svg-arc (x1 y1 rx ry fai fa fs x2 y2)
  "Conversion from endpoint to center parameterization.

Returns a list, the CAR is :ARC, the CDR conforms to the arglist of
VECTO::APPROXIMATE-ELLIPTICAL-ARC.

https://svgwg.org/svg2-draft/implnote.html#ArcImplementationNotes"
  (declare (optimize (float 0) (safety 0) (speed 3) (space 0))
           (type double-float x1 y1 rx ry x2 y2 fa fs fai)
           ;(:explain :non-floats)
           )
  (setq fai (* (/ fai 180.0) pi))
  (let* ((sinfai (sin fai))
         (-sinfai (- sinfai))
         (cosfai (cos fai))
         (x1-x2/2 (/ (- x1 x2) 2.0))
         (y1-y2/2 (/ (- y1 y2) 2.0))
         (x1p (+ (* x1-x2/2 cosfai) (* y1-y2/2 sinfai)))
         (y1p (+ (* x1-x2/2 -sinfai) (* y1-y2/2 cosfai)))
         (rx^ (* rx rx))
         (ry^ (* ry ry))
         (lamb (+ (/ (* x1p x1p) rx^) (/ (* y1p y1p) ry^))))
    (declare (type double-float rx ry rx^ ry^))
    (if (> lamb 1)
      ;; We add a very little constant to the enlarged radii,
      ;; to compensate the probable loss during float-point calculation,
      ;; ensure we always have a valid solution for RC
      (setq rx (+ (* rx (sqrt lamb)) 1d-6)
            ry (+ (* ry (sqrt lamb)) 1d-6)
            rx^ (* rx rx)
            ry^ (* ry ry)))
    (let* ((rc (let ((rc (sqrt
                          (- (/ (* rx^ ry^)
                                (+ (* rx^ y1p y1p) (* ry^ x1p x1p)))
                             1.0))))
                 (if (= fa fs) (- rc) rc)))
           (cxp (* rc (/ (* rx y1p) ry)))
           (cyp (* rc (- (/ (* ry x1p) rx))))
           (cx (+ (* cxp cosfai) (* cyp -sinfai) (/ (+ x1 x2) 2.0)))
           (cy (+ (* cxp sinfai) (* cyp cosfai)  (/ (+ y1 y2) 2.0))))
      (flet ((angle (ux uy vx vy)
               (declare (inline angle) (type double-float ux uy vx vy))
               (let ((arc (acos (/ (+ (* ux vx) (* uy vy))
                                   (* (sqrt (+ (* ux ux) (* uy uy)))
                                      (sqrt (+ (* vx vx) (* vy vy))))))))
                 (declare (type double-float arc))
                 (if (minusp (- (* ux vy) (* uy vx)))
                   (- arc) arc))))
        (let* ((vx (/ (- x1p cxp) rx))
               (vy (/ (- y1p cyp) ry))
               (start (angle 1.0d0 0.0d0 vx vy))
               (sweep (mod (angle vx vy (/ (- (- x1p) cxp) rx) (/ (- (- y1p) cyp) ry))
                           2pi)))
          (declare (type double-float start sweep))
          (if (= fs 0d0)
            (when (plusp sweep) (setq sweep (- sweep 2pi)))
            (when (minusp sweep) (setq sweep (- sweep 2pi))))
          ;; Modified for TRIVIAL-SVG
          (list :arc cx cy rx ry fai start (+ start sweep)))))))

(defstruct svg-path-command
  (char #\Null :type character)
  (args (make-array 8 :element-type 'double-float :fill-pointer 0 :adjustable t)
        :type (vector double-float)))

(defmacro do-svg-path-arguments ((args &rest variables) &body body)
  (let ((arg-count (length variables)))
    (with-unique-names (start index len)
      `(let ((,index 0)
             (,len (length ,args)))
         (declare (type fixnum start))
         (tagbody
          ,start
          (let ,(loop for i from 0
                      for var in variables
                      collect (list var `(aref ,args (+ ,index ,i))))
            (declare (type double-float ,@variables))
            ,@body)
          (setq ,index (the fixnum (+ ,index ,arg-count)))
          (when (> ,len ,index)
            (go ,start)))))))

;; https://www.w3.org/TR/2018/CR-SVG2-20181004/paths.html
(defun convert-path-commands (commands)
  "Convert a vector of parsed SVG <path> element data (see
SVG-PARSE-PATH-DATA below) to a vector of PATH that conform with
LispWorks GP:DRAW-PATH."
  (declare (type (vector svg-path-command) commands)
           (optimize (float 0) (safety 0) (speed 3) (debug 0))
           ;(:explain :non-floats :print-original-form)
           )
  (let ((cpx 0d0) (cpy 0d0)
        (ocx2 0d0) (ocy2 0d0)
        (oqx 0d0) (oqy 0d0)
        last-cubic-p
        last-quadratic-p
        last-move-p
        (path (make-array (length commands) :fill-pointer 0 :adjustable t)))
    (declare (type double-float cpx cpy ocx2 ocy2 oqx oqy))
    (macrolet ((push-path (data) `(vector-push-extend ,data path))
               (line-absolute (x y)
                 `(progn
                    (push-path (list :line ,x ,y))
                    (setq cpx ,x cpy ,y)))
               (line-relative (x y)
                 `(progn
                    (setq cpx (+ cpx ,x) cpy (+ cpy ,y))
                    (push-path (list :line cpx cpy)))))
      (dotimes (i (length commands))
        (let* ((command (aref commands i))
               (char (svg-path-command-char command))
               (args (svg-path-command-args command)))
          (case char
            (#\M
             (do-svg-path-arguments (args x y)
               (setq cpx x cpy y)
               (push-path (list (if last-move-p :line :move) x y))
               (setq last-move-p t)))
            (#\m
             (do-svg-path-arguments (args x y)
               (setq cpx (+ cpx x)
                     cpy (+ cpy y))
               (push-path (list (if last-move-p :line :move) cpx cpy))
               (setq last-move-p t)))
            ((or #\Z #\z)
             (push-path (list :close)))
            (#\L
             (do-svg-path-arguments (args x y)
               (line-absolute x y)))
            (#\l
             (do-svg-path-arguments (args x y)
               (line-relative x y)))
            (#\H
             (do-svg-path-arguments (args x)
               (line-absolute x cpy)))
            (#\V
             (do-svg-path-arguments (args y)
               (line-absolute cpx y)))
            (#\h
             (do-svg-path-arguments (args x)
               (line-relative x 0.0d0)))
            (#\v
             (do-svg-path-arguments (args y)
               (line-relative 0.0d0 y)))
            (#\C
             (do-svg-path-arguments (args cx1 cy1 cx2 cy2 nx ny)
               (setq cpx nx cpy ny
                     ocx2 cx2 ocy2 cy2)
               (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))))
            (#\c
             (do-svg-path-arguments (args rcx1 rcy1 rcx2 rcy2 rnx rny)
               (let ((cx1 (- rcx1 cpx)) (cy1 (- rcy1 cpy))
                     (cx2 (- rcx2 cpx)) (cy2 (- rcy2 cpy))
                     (nx (- rnx cpx)) (ny (- rny cpy)))
                 (setq cpx nx cpy ny
                       ocx2 cx2 ocy2 cy2)
                 (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny)))))
            (#\S
             (do-svg-path-arguments (args cx2 cy2 nx ny)
               (let ((cx1 (if last-cubic-p (- (* cpx 2.0d0) ocx2)
                            cpx))
                     (cy1 (if last-cubic-p (- (* cpy 2.0d0) ocy2)
                            cpy)))
                 (setq cpx nx cpy ny
                       ocx2 cx2 ocy2 cy2
                       last-cubic-p t)
                 (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny)))))
            (#\s
             (do-svg-path-arguments (args rcx2 rcy2 rnx rny)
               (let ((cx1 (if last-cubic-p (- (* cpx 2.0d0) ocx2)
                            cpx))
                     (cy1 (if last-cubic-p (- (* cpy 2.0d0) ocy2)
                            cpy))
                     (cx2 (+ cpx rcx2)) (cy2 (+ cpy rcy2))
                     (nx (+ cpx rnx)) (ny (+ cpy rny)))
                 (setq cpx nx cpy ny
                       ocx2 cx2 ocy2 cy2
                       last-cubic-p t)
                 (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny)))))
            (#\Q
             (do-svg-path-arguments (args qcx1 qcy1 nx ny)
               (let ((cx1 (+ cpx (/ (* (- qcx1 cpx) 2.0d0) 3.0d0)))
                     (cy1 (+ cpy (/ (* (- qcy1 cpy) 2.0d0) 3.0d0)))
                     (cx2 (+ nx (/ (* (- qcx1 nx) 2.0d0) 3.0d0)))
                     (cy2 (+ ny (/ (* (- qcy1 ny) 2.0d0) 3.0d0))))
                 (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))
                 (setq cpx nx cpy ny
                       oqx qcx1 oqy qcy1
                       last-quadratic-p t))))
            (#\q
             (do-svg-path-arguments (args rqcx1 rqcy1 rnx rny)
               (let ((qcx1 (+ cpx rqcx1))
                     (qcy1 (+ cpy rqcy1))
                     (nx (+ cpx rnx))
                     (ny (+ cpy rny)))
                 (let ((cx1 (+ cpx (/ (* (- qcx1 cpx) 2d0) 3d0)))
                       (cy1 (+ cpy (/ (* (- qcy1 cpy) 2d0) 3d0)))
                       (cx2 (+ nx (/ (* (- qcx1 nx) 2d0) 3d0)))
                       (cy2 (+ ny (/ (* (- qcy1 ny) 2d0) 3d0))))
                   (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))
                   (setq cpx (+ cpx rnx) cpy (+ cpy rny)
                         oqx qcx1 oqy qcy1
                         last-quadratic-p t)))))
            (#\T
             (do-svg-path-arguments (args nx ny)
               (if last-quadratic-p
                 (let ((qcx1 (- (* cpx 2d0) oqx))
                       (qcy1 (- (* cpy 2d0) oqy)))
                   (let ((cx1 (+ cpx (/ (* (- qcx1 cpx) 2d0) 3d0)))
                         (cy1 (+ cpy (/ (* (- qcy1 cpy) 2d0) 3d0)))
                         (cx2 (+ nx (/ (* (- qcx1 nx) 2d0) 3d0)))
                         (cy2 (+ ny (/ (* (- qcy1 ny) 2d0) 3d0))))
                     (setq oqx qcx1 oqy qcy1 last-quadratic-p t)
                     (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))))
                 (push-path (list :line nx ny)))
               (setq cpx nx cpy ny)))
            (#\t
             (do-svg-path-arguments (args rnx rny)
               (let ((nx (+ cpx rnx))
                     (ny (+ cpy rny)))
                 (if last-quadratic-p
                   (let ((qcx1 (- (* cpx 2d0) oqx))
                         (qcy1 (- (* cpy 2d0) oqy)))
                     (let ((cx1 (+ cpx (/ (* (- qcx1 cpx) 2d0) 3d0)))
                           (cy1 (+ cpy (/ (* (- qcy1 cpy) 2d0) 3d0)))
                           (cx2 (+ nx (/ (* (- qcx1 nx) 2d0) 3d0)))
                           (cy2 (+ ny (/ (* (- qcy1 ny) 2d0) 3d0))))
                       (setq oqx qcx1 oqy qcy1 last-quadratic-p t)
                       (push-path (list :bezier cx1 cy1 cx2 cy2 nx ny))))
                   (push-path (list :line nx ny)))
                 (setf cpx nx cpy ny))))
            (#\A (do-svg-path-arguments (args rx ry fai fa fs x2 y2)
                   (unless (and (= cpx x2) (= cpy y2))
                     (if (or (= rx 0d0) (= ry 0d0))
                       (line-absolute x2 y2)
                       (progn
                         (if (< rx 0d0) (setq rx (abs rx)))
                         (if (< ry 0d0) (setq ry (abs ry)))
                         (push-path (convert-svg-arc cpx cpy rx ry fai fa fs x2 y2))
                         (setq cpx x2 cpy y2))))))
            (#\a (do-svg-path-arguments (args rx ry fai fa fs x2 y2)
                   (unless (and (= cpx x2) (= cpy y2))
                     (if (or (= rx 0d0) (= ry 0d0))
                       (line-relative x2 y2)
                       (progn
                         (if (< rx 0d0) (setq rx (abs rx)))
                         (if (< ry 0d0) (setq ry (abs ry)))
                         (push-path (convert-svg-arc cpx cpy rx ry fai fa fs (+ cpx x2) (+ cpy y2)))
                         (setq cpx (+ cpx x2) cpy (+ cpy y2))))))))
          (setq last-cubic-p (member char '(#\C #\c #\S #\s))
                last-quadratic-p (member char '(#\Q #\q #\T #\t))
                last-move-p (member char '(#\M #\m)))))
      path)))

(defun svg-parse-path-data (data-string)
  "Parse the svg path `d` property from string to a vector of SVG-PATH-COMMAND for further parsing."
  (declare (type string data-string))
  (let ((commands (make-array 20 :element-type 'vector :fill-pointer 0 :adjustable t))
        current-command
        (i 0)
        (len (length data-string)))
    (declare (type fixnum i len))
    (flet ((start-new-command (command-char)
             (when current-command
               (vector-push-extend current-command commands))
             (setq current-command (make-svg-path-command :char command-char))))
      (tagbody
       start
       (if (= i len) (go end) nil)
       (let ((c (char data-string i)))
         (cond ((alpha-char-p c)
                (start-new-command c)
                (setq i (1+ i))
                (go start))
               ((or (digit-char-p c) (member c '(#\+ #\-)))
                (multiple-value-bind (num idx)
                    (css-parse-a-number data-string i)
                  (if num
                    (progn
                      (vector-push-extend num (svg-path-command-args current-command))
                      (setq i idx)
                      (go start))
                    (go end))))
               (t (setq i (1+ i))
                  (go start))))
       end)
      (start-new-command #\Null)
      commands)))


;; Gradient

(defclass svg-gradient ()
  ((stops :initform nil
          :accessor svg-gradient-stops)))

(defclass svg-linear-gradient (svg-gradient)
  ((x1 :type fixnum :initarg :x1)
   (y1 :type fixnum :initarg :y1)
   (x2 :type fixnum :initarg :x2)
   (y2 :type fixnum :initarg :y2)))

(defclass svg-radial-gradient (svg-gradient)
  ((cx :type fixnum :initarg :cx)
   (cy :type fixnum :initarg :cy)
   (r :type fixnum :initarg :r)
   (fx :type fixnum :initarg :fx)
   (fy :type fixnum :initarg :fy)
   (fr :type fixnum :initarg :fr)
   (mx :type fixnum :initarg :mx)
   (my :type fixnum :initarg :my)))

(defstruct svg-gradient-stop
  (offset 0d0 :type double-float)
  color
  opacity)

(defun svg-parse-gradient (node left top right bottom)
  (string-case  ((plump-dom:tag-name node))
    ("linearGradient" (svg-parse-linear-gradient node left top right bottom))
    ("radialGradient" (svg-parse-radial-gradient node left top right bottom))))

(defun svg-parse-linear-gradient (node left top right bottom)
  (declare (optimize (safety 0)))
  (flet ((parse-vector-value (node key start end)
           (when-let (str (gethash key (plump:attributes node)))
             (let ((len (length str)))
               (if (eql #\% (char str (1- len)))
                 (+ start
                    (round (* (- end start)
                              (/ (parse-float str :end (1- len)) 100d0))))
                 (parse-integer str)))))
         (parse-stop-value (node key)
           (when-let (str (gethash key (plump:attributes node)))
             (let ((len (length str)))
               (if (eql #\% (char str (1- len)))
                 (/ (parse-float str :end (1- len)) 100d0)
                 (parse-integer str))))))
    (let ((grad (make-instance 'svg-linear-gradient
                               :x1 (or (parse-vector-value node "x1" left right) (round left))
                               :y1 (or (parse-vector-value node "y1" top bottom) (round top))
                               :x2 (or (parse-vector-value node "x2" left right) (round right))
                               :y2 (or (parse-vector-value node "y2" top bottom) (round top))))
          (stops (plump-dom:get-elements-by-tag-name node "stop")))
      (dolist (stop stops)
        (push (make-svg-gradient-stop
               :offset (or (parse-stop-value stop "offset") 0d0)
               :color (or (css-parse-color (gethash "stop-color" (plump:attributes stop))) :black)
               :opacity (or (parse-stop-value stop "stop-opacity") 1d0))
              (svg-gradient-stops grad)))
      (setf (svg-gradient-stops grad)
            (sort (svg-gradient-stops grad)
                  (lambda (grad1 grad2)
                    (< (svg-gradient-stop-offset grad1) (svg-gradient-stop-offset grad2)))))
      (let ((start (copy-svg-gradient-stop
                    (first (svg-gradient-stops grad))))
            (end (copy-svg-gradient-stop
                  (car (last (svg-gradient-stops grad))))))
        (when (/= (svg-gradient-stop-offset start) 0d0)
          (setf (svg-gradient-stop-offset start) 0d0)
          (push start (svg-gradient-stops grad)))
        (when (/= (svg-gradient-stop-offset end) 1d0)
          (setf (svg-gradient-stop-offset end) 1d0)
          (push-end end (svg-gradient-stops grad))))
      grad)))

(defun svg-parse-radial-gradient (node left top right bottom)
  (declare (optimize (safety 0)))
  (flet ((parse-vector-value (node key start end)
           (when-let (str (gethash key (plump:attributes node)))
             (let ((len (length str)))
               (if (eql #\% (char str (1- len)))
                 (+ start
                    (round (* (- end start)
                              (/ (parse-float str :end (1- len)) 100d0))))
                 (parse-integer str)))))
         (parse-stop-value (node key)
           (when-let (str (gethash key (plump:attributes node)))
             (let ((len (length str)))
               (if (eql #\% (char str (1- len)))
                 (/ (parse-float str :end (1- len)) 100d0)
                 (parse-integer str))))))
    (let* ((cx (or (parse-vector-value node "cx" left right) left))
           (cy (or (parse-vector-value node "cy" top bottom) top))
           (r (or (parse-vector-value node "r" 0 (- right left)) (round (- right left) 2)))
           (fx (or (parse-vector-value node "fx" left right) cx))
           (fy (or (parse-vector-value node "fy" top bottom) cy))
           (fr (or (parse-vector-value node "fr" left right) 0))
           ;; 设大圆为Cr，圆心为R；小圆为Cf，圆心为F。在Cr上任取一点M=(x1, y1)，Cf上任取一点N=(x2, y2)，
           ;; 使MR平行于NF。同理取得点P, Q。求直线MN与直线PQ的交点为(mx, my)
           ;; https://www.cnblogs.com/lingge1992/p/6738487.html
           (x1 cx)
           (y1 (+ cy r))
           (x2 fx)
           (y2 (+ fy fr))
           (x3 (+ cx r))
           (y3 cy)
           (x4 (+ fx fr))
           (y4 fy)
           (a1 (- y2 y1))
           (b1 (- x1 x2))
           (c1 (- (* x2 y1) (* y2 x1)))
           (a2 (- y4 y3))
           (b2 (- x3 x4))
           (c2 (- (* x4 y3) (* y4 x3)))
           (d (- (* a1 b2) (* a2 b1)))
           (mx (if (zerop d) cx (round (/ (- (* b1 c2) (* b2 c1)) d))))
           (my (if (zerop d) cy (round (/ (- (* c1 a2) (* c2 a1)) d))))
           (grad (make-instance 'svg-radial-gradient
                                :cx cx :cy cy :r r :fx fx :fy fy :fr fr :mx mx :my my))
           (stops (plump-dom:get-elements-by-tag-name node "stop")))
      (declare (type fixnum cx cy r fx fy fr))
      (dolist (stop stops)
        (push (make-svg-gradient-stop
               :offset (or (parse-stop-value stop "offset") 0d0)
               :color (or (css-parse-color (gethash "stop-color" (plump:attributes stop))) :black)
               :opacity (or (parse-stop-value stop "stop-opacity") 1d0))
              (svg-gradient-stops grad)))
      (setf (svg-gradient-stops grad)
            (sort (svg-gradient-stops grad)
                  (lambda (grad1 grad2)
                    (< (svg-gradient-stop-offset grad1) (svg-gradient-stop-offset grad2)))))
      (let ((start (copy-svg-gradient-stop
                    (first (svg-gradient-stops grad))))
            (end (copy-svg-gradient-stop
                  (car (last (svg-gradient-stops grad))))))
        (when (/= (svg-gradient-stop-offset start) 0d0)
          (setf (svg-gradient-stop-offset start) 0d0)
          (push start (svg-gradient-stops grad)))
        (when (/= (svg-gradient-stop-offset end) 1d0)
          (setf (svg-gradient-stop-offset end) 1d0)
          (push-end end (svg-gradient-stops grad))))
      grad)))

(defgeneric svg-gradient-color (x y grad)
  (:method (x y (grad svg-linear-gradient))
   (declare (optimize (safety 0) (float 0))
            (type fixnum x y))
   (let* ((x1 (slot-value grad 'x1))
          (y1 (slot-value grad 'y1))
          (x2 (slot-value grad 'x2))
          (y2 (slot-value grad 'y2))
          (v1x (- x1 x))
          (v1y (- y1 y))
          (v2x (- x1 x2))
          (v2y (- y1 y2))
          (v2len (sqrt (+ (* v2x v2x) (* v2y v2y))))
          (percentage (/ (+ (* v1x v2x) (* v1y v2y)) v2len v2len))
          (stops (svg-gradient-stops grad))
          before after)
     (declare (type fixnum x1 y1 x2 y2))
     (loop for prev-stop = (first stops) then stop
           for stop in (cdr stops)
           until (<= (svg-gradient-stop-offset prev-stop)
                     percentage
                     (svg-gradient-stop-offset stop))
           finally (setq before prev-stop
                         after stop))
     (let ((ratio (/ (- percentage (svg-gradient-stop-offset before))
                     (- (svg-gradient-stop-offset after) (svg-gradient-stop-offset before)))))
       (flet ((lerp-color (func)
                (lerp ratio
                      (funcall func (svg-gradient-stop-color before))
                      (funcall func (svg-gradient-stop-color after)))))
         (make-rgb
          (lerp-color #'color-red) (lerp-color #'color-green) (lerp-color #'color-blue)
          (lerp ratio (svg-gradient-stop-opacity before) (svg-gradient-stop-opacity after)))))))
  
  (:method (x y (grad svg-radial-gradient))
   (declare (optimize (safety 0) (float 0))
            (type fixnum x y))
   (let* ((cx (slot-value grad 'cx))
          (cy (slot-value grad 'cy))
          (r (slot-value grad 'r))
          (fx (slot-value grad 'fx))
          (fy (slot-value grad 'fy))
          (fr (slot-value grad 'fr))
          (mx (slot-value grad 'mx))
          (my (slot-value grad 'my))
          (stops (svg-gradient-stops grad)))
     (declare (type fixnum cx cy r fx fy fr mx my))
     ;; 设大圆为Cr，圆心为R；小圆为Cf，圆心为F；已知点M(mx, my)，N(x, y)， 射线MN交Cf于点P, 交Cr于点Q, 求PN:PQ
     ;; 首先解三角形MFP得到MP，根据余弦定理，PF²=MF²+MP²-2MF⋅MP⋅cos∠PMF；
     ;; 使用向量点积法求得cos∠PMF，代入等式并解二元一次方程得到线段MP的长
     ;; 根据定义知PF平行于QR, 可知三角形PMF与三角形QMR相似，因而可得MF:MR=MP:MQ，进而求得线段MQ的长
     ;; 根据勾股定理求得MN，PN:PQ=(MN-MP):(MQ-MP)，计算完毕
     (cond ((<= (sqrt (+ (* (- x fx) (- x fx)) (* (- y fy) (- y fy)))) fr)
            (svg-gradient-stop-color (first stops)))
           ((>= (sqrt (+ (* (- x cx) (- x cx)) (* (- y cy) (- y cy)))) r)
            (svg-gradient-stop-color (car (last stops))))
           (t
            (let* ((vMFx (- fx mx))
                   (vMFy (- fy my))
                   (lenMF (float (sqrt (+ (* vMFx vMFx) (* vMFy vMFy))) 0d0))
                   (vMNx (- x mx))
                   (vMNy (- y my))
                   (lenMN (float (sqrt (+ (* vMNx vMNx) (* vMNy vMNy))) 0d0))
                   (MFMN (* lenMF lenMN))
                   (cosR (if (zerop MFMN) 0d0
                           (/ (+ (* vMNx vMFx) (* vMNy vMFy))
                              MFMN)))
                   (lenMP (if (zerop MFMN) 0d0
                                 (/ (+ (* 2d0 lenMF cosR) (sqrt (+ (* 4d0 (* fr fr)) (* cosR cosR))))
                                    2d0)))
                   (lenMQ (if (zerop MFMN) (float r 0d0)
                                 (* (/ lenMP fr) r)))
                   (percentage (clamp (/ (- lenMN lenMP) (- lenMQ lenMP)) 0d0 1d0))
                   before after)
              (declare (type fixnum vMFx vMFy vMNx vMNy)
                       (type double-float cosR lenMP lenMQ))
              (loop for prev-stop = (first stops) then stop
                    for stop in (cdr stops)
                    until (<= (svg-gradient-stop-offset prev-stop)
                              percentage
                              (svg-gradient-stop-offset stop))
                    finally (setq before prev-stop
                                  after stop))
              (let ((ratio (/ (- percentage (svg-gradient-stop-offset before))
                              (- (svg-gradient-stop-offset after) (svg-gradient-stop-offset before)))))
                (flet ((lerp-color (func)
                         (lerp ratio
                               (funcall func (svg-gradient-stop-color before))
                               (funcall func (svg-gradient-stop-color after)))))
                  (make-rgb
                   (lerp-color #'color-red) (lerp-color #'color-green) (lerp-color #'color-blue)
                   (lerp ratio (svg-gradient-stop-opacity before) (svg-gradient-stop-opacity after)))))))))))


;; SVG render

(defvar *svg-presentation-attributes*
  '("x"                            "y"                          "cx"                          "cy"
    "r"                            "rx"                         "ry"                          "width"
    "height"                       "d"                          "fill"                        "transform"
    "alignment-baseline"           "baseline-shift"             "clip-path"                   "clip-rule"
    "color"                        "color-interpolation"        "color-interpolation-filters" "color-rendering"
    "cursor"                       "direction"                  "display"                     "dominant-baseline"
    "fill-opacity"                 "fill-rule"                  "filter"                      "flood-color"
    "flood-opacity"                "font-family"                "font-size"                   "font-size-adjust"
    "font-stretch"                 "font-style"                 "font-variant"                "font-weight"
    "glyph-orientation-horizontal" "glyph-orientation-vertical" "image-rendering"             "letter-spacing"
    "lighting-color"               "marker-end"                 "marker-mid"                  "marker-start"
    "mask"                         "opacity"                    "overflow"                    "paint-order"
    "pointer-events"               "shape-rendering"            "stop-color"                  "stop-opacity"
    "stroke"                       "stroke-dasharray"           "stroke-dashoffset"           "stroke-linecap"
    "stroke-linejoin"              "stroke-miterlimit"          "stroke-opacity"              "stroke-width"
    "text-anchor"                  "text-decoration"            "text-overflow"               "text-rendering"
    "unicode-bidi"                 "vector-effect"              "visibility"                  "white-space"
    "word-spacing"                 "writing-mode"               ))

(defun draw-lw-path-to-vecto-state (state lw-path)
  (flet ((draw-sub-path (p)
           (case (first p)
             (:move (apply #'vecto::%move-to state (rest p)))
             (:line (apply #'vecto::%line-to state (rest p)))
             (:bezier (apply #'vecto::%curve-to state (rest p)))
             (:arc
              (destructuring-bind (cx cy rx ry fai start sweep) (rest p)
                (let ((curves (if (> start sweep)
                                (nreverse (mapcar #'nreverse
                                                  (vecto::approximate-elliptical-arc cx cy rx ry fai sweep start)))
                                (vecto::approximate-elliptical-arc cx cy rx ry fai start sweep))))
                  (destructuring-bind (((startx . starty) &rest ignored-curve)
                                       &rest ignored-curves)
                      curves
                    (declare (ignore ignored-curve ignored-curves))
                    (if (vecto::path state)
                      (vecto::%line-to state startx starty)
                      (vecto::%move-to state startx starty)))
                  (loop for (nil
                             (cx1 . cy1)
                             (cx2 . cy2)
                             (x2 . y2)) in curves
                        do (vecto::%curve-to state cx1 cy1 cx2 cy2 x2 y2))))))))
    (if (listp lw-path)
      (dolist (p lw-path) (draw-sub-path p))
      (loop for p across lw-path do (draw-sub-path p)))))

(defun lw-color-to-vecto (color)
  (make-instance 'vecto::rgba-color
                 :red (color-red color)
                 :green (color-green color)
                 :blue (color-blue color)
                 :alpha (color-alpha color)))

(defvar *css-collapse-whitespace-scanner*
  (ppcre:create-scanner "\\s{2,}"))

(defun create-renderer (state node &optional (root-node node) (container-attributes (make-hash-table :test #'equalp)))
  "Compile a SVG DOM element into a \"renderer\" function, which can
draw the SVG to the specified STATE.

STATE: a graphics-state that is used to provide necessary display
informations to compile the SVG.

Note that the returned function can be used to draw the SVG at any
graphics state, but the graphics informations, i.e. fonts, relative
size, screen DPI, will be embedded using the attributes from this
STATE.

ROOT-NODE: The root node of the SVG DOM, in PLUMP:NODE, which can
be NODE or its parent.  It is used to search for DOM elements being
referenced by SVG, to build re-used graphic objects.

NODE: the PLUMP:NODE of the SVG element being compiled.

CONTAINER-ATTRIBUTES: A hash-table of inherit attributes that are used
in recursive parsing. Not needed when parsing a standalone SVG
element."
  (declare (optimize (float 0) (safety 0) (speed 3))
           (type hash-table container-attributes)
           ;(:explain :types)
           )
  (let ((tag (plump-dom:tag-name node))
        (new-attrs (plump-dom:attributes node))
        (first-call-p (zerop (hash-table-count container-attributes))))
    (declare (type string tag))
    ;; Processing CSS style properties and merge them into NEW-ATTRS
    (let ((styles (ensure-gethash
                   "css-styles" container-attributes
                   (mapcan #'css-parse-style-element
                           (plump-dom:get-elements-by-tag-name root-node "style")))))
      ;; There're some problems here, so it only conforms SVG 1.1 but not 2
      (loop for (pred . attrs) in styles
            when (funcall pred node)
              do (nmerge-tables new-attrs attrs))
      (when-let (inline-style (plump-dom:attribute node "style"))
        (nmerge-tables new-attrs (css-parse-style-properties inline-style))))
    (labels (;; Helper functions & Getter of presentation attributes
             (svg-parse-length (val width-or-height)
               (if (stringp val)
                 (rectangle-bind (x y w h)
                     (gethash "viewBox" container-attributes)
                   (declare (ignore x y))
                   (css-parse-length
                    state val width-or-height w h
                    (gethash "width" container-attributes)
                    (gethash "height" container-attributes)))
                 val))
             (svg-parse-all-length (val width-or-height)
               (if (stringp val)
                 (rectangle-bind (x y w h)
                     (gethash "viewBox" container-attributes)
                   (declare (ignore x y))
                   (css-parse-all-length-from-string
                    state val width-or-height w h
                    (gethash "width" container-attributes)
                    (gethash "height" container-attributes)))
                 val))
             (parse-opacity (val)
               (if (stringp val) (clamp (parse-float val) 0.0 1.0) (if val 1.0 nil)))
             (with-alpha (color alpha)
               (when color
                 (setq color (ensure-rgb color))
                 (make-rgb (color-red color)
                           (color-green color)
                           (color-blue color)
                           (* (color-alpha color) alpha))))
             (get-attr (key &optional default)
               (declare (inline get-attr))
               (gethash key new-attrs
                        (gethash key container-attributes
                                 default)))
             (get-fill ()
               (declare (inline get-fill))
               (when-let (fill (get-attr "fill"))
                 (string-case (fill)
                   ("context-fill"
                    (setq fill (gethash "fill" container-attributes)))
                   ("context-stroke"
                    (setq fill (gethash "stroke" container-attributes)))
                   (t nil))
                 (setq fill
                       (cond ((member fill '(nil "auto") :test #'equalp) (make-rgb 0.0 0.0 0.0 1.0))
                             ((string-equal fill "none") nil)
                             ((search "url" fill) (css-parse-url fill root-node))
                             (t (css-parse-color fill))))
                 (when-let (op (or (parse-opacity (gethash "fill-opacity" container-attributes))
                                   (parse-opacity (gethash "opacity" container-attributes))))
                   (setq fill (with-alpha fill op)))
                 (when-let (op (or (parse-opacity (gethash "fill-opacity" new-attrs))
                                   (parse-opacity (gethash "opacity" new-attrs))))
                   (setq fill (with-alpha fill op)))
                 fill))
             (get-fill-rule ()
               (declare (inline get-fill-rule))
               (if-let (val (get-attr "fill-rule"))
                   (if (stringp val)
                     (if (string-equal val "evenodd") :even-odd :winding)
                     val)
                 :winding))
             (get-stroke ()
               (declare (inline get-stroke))
               (when-let (stroke (get-attr "stroke"))
                 (string-case (stroke)
                   ("context-fill"
                    (setq stroke (gethash "fill" container-attributes)))
                   ("context-stroke"
                    (setq stroke (gethash "stroke" container-attributes)))
                   (t nil))
                 (setq stroke
                       (cond ((member stroke '(nil "auto" "none") :test #'equalp) nil)
                             ((search "url" stroke) (css-parse-url stroke root-node))
                             (t (css-parse-color stroke))))
                 (when-let (op (or (parse-opacity (gethash "stroke-opacity" container-attributes))
                                   (parse-opacity (gethash "opacity" container-attributes))))
                   (setq stroke (with-alpha stroke op)))
                 (when-let (op (or (parse-opacity (gethash "stroke-opacity" new-attrs))
                                   (parse-opacity (gethash "opacity" new-attrs))))
                   (setq stroke (with-alpha stroke op)))
                 stroke))
             (get-stroke-dasharray ()
               (declare (inline get-stroke-dasharray))
               (when-let (val (get-attr "stroke-dasharray"))
                 (if (stringp val)
                   (map 'vector #'round (svg-parse-all-length val :width))
                   val)))
             (get-linecap ()
               (declare (inline get-linecap))
               (string-case ((get-attr "stroke-linecap"))
                 ("round" :round)
                 ("square" :square)
                 (t :butt)))
             (get-linejoin ()
               (declare (inline get-linejoin))
               (string-case ((get-attr "stroke-linejoin"))
                 ("round" :round)
                 ("bevel" :bevel)
                 (t :miter)))
             (get-a-length (name)
               (declare (inline get-a-length))
               (svg-parse-length
                (get-attr name)
                (if (or (find #\y name) (search "height" name)) :height :width)))
             (get-font ()
               (let* ((family (get-attr "font-family"))
                      (size (if-let (size (get-attr "font-size"))
                                (round (svg-parse-length size :width))
                              (vecto::size (vecto::font state))))
                      (loader (vecto::loader (vecto::font state))))
                 (when family
                   (let ((subs (split-sequence-if (lambda (c) (member c '(#\, #\Space #\"))) family
                                                  :remove-empty-subseqs t)))
                     (dolist (sub subs)
                       (string-case (sub)
                         ("serif"
                          #+(or mswindows darwin) (setq family "Times New Roman")
                          #-(or mswindows darwin) (setq family "Liberation Serif")
                          (return))
                         ("sans-serif"
                          #+mswindows (setq family "Arial")
                          #+darwin (setq family ".AppleSystemUIFont")
                          #-(or mswindows darwin) (setq family"Liberation Sans")
                          (return))
                         ("monospace"
                          #+(or mswindows darwin) (setq family "Courier New")
                          #-(or mswindows darwin) (setq family "Liberation Mono")
                          (return))
                         (t (when-let (file (find-matching-font sub))
                              (setq family file)
                              (return)))))
                     (when (or (stringp family) (pathnamep family))
                       (setq loader (vecto::%get-font state (get-font-file family))))))
                 (values loader size)))
             (multiply-transforms-for-drawing (trans-origin-x trans-origin-y)
               (let ((transform (make-transform))
                     (svg-transform (gethash "svg-transform" container-attributes))
                     (container-transforms (gethash "container-transforms" container-attributes))
                     (self-transforms (gethash "transform" new-attrs)))
                 ;(print (list svg-transform container-transforms self-transforms node))
                 ;; These transforms has not been parsed, so parse them first
                 (rectangle-bind (x y w h)
                     (gethash "viewBox" container-attributes)
                   (declare (ignore x y))
                   (when self-transforms
                     (setq self-transforms
                           (css-parse-transforms
                            state self-transforms w h
                            (gethash "width" container-attributes)
                            (gethash "height" container-attributes))))
                   (when container-transforms
                     (setq container-transforms
                           (css-parse-transforms
                            state container-transforms w h
                            (gethash "width" container-attributes)
                            (gethash "height" container-attributes)))))
                 (apply-translation transform (- trans-origin-x) (- trans-origin-y))
                 (dolist (trans (append self-transforms container-transforms))
                   (premultiply-transforms transform trans))
                 (apply-translation transform trans-origin-x trans-origin-y)
                 ;; Apply svg-transform in the end. It's relative with (0, 0);
                 (when svg-transform (postmultiply-transforms transform svg-transform))
                 transform))
             (run-draw-path (trans-origin-x trans-origin-y path)
               (declare (type double-float trans-origin-x trans-origin-y))
               (let* ((transform (coerce (multiply-transforms-for-drawing trans-origin-x trans-origin-y) 'vector))
                      (fill (get-fill))
                      (stroke (get-stroke))
                      (stroke-width (or (get-a-length "stroke-width") 1))
                      (linecap (get-linecap))
                      (linejoin (get-linejoin))
                      (dash (get-stroke-dasharray))
                      (fill-rule (get-fill-rule)))
                 ;; Deal with gradients
                 (when (plump-dom:node-p stroke)
                   (setq stroke :black))
                 (if (plump-dom:node-p fill)
                   (let ((grad-trans (gethash "gradientTransform" (plump-dom:attributes fill))))
                     ;; Parse gradient transforms
                     (rectangle-bind (x y w h)
                         (gethash "viewBox" container-attributes)
                       (declare (ignore x y))
                       (if grad-trans
                         (setq grad-trans
                               (css-parse-transforms state grad-trans w h
                                                     (gethash "width" container-attributes)
                                                     (gethash "height" container-attributes)))
                         (setq grad-trans (make-transform)))
                       (let* ((grad (svg-parse-gradient fill 0 0 w h))
                              (rev-grad-trans (vecto::invert-matrix (coerce grad-trans 'vector)))
                              (fill-source (lambda (x y)
                                             (multiple-value-bind (origin-x origin-y)
                                                 (vecto::transform-coordinates x y rev-grad-trans)
                                               (let ((color (svg-gradient-color origin-x origin-y grad)))
                                                 (values-list (map 'list #'vecto::float-octet (subseq color 1))))))))
                         (lambda (state)
                           (let ((state (vecto::copy state)))
                             (setf (vecto::line-width state) stroke-width
                                   (vecto::cap-style state) linecap
                                   (vecto::join-style state) linejoin
                                   (vecto::dash-vector state) dash
                                   (vecto::transform-matrix state) (if-let (old-trans (vecto::transform-matrix state))
                                                                       (vecto::mult transform old-trans)
                                                                     transform))
                             (draw-lw-path-to-vecto-state state path)
                             (setf (vecto::fill-source state) fill-source)
                             (if (eq fill-rule :even-odd)
                               (vecto::draw-even-odd-filled-paths state)
                               (vecto::draw-filled-paths state))
                             (when stroke
                               (setf (vecto::stroke-color state) (lw-color-to-vecto stroke))
                               (vecto::draw-stroked-paths state)))
                           ))))
                   ;; No gradient, yield normal drawing function
                   (lambda (state)
                     (let ((state (vecto::copy state)))
                       (setf (vecto::line-width state) stroke-width
                             (vecto::cap-style state) linecap
                             (vecto::join-style state) linejoin
                             (vecto::dash-vector state) dash)
                       (vecto::apply-matrix state transform)
                       (draw-lw-path-to-vecto-state state path)
                       (when fill
                         (setf (vecto::fill-color state) (lw-color-to-vecto fill))
                         (if (eq fill-rule :even-odd)
                           (vecto::draw-even-odd-filled-paths state)
                           (vecto::draw-filled-paths state)))
                       (if stroke
                         (progn
                           (setf (vecto::stroke-color state) (lw-color-to-vecto stroke))
                           (vecto::draw-stroked-paths state))
                         (when (not fill) ; Draw the path in default color if nether `fill` nor `stroke` specified.
                           (setf (vecto::stroke-color state) ())
                           (vecto::draw-stroked-paths state))))))))
             (collapse-whitespace-around-children (node)
               (let ((children (plump:children node))
                     (space (gethash "text-space" container-attributes)))
                 (unless (equalp space "preserve")
                   (let ((start (aref children 0))
                         (end (aref children (1- (length children)))))
                     (when (plump-dom:text-node-p start)
                       (setf (plump:text start) (string-left-trim '(#\Space #\Tab #\Return #\Newline) (plump:text start))))
                     (when (plump-dom:text-node-p end)
                       (setf (plump:text end) (string-right-trim '(#\Space #\Tab #\Return #\Newline) (plump:text end))))))))
             (draw-a-character (char font-loader size transform)
               ;; FIXME: i18n for :lr and :tb
               (let* ((text-x (pop (gethash "text-x" container-attributes)))
                      (shift-x (pop (gethash "text-dx" container-attributes)))
                      (text-y (pop (gethash "text-y" container-attributes)))
                      (shift-y (pop (gethash "text-dy" container-attributes)))
                      (rotate (pop (gethash "text-rotate" container-attributes)))
                      (x1 (or text-x
                              (+ (or (gethash "text-prev-x" container-attributes) 0)
                                 (or (gethash "text-total-width" container-attributes) 0))))
                      (dx1 (or shift-x
                               (gethash "text-prev-dx" container-attributes)
                               0))
                      (y1 (or text-y
                              (+ (or (gethash "text-prev-y" container-attributes) 0)
                                 (or (gethash "text-total-height" container-attributes) 0))))
                      (dy1 (or shift-y
                               (gethash "text-prev-dy" container-attributes)
                               0))
                      (r1 (or rotate
                              (gethash "text-prev-rotate" container-attributes)
                              0))
                      (x (+ x1 dx1))
                      (y (+ y1 dy1))
                      (char-width (get-char-width char font-loader size))
                      (char-height (get-char-height char font-loader size))
                      (fill (get-fill)))
                 (when text-x
                   (setf (gethash "text-prev-x" container-attributes) text-x
                         (gethash "text-total-width" container-attributes) 0))
                 (when text-y
                   (setf (gethash "text-prev-y" container-attributes) text-y
                         (gethash "text-total-height" container-attributes) 0))
                 (when rotate
                   (setf (gethash "text-prev-rotate" container-attributes) rotate))
                 (case (gethash "writing-mode" container-attributes)
                   (:lr
                    (setf (gethash "text-total-width" container-attributes)
                          (+ (gethash "text-total-width" container-attributes 0)
                             char-width)))
                   (:tb
                    (setf (gethash "text-total-height" container-attributes)
                          (+ (gethash "text-total-height" container-attributes 0)
                             char-height))))
                 (unless (serapeum:whitespacep char)
                   (let ((new-transform (make-transform)))
                     ;; ??? Why the path of string is mirrored @VECTO
                     (apply-translation new-transform (- x) (- y))
                     (apply-scale new-transform 1 -1)
                     (apply-rotation new-transform r1)
                     (apply-translation new-transform x y)
                     (postmultiply-transforms new-transform transform)
                     (setq new-transform (coerce new-transform 'vector))
                     (lambda (state)
                       (let ((state (vecto::copy state)))
                         (vecto::%set-font state font-loader size)
                         (setf (vecto::transform-matrix state) (if-let (old-trans (vecto::transform-matrix state))
                                                                   (vecto::mult new-transform old-trans)
                                                                 new-transform)
                               (vecto::fill-color state) (lw-color-to-vecto fill))
                         (vecto::%draw-string state x y (string char))))))))
             (draw-a-text-node (text-node)
               (let* ((transform (multiply-transforms-for-drawing
                                  (or (gethash "text-start-x" container-attributes) 0)
                                  (or (gethash "text-start-y" container-attributes) 0)))
                      (space (gethash "text-space" container-attributes)))
                 (multiple-value-bind (loader size) (get-font)
                   (unless (equalp space "preserve")
                     (setf (plump:text text-node)
                           (ppcre:regex-replace-all
                            *css-collapse-whitespace-scanner* (plump:text text-node)
                            ;; FIXME: check i18n here
                            (if (member (get-attr "lang") '("jp" "kr" "zh")
                                        :test (lambda (lang str) (string-prefix-p str lang)))
                              "" " "))))
                   (when-let (funcs (loop for char across (plump:text text-node)
                                          for func = (draw-a-character char loader size transform)
                                          when func collect func))
                     (lambda (state)
                       (dolist (func funcs)
                         (funcall func state))))))))
      (string-case (tag)
        ("path" (run-draw-path
                 0d0 0d0
                 (convert-path-commands (svg-parse-path-data (gethash "d" new-attrs)))))
        ("rect" (let ((x (get-a-length "x"))
                      (y (get-a-length "y"))
                      (w (get-a-length "width"))
                      (h (get-a-length "height"))
                      (rx (get-a-length "rx"))
                      (ry (get-a-length "ry")))
                  (declare (type double-float x y w h))
                  (cond ((and rx (null ry)) (setq ry (min (/ h 2d0) (* (/ rx w) h))))
                        ((and (null rx) ry) (setq rx (min (/ w 2d0) (* (/ ry h) w)))))
                  (run-draw-path
                   x y
                   (if (or (null rx) (and (= rx 0d0) (= ry 0d0)))
                     `((:move ,x ,y) (:line ,(+ x w) ,y) (:line ,(+ x w) ,(+ y h)) (:line ,x ,(+ y h)) (:close))
                     `((:move ,(+ x rx) ,y) (:line ,(- (+ x w) rx) ,y) (:arc ,(- (+ x w) rx) ,(+ y ry) ,rx ,ry 0 ,(- pi-by-2) 0)
                       (:line ,(+ x w) ,(- (+ y h) ry)) (:arc ,(- (+ x w) rx) ,(- (+ y h) ry) ,rx ,ry 0 0 ,pi-by-2)
                       (:line ,(+ x rx) ,(+ y h)) (:arc ,(+ x rx) ,(- (+ y h) ry) ,rx ,ry 0 ,pi-by-2 ,pi)
                       (:line ,x ,(+ y ry)) (:arc ,(+ x rx) ,(+ y ry) ,rx ,ry 0 ,pi ,(+ pi pi-by-2))
                       (:close))))))
        ("circle" (let ((cx (get-a-length "cx"))
                        (cy (get-a-length "cy"))
                        (r (get-a-length "r")))
                    (declare (type double-float cx cy r))
                    (run-draw-path cx cy (list (list :arc cx cy r r 0 0 2pi)))))
        ("ellipse" (let ((cx (get-a-length "cx")) (cy (get-a-length "cy"))
                         (rx (get-a-length "rx")) (ry (get-a-length "ry")))
                     (declare (type double-float cx cy rx ry))
                     (run-draw-path cx cy (list (list :arc cx cy rx ry 0 0 2pi)))))
        ("line" (let ((x1 (get-a-length "x1")) (y1 (get-a-length "y1"))
                      (x2 (get-a-length "x2")) (y2 (get-a-length "y2")))
                  (declare (type double-float x1 y1 x2 y2))
                  (run-draw-path (/ (+ x1 x2) 2d0) (/ (+ y1 y2) 2d0)
                                 (list (list :move x1 y1) (list :line x2 y2)))))
        ("polyline" (let* ((points (css-parse-all-numbers-from-string (get-attr "points")))
                           (path (make-array (1+ (/ (length points) 2)) :element-type 'list :fill-pointer 0))
                           (start-x (aref points 0)) (start-y (aref points 1))
                           mid-x mid-y)
                      (vector-push (list :move start-x start-y) path)
                      (loop for i fixnum from 2 to (1- (length points)) by 2
                            for x double-float = (aref points i)
                            and y double-float = (aref points (1+ i))
                            do (vector-push (list :line x y) path)
                            minimize x into min-x
                            maximize x into max-x
                            minimize y into min-y
                            maximize y into max-y
                            finally (setq min-x (min min-x start-x)
                                          max-x (max max-x start-x)
                                          min-y (min min-y start-y)
                                          max-y (max max-y start-y)
                                          mid-x (/ (+ min-x max-x) 2d0)
                                          mid-y (/ (+ min-y max-y) 2d0)))
                      (run-draw-path mid-x mid-y path)))
        ("polygon" (let* ((points (css-parse-all-numbers-from-string (get-attr "points")))
                          (path (make-array (+ (/ (length points) 2) 2) :element-type 'list :fill-pointer 0))
                          (start-x (aref points 0))
                          (start-y (aref points 1))
                          mid-x mid-y)
                     (vector-push (list :move start-x start-y) path)
                     (loop for i fixnum from 2 to (1- (length points)) by 2
                           for x double-float = (aref points i)
                           and y double-float = (aref points (1+ i))
                           do (vector-push (list :line x y) path)
                           minimize x into min-x
                           maximize x into max-x
                           minimize y into min-y
                           maximize y into max-y
                           finally (setq min-x (min min-x start-x)
                                         max-x (max max-x start-x)
                                         min-y (min min-y start-y)
                                         max-y (max max-y start-y)
                                         mid-x (/ (the double-float (+ min-x max-x)) 2d0)
                                         mid-y (/ (the double-float (+ min-y max-y)) 2d0)))
                     (vector-push (list :close) path)
                     (run-draw-path mid-x mid-y path)))
        ("text"
         (let* ((children (plump-dom:children node))
                (new-table (copy-hash-table container-attributes))
                ;; FIXME: formal whitespace handling
                (space (if-let (val (get-attr "white-space"))
                           (if (string-prefix-p "pre" val) "preserve" "normal")
                         (or (get-attr "space") (get-attr "xml:space")))))
           (maphash (lambda (k v) (setf (gethash k new-table) v))
                    new-attrs)
           (setf (gethash "text-x" new-table) (svg-parse-all-length (gethash "x" new-attrs) :width)
                 (gethash "text-y" new-table) (svg-parse-all-length (gethash "y" new-attrs) :height)
                 (gethash "text-dx" new-table) (svg-parse-all-length (gethash "dx" new-attrs) :width)
                 (gethash "text-dy" new-table) (svg-parse-all-length (gethash "dy" new-attrs) :height)
                 (gethash "text-rotate" new-table) (coerce (css-parse-all-angels-from-string (gethash "rotate" new-attrs)) 'list)
                 (gethash "text-space" new-table) space
                 (gethash "writing-mode" new-table) :lr
                 (gethash "text-start-x" new-table) (when-let (xs (gethash "text-x" new-table))
                                                      (first xs))
                 (gethash "text-start-y" new-table) (when-let (ys (gethash "text-y" new-table))
                                                      (first ys)))
           (setq container-attributes new-table)
           (collapse-whitespace-around-children node)
           (when-let (funcs (loop for child across children
                                  for func = (cond ((plump-dom:textual-node-p child)
                                                    (draw-a-text-node child))
                                                   ((plump-dom:element-p child)
                                                    (create-renderer state child root-node new-table)))
                                  when func collect func))
             (lambda (state)
               (dolist (func funcs)
                 (funcall func state))))))
        ("tspan"
         (let ((backup-attrs (loop for attr in (append 
                                                       *svg-presentation-attributes*)
                                   for val = (gethash attr new-attrs)
                                   for old = (gethash attr container-attributes)
                                   when val
                                     do (setf (gethash attr container-attributes) val)
                                     and collect (cons attr old))))
           (dolist (attr '("text-x" "text-y" "text-dx" "text-dy"
                           "text-rotate" "text-prev-rotate"))
             (push (cons attr (gethash attr container-attributes)) backup-attrs))
           (when-let (new (svg-parse-all-length (gethash "x" new-attrs) :width))
             (setf (gethash "text-x" container-attributes) (append new (gethash "text-x" container-attributes)))
             (unless (gethash "text-start-x" container-attributes)
               (setf (gethash "text-start-x" container-attributes) (first new))))
           (when-let (new (svg-parse-all-length (gethash "y" new-attrs) :height))
             (setf (gethash "text-y" container-attributes) (append new (gethash "text-y" container-attributes)))
             (unless (gethash "text-start-y" container-attributes)
               (setf (gethash "text-start-y" container-attributes) (first new))))
           (when-let (new (svg-parse-all-length (gethash "dx" new-attrs) :width))
             (setf (gethash "text-dx" container-attributes) (append new (gethash "text-dx" container-attributes))))
           (when-let (new (svg-parse-all-length (gethash "dy" new-attrs) :height))
             (setf (gethash "text-dy" container-attributes) (append new (gethash "text-dy" container-attributes))))
           (when-let (new (coerce (css-parse-all-angels-from-string (gethash "rotate" new-attrs)) 'list))
             (setf (gethash "text-rotate" container-attributes) new))
           (collapse-whitespace-around-children node)
           (when-let (funcs (loop for child across (plump-dom:children node)
                                  for func = (cond ((plump-dom:textual-node-p child)
                                                    (draw-a-text-node child))
                                                   ((plump-dom:element-p child)
                                                    (create-renderer state child root-node container-attributes)))
                                  when func collect func))
             (prog1 (lambda (state)
                      (dolist (func funcs)
                        (funcall func state)))
               (loop for (attr . val) in backup-attrs
                     do (setf (gethash attr container-attributes) val))))))
        (t (when (member tag '("a" "clipPath" "g" "mask" "pattern" "svg" "switch" "unknown" "use")
                         :test #'string=)
             (let ((new-table (copy-hash-table container-attributes)))
               (maphash (lambda (k v) (setf (gethash k new-table) v))
                        new-attrs)
               ;; If a transform is specified for a container,
               ;; since we know nether the transform-origin, nor the parent's size it relative with,
               ;; we need to store it for later parse & apply. It should be a STRING.
               (when-let (new-transform (gethash "transform" new-attrs))
                 ;; When transformed containers nested, append them after new transforms.
                 (when-let (trans (gethash "container-transforms" new-table))
                   (setq new-transform (string-append new-transform " " trans)))
                 (setf (gethash "container-transforms" new-table) new-transform))
               (string-case (tag)
                 ("svg"
                  (let ((viewbox (if-let (val (gethash "viewBox" new-attrs))
                                     (coerce (css-parse-all-numbers-from-string val) 'list)
                                   (list 0 0 (vecto::width state) (vecto::height state)))))
                    (rectangle-bind (viewbox-l viewbox-t viewbox-w viewbox-h)
                        viewbox
                      (let* ((new-x (if-let (val (gethash "x" new-attrs))
                                        (svg-parse-length val :width)
                                      0d0))
                             (new-y (if-let (val (gethash "y" new-attrs))
                                        (svg-parse-length val :height)
                                      0d0))
                             (new-w (svg-parse-length (gethash "width" new-attrs) :width))
                             (new-h (svg-parse-length (gethash "height" new-attrs) :height))
                             (transform (make-transform))
                             (preserve-aspect (if-let (val (gethash "preserveAspectRatio" new-attrs))
                                                  (split-sequence #\Space val)
                                                '("xMidYMid" "meet")))
                             (align (first preserve-aspect))
                             (scale (or (second preserve-aspect) "meet")))
                        (declare (type string align scale))
                        (cond ((and (null new-w) new-h) (setq new-w new-h))
                              ((and new-w (null new-h)) (setq new-h new-w))
                              ((null new-h) (setq new-w viewbox-w new-h viewbox-h)))
                        (setq new-w (round new-w)
                              new-h (round new-h))
                        (if (string= align "none") ; Do not preserve aspect
                          (progn
                            ;; Scale viewBox to fit SVG `width` and `height`
                            (when (and viewbox (or new-w new-h))
                              (apply-scale transform (/ new-w viewbox-w) (/ new-h viewbox-h)))
                            ;; Move the left-top of the viewBox to (x, y)
                            (apply-translation transform new-x new-y))
                          (let* ((xalign (subseq align 0 4))
                                 (yalign (subseq align 4 8))
                                 (svg-align-x 0)
                                 (svg-align-y 0)
                                 (viewbox-align-x 0)
                                 (viewbox-align-y 0))
                            (declare (type string xalign yalign))
                            (string-case (xalign)
                              ("xMin" (setq svg-align-x new-x
                                            viewbox-align-x viewbox-l))
                              ("xMid" (setq svg-align-x (+ new-x (/ new-w 2))
                                            viewbox-align-x (+ viewbox-l (/ viewbox-w 2))))
                              ("xMax" (setq svg-align-x (+ new-x new-w)
                                            viewbox-align-x (+ viewbox-l viewbox-w))))
                            (string-case (yalign)
                              ("YMin" (setq svg-align-y new-y
                                            viewbox-align-y viewbox-t))
                              ("YMid" (setq svg-align-y (+ new-y (/ new-h 2))
                                            viewbox-align-y (+ viewbox-t (/ viewbox-h 2))))
                              ("YMax" (setq svg-align-y (+ new-y new-h)
                                            viewbox-align-y (+ viewbox-t viewbox-h))))
                            ;; Apply scaling, and align selected points
                            (apply-translation transform (- viewbox-align-x) (- viewbox-align-y))
                            (let* ((x-ratio (/ new-w viewbox-w))
                                   (y-ratio (/ new-h viewbox-h))
                                   (ratio (if (string= scale "meet")
                                            (min x-ratio y-ratio)
                                            (max x-ratio y-ratio))))
                              (apply-scale transform ratio ratio))
                            (apply-translation transform svg-align-x svg-align-y)))
                        ;; If the element itself is `svg`, we assume the transform-origin is 0, 0;
                        ;; no additional translation needed, so we just multiply them into the svg-transform
                        (when-let (container-transforms (gethash "container-transforms" new-table))
                          (dolist (trans container-transforms)
                            (postmultiply-transforms transform trans))
                          (remhash "container-transforms" new-table))
                        (when-let (prev-transform (gethash "svg-transform" container-attributes))
                          (postmultiply-transforms transform prev-transform))
                        ;; Store modified attributes
                        (setf (gethash "svg-transform" new-table) transform
                              (gethash "viewBox" new-table) viewbox
                              (gethash "width" new-table) new-w
                              (gethash "height" new-table) new-h)
                        ;; Pack a drawing function
                        (let ((funcs (delete nil (loop for child across (plump-dom:children node)
                                                       when (plump-dom:element-p child)
                                                         collect (create-renderer state child root-node new-table)))))
                          (lambda (state)
                            ;; If it's the out-most SVG element,
                            ;; and the graphics state has no underlying image representation,
                            ;; resize the Vecto canvas to fit its size
                            (when (and first-call-p
                                       (or (not (slot-boundp state 'vecto::image))
                                           (null (vecto::image state))))
                              ;; From vecto::state-image
                              (setf (vecto::image state)
                                    (make-instance 'zpng:png
                                                   :width new-w
                                                   :height new-h
                                                   :color-type vecto::+png-color-type+)
                                    (vecto::width state) new-w
                                    (vecto::height state) new-h
                                    (vecto::clipping-path state) (vecto::make-clipping-path new-w new-h)))
                            (dolist (func funcs)
                              (funcall func state))))))))
                 ("use"
                  (let* ((id (string-left-trim '(#\#) (or (gethash "href" new-attrs)
                                                          (gethash "xlink:href" new-attrs))))
                         (new-x (if-let (val (gethash "x" new-attrs))
                                    (svg-parse-length val :width)
                                  0d0))
                         (new-y (if-let (val (gethash "y" new-attrs))
                                    (svg-parse-length val :height)
                                  0d0))
                         (transform (if-let (val (gethash "svg-transform" container-attributes))
                                        (copy-transform val)
                                      (make-transform)))
                         (child (plump-dom:get-element-by-id root-node id))
                         (child-tag (plump-dom:tag-name child))
                         ; Make a shadow tree
                         (shadow-child (make-instance 'plump:element
                                                      :parent node
                                                      :tag-name (if (member child-tag '("def" "marker" "symbol"))
                                                                  "g" child-tag)
                                                      :children (plump:make-child-array)
                                                      :attributes (plump:attributes node))))
                    (declare (type double-float new-x new-y))
                    ;; Move the left-top of the sub-graph to (x, y)
                    (apply-translation transform new-x new-y)
                    (setf (gethash "svg-transform" new-table) transform)
                    (maphash (lambda (key val)
                               (when (or (string= key "style")
                                         (member key *svg-presentation-attributes* :test #'string=))
                                 (plump:set-attribute shadow-child key val)))
                             (plump:attributes child))
                    (loop for child-child across (plump:children child)
                          for copy = (plump-dom:clone-node child-child)
                          do (plump-dom:append-child shadow-child copy))
                    (create-renderer state shadow-child root-node new-table)))
                 (t
                  (let ((funcs (delete nil (loop for child across (plump-dom:children node)
                                                 when (plump-dom:element-p child)
                                                   collect (create-renderer state child root-node new-table)))))
                    (lambda (state)
                      (dolist (func funcs)
                        (funcall func state)))))))))))))

(defun draw-svg-from-string (svg-string output &key (font (get-font-file)) (viewport-width 1920) (viewport-height 1080))
  "Draw the first SVG element inside STRING.

If OUTPUT is a stream, write the PNG binary data to OUTPUT; Otherwise
it treat the OUTPUT as a namestring, try to write the PNG to its file
using ZPNG:WRITE-PNG."
  (let ((svg (first (plump:get-elements-by-tag-name (plump:parse svg-string) "svg"))))
    (let ((vecto::*graphics-state* (make-instance
                                    'vecto::graphics-state
                                    :width viewport-width
                                    :height viewport-height
                                    :transform-matrix (vecto::identity-matrix))))
      (vecto:set-font (vecto:get-font font) 12)
      (let ((renderer (create-renderer vecto::*graphics-state* svg)))
        (funcall renderer vecto::*graphics-state*))
      (vecto::after-painting vecto::*graphics-state*)
      (if (streamp output)
        (vecto:save-png-stream output)
        (vecto:save-png output))
      (vecto::clear-state vecto::*graphics-state*))))

(defparameter *interactive-tests*
  '(("Text and tspan" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/text/tspan01.svg")
    ("Text rotation 1" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/text/tspan04.svg")
    ("Text rotation 2" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/text/tspan05.svg")
    ("Path quadratic bezier" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/paths/quad01.svg")
    ("Path arc" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/paths/arcs01.svg")
    ("Retangle and transform" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/rect02.svg")
    ("Ellipse and transform" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/ellipse01.svg")
    ("Line and stroke-width" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/line01.svg")
    ("Polyline" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/polyline01.svg")
    ("Polygon" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/shapes/polygon01.svg")
    ("Dashed stroke" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/painting/dashes.svg")
    ("Fill rule nonzero" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/painting/fillrule-nonzero.svg")
    ("Fill rule evenodd" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/painting/fillrule-evenodd.svg")
    ("Groups and Opacity" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/masking/opacity01.svg")
    ("Linear gradient" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/pservers/lingrad01.svg")
    ("Radial gradient" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/pservers/radgrad01.svg")
    ("Use element and CSS style" "https://www.w3.org/TR/2018/CR-SVG2-20181004/images/struct/Use-changed-styles.svg")))

;; Here's the interactive test adapted from LW-SVG.
;; Can only running with LispWorks Macintosh.
;; It requires DEXADOR package:
;; (ql:quickload :dexador)

#+nil
(capi:define-interface interactive-test-interface ()
  ((i :initform 0)
   (name :initform (first (first *interactive-tests*)))
   (url :initform (second (first *interactive-tests*)))
   (wrong :initform nil))
  (:panes
   (browser
    capi:browser-pane
    :url url
    :visible-min-width 500
    :visible-min-height 500)
   (output
    capi:output-pane
    :visible-min-width 500
    :visible-min-height 500
    :background :white
    :foreground :black
    :display-callback (lambda (pane x y w h)
                        (declare (ignore x y w h))
                        (gp:draw-image
                         pane
                         (gp:convert-external-image
                          pane
                          (make-instance 'gp:external-image
                                         :type :png
                                         :data (flexi-streams:with-output-to-sequence (out)
                                                 (draw-svg-from-string
                                                  (dex:get (slot-value (capi:element-interface pane) 'url) :force-string t)
                                                  out
                                                  :viewport-width 500 :viewport-height 500))))
                         0 0)))
   (buttons
    capi:push-button-panel
    :items '(:yes :no)
    :selection-callback
    (lambda (data itf)
      (with-slots (i name url browser output wrong) itf
        (when (eql data :no)
          (push name wrong))
        (incf i)
        (if (>= i (length *interactive-tests*))
          (progn
            (if wrong
              (capi:prompt-with-list
               wrong
               (format nil "Test finished with ~A/~A errors." (length wrong) (length *interactive-tests*)))
              (capi:prompt-with-message "All test are passed!"))
            (capi:quit-interface itf))
          (progn
            (setf (capi:interface-title itf) (format nil "~A/~A ~A"
                                                     (1+ i) (1+ (length *interactive-tests*))
                                                     (first (nth i *interactive-tests*)))
                  url (second (nth i *interactive-tests*)))
            (capi:browser-pane-navigate browser url)
            (gp:invalidate-rectangle output)))))))
  (:layouts
   (main-layout
    capi:column-layout
    '(displayers-row buttons)
    :title "Compare if two images are same"
    :title-position :frame
    :adjust :center)
   (displayers-row
    capi:row-layout
    '(browser output)))
  (:default-initargs
   :title (format nil "1/~A ~A" (1+ (length *interactive-tests*)) (first (first *interactive-tests*)))))

;(capi:display (make-instance 'interactive-test-interface))

#|
(ql:quickload :trivial-svg)

(let* ((width (* 32 24))
       (height (* 32 7))
       (state (make-instance 'vecto::graphics-state
                             :width width
                             :height height
                             :transform-matrix (vecto::identity-matrix)))
       ;; We put 42 Google material symbols in ~/svg-test/
       (svg-files (directory "~/svg-test/*.svg")))
  (setf (vecto::image state)
        (make-instance 'zpng:png
                       :width width
                       :height height
                       :color-type vecto::+png-color-type+)
        (vecto::clipping-path state) (vecto::make-clipping-path width height))
  (vecto::%set-font state (vecto::%get-font state (trivial-svg:get-font-file)) 12)
  (loop for i from 0
        for svg-file in (serapeum:repeat-sequence svg-files 4)
        for svg-node = (first (plump:get-elements-by-tag-name (plump:parse svg-file) "svg"))
        do (multiple-value-bind (y x) (floor i 24)
             (let* ((state (vecto::copy state))
                    (renderer (trivial-svg:create-renderer state svg-node)))
               (vecto::%translate state (* x 32) (* y 32))
               (funcall renderer state))))
  (vecto::after-painting state)
  (zpng:write-png (vecto::image state)
                  (merge-pathnames "example.png" (asdf:system-source-directory :trivial-svg)))
  (vecto::clear-state state))
|#
