;;;; graphics.lisp - Showing Version 6 pictures in the terminal
;;;;
;;;; Pictures are drawn with sixel graphics when the terminal supports them.
;;;; There is no pixel-addressed screen here, so a picture appears inline
;;;; where the text has reached rather than at the coordinates the story asks
;;;; for. That is enough for the illustrations; it is not enough to rebuild a
;;;; Version 6 screen out of tiles, so only pictures above a size threshold
;;;; are drawn and the small layout pieces are skipped.
;;;;
;;;; Two conversion paths, as for any PNG:
;;;;   1. img2sixel (libsixel) when it is installed
;;;;   2. PowerShell and System.Drawing to scale, then the encoder below

(in-package :zmachine)

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *graphics-enabled* :auto
  "T always draws pictures, NIL never does, :AUTO draws them when the
terminal looks capable of sixel graphics")

(defvar *picture-min-area* 10000
  "Pictures smaller than this many pixels are treated as layout pieces and
skipped. A Version 6 story composes its screen from small tiles - Zork Zero
draws its border out of forty-five by forty pieces - and drawing those one at
a time in a terminal is meaningless. The illustrations are far larger.")

(defvar *picture-width* 400
  "Width in pixels a picture is scaled to before display")

(defvar *sixel-levels* 6
  "Colour levels per channel (6 => 216 colours)")

(defvar *sixel-dither* t
  "Apply ordered dithering when reducing colours")

(defvar *picture-cache* (make-hash-table)
  "Picture number -> its sixel encoding, so each one is converted once")

(defvar *img2sixel-available* :unknown
  "Cached result of the img2sixel lookup")

;;; ============================================================
;;; Terminal Capability
;;; ============================================================

(defun env-var (name)
  (sb-ext:posix-getenv name))

(defun sixel-terminal-p ()
  "Guess whether the current terminal can render sixel graphics"
  (let ((program (or (env-var "TERM_PROGRAM") ""))
        (term (or (env-var "TERM") "")))
    (or (and (env-var "WT_SESSION") t)
        (and (some (lambda (s) (search s program :test #'char-equal))
                   '("wezterm" "iterm" "mintty" "contour"))
             t)
        (and (some (lambda (s) (search s term :test #'char-equal))
                   '("sixel" "kitty" "foot" "mlterm" "contour" "wezterm" "vt340"))
             t))))

(defun graphics-available-p ()
  "Whether pictures should be drawn at all"
  (and (resources-loaded-p)
       (cond ((null *graphics-enabled*) nil)
             ((eq *graphics-enabled* :auto) (sixel-terminal-p))
             (t t))))

(defun img2sixel-available-p ()
  (when (eq *img2sixel-available* :unknown)
    (setf *img2sixel-available*
          (handler-case
              (let ((out (make-string-output-stream)))
                (sb-ext:run-program "img2sixel" '("--version")
                                    :output out :error nil :search t)
                (and (search "sixel" (get-output-stream-string out)
                             :test #'char-equal)
                     t))
            (error () nil))))
  *img2sixel-available*)

;;; ============================================================
;;; PNG -> RGB (PowerShell and System.Drawing)
;;; ============================================================

(defun png-to-raw-script (in-path out-path width)
  "PowerShell that scales an image file and dumps raw BGR rows"
  (format nil
          "$ErrorActionPreference='Stop'
Add-Type -AssemblyName System.Drawing
$src=[System.Drawing.Image]::FromFile('~A')
$w=~D
if($src.Width -lt $w){$w=$src.Width}
$h=[int][Math]::Max(1,[Math]::Round($src.Height*$w/$src.Width))
$bmp=New-Object System.Drawing.Bitmap($w,$h,[System.Drawing.Imaging.PixelFormat]::Format24bppRgb)
$rect=New-Object System.Drawing.Rectangle(0,0,$w,$h)
$g=[System.Drawing.Graphics]::FromImage($bmp)
$g.InterpolationMode=[System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
$g.PixelOffsetMode=[System.Drawing.Drawing2D.PixelOffsetMode]::HighQuality
$attr=New-Object System.Drawing.Imaging.ImageAttributes
$attr.SetWrapMode([System.Drawing.Drawing2D.WrapMode]::TileFlipXY)
$g.DrawImage($src,$rect,0,0,$src.Width,$src.Height,[System.Drawing.GraphicsUnit]::Pixel,$attr)
$g.Dispose()
$attr.Dispose()
$d=$bmp.LockBits($rect,[System.Drawing.Imaging.ImageLockMode]::ReadOnly,[System.Drawing.Imaging.PixelFormat]::Format24bppRgb)
$len=$d.Stride*$h
$buf=New-Object byte[] $len
[System.Runtime.InteropServices.Marshal]::Copy($d.Scan0,$buf,0,$len)
$bmp.UnlockBits($d)
$fs=[System.IO.File]::Create('~A')
$hdr=[System.Text.Encoding]::ASCII.GetBytes(\"ZMBGR $w $h $($d.Stride)`n\")
$fs.Write($hdr,0,$hdr.Length)
$fs.Write($buf,0,$len)
$fs.Close()
$bmp.Dispose()
$src.Dispose()"
          (ps-escape (namestring (truename in-path)))
          width
          (ps-escape out-path)))

(defun split-on-space (string)
  (let ((words nil) (start nil))
    (dotimes (i (length string))
      (if (char= (char string i) #\Space)
          (when start (push (subseq string start i) words) (setf start nil))
          (unless start (setf start i))))
    (when start (push (subseq string start) words))
    (nreverse words)))

(defun decode-zmbgr (bytes)
  "Decode a ZMBGR dump into (values rgb width height)"
  (let ((nl (position 10 bytes)))
    (when nl
      (let ((parts (split-on-space
                    (string-right-trim
                     '(#\Return)
                     (map 'string #'code-char (subseq bytes 0 nl))))))
        (when (and (= (length parts) 4) (string= (first parts) "ZMBGR"))
          (let* ((w (parse-integer (second parts)))
                 (h (parse-integer (third parts)))
                 (stride (parse-integer (fourth parts)))
                 (base (1+ nl))
                 (rgb (make-array (* w h 3) :element-type '(unsigned-byte 8))))
            (when (< (length bytes) (+ base (* stride h)))
              (return-from decode-zmbgr nil))
            (dotimes (y h)
              (let ((src (+ base (* y stride)))
                    (dst (* y w 3)))
                (dotimes (x w)
                  (let ((s (+ src (* x 3)))
                        (d (+ dst (* x 3))))
                    ;; System.Drawing gives BGR, sixel wants RGB
                    (setf (aref rgb d)       (aref bytes (+ s 2))
                          (aref rgb (+ d 1)) (aref bytes (+ s 1))
                          (aref rgb (+ d 2)) (aref bytes s))))))
            (values rgb w h)))))))

(defun image-file-to-rgb (path width)
  "Scale the image at PATH to WIDTH and return (values rgb width height)"
  (let ((raw (temp-file-path "img.raw")))
    (unwind-protect
         (handler-case
             (progn
               (run-powershell (png-to-raw-script path raw width))
               (let ((bytes (read-binary-file raw)))
                 (when (and bytes (> (length bytes) 16))
                   (decode-zmbgr bytes))))
           (error (e)
             (format *error-output* "~&[image decode error: ~A]~%" e)
             nil))
      (delete-file-if-exists raw))))

;;; ============================================================
;;; Sixel Encoding
;;; ============================================================

(defparameter *bayer-matrix*
  #2A((0 8 2 10) (12 4 14 6) (3 11 1 9) (15 7 13 5))
  "4x4 ordered dithering matrix")

(defun bayer-offset (x y)
  (- (/ (aref *bayer-matrix* (mod y 4) (mod x 4)) 16.0) 0.5))

(defun quantize-channel (value maxi offset)
  (let ((level (round (+ (* (/ value 255.0) maxi) offset))))
    (max 0 (min maxi level))))

(defun sixel-emit-run (stream char count)
  (if (<= count 3)
      (dotimes (i count) (write-char char stream))
      (format stream "!~D~C" count char)))

(defun sixel-band-runs (indices width y0 rows colour)
  "Run-length encoded sixel characters for COLOUR in one six pixel band"
  (let ((runs nil) (prev nil) (count 0))
    (dotimes (x width)
      (let ((bits 0))
        (dotimes (dy rows)
          (when (= (aref indices (+ (* (+ y0 dy) width) x)) colour)
            (setf bits (logior bits (ash 1 dy)))))
        (let ((ch (code-char (+ 63 bits))))
          (if (eql ch prev)
              (incf count)
              (progn (when prev (push (cons prev count) runs))
                     (setf prev ch count 1))))))
    (when prev (push (cons prev count) runs))
    ;; RUNS is reversed, so trailing empty sixels sit at the front
    (loop while (and runs (char= (car (first runs)) #\?))
          do (pop runs))
    (nreverse runs)))

(defun rgb-to-sixel (rgb width height)
  "Encode RGB pixel data as a sixel escape sequence"
  (let* ((levels (max 2 (min 8 *sixel-levels*)))
         (maxi (1- levels))
         (indices (make-array (* width height) :element-type 'fixnum))
         (used (make-array (* levels levels levels) :element-type 'bit
                                                    :initial-element 0)))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((p (* 3 (+ (* y width) x)))
               (offset (if *sixel-dither* (bayer-offset x y) 0))
               (r (quantize-channel (aref rgb p) maxi offset))
               (g (quantize-channel (aref rgb (+ p 1)) maxi offset))
               (b (quantize-channel (aref rgb (+ p 2)) maxi offset))
               (colour (+ (* r levels levels) (* g levels) b)))
          (setf (aref indices (+ (* y width) x)) colour)
          (setf (aref used colour) 1))))
    (with-output-to-string (s)
      (format s "~CPq\"1;1;~D;~D" #\Escape width height)
      (dotimes (colour (length used))
        (when (= 1 (aref used colour))
          (multiple-value-bind (r rest) (floor colour (* levels levels))
            (multiple-value-bind (g b) (floor rest levels)
              (format s "#~D;2;~D;~D;~D" colour
                      (round (* 100 r) maxi)
                      (round (* 100 g) maxi)
                      (round (* 100 b) maxi))))))
      (loop for y0 from 0 below height by 6
            do (let ((rows (min 6 (- height y0)))
                     (present (make-hash-table))
                     (first-colour t))
                 (dotimes (dy rows)
                   (dotimes (x width)
                     (setf (gethash (aref indices (+ (* (+ y0 dy) width) x))
                                    present)
                           t)))
                 (dolist (colour (sort (loop for c being the hash-keys of present
                                             collect c)
                                       #'<))
                   (let ((runs (sixel-band-runs indices width y0 rows colour)))
                     (when runs
                       (if first-colour (setf first-colour nil) (write-char #\$ s))
                       (format s "#~D" colour)
                       (loop for (ch . n) in runs do (sixel-emit-run s ch n)))))
                 (write-char #\- s)))
      (format s "~C\\" #\Escape))))

(defun image-file-to-sixel (path width)
  "Sixel encoding of the image at PATH, or NIL"
  (or (when (img2sixel-available-p)
        (let ((out-file (temp-file-path "img.six")))
          (unwind-protect
               (handler-case
                   (progn
                     (sb-ext:run-program "img2sixel"
                                         (list "-w" (format nil "~D" width)
                                               "-o" out-file
                                               (namestring (truename path)))
                                         :search t :wait t :error nil)
                     (read-text-file out-file))
                 (error () nil))
            (delete-file-if-exists out-file))))
      (multiple-value-bind (rgb w h) (image-file-to-rgb path width)
        (when rgb
          (rgb-to-sixel rgb w h)))))

;;; ============================================================
;;; Drawing a Picture
;;; ============================================================

(defun picture-worth-drawing-p (number)
  "Whether picture NUMBER is an illustration rather than a layout piece"
  (multiple-value-bind (w h) (picture-size number)
    (and (member (picture-kind number) '(:png :jpeg))
         (>= (* w h) *picture-min-area*))))

(defun picture-sixel (number)
  "Sixel encoding of picture NUMBER, converting and caching it on first use"
  (multiple-value-bind (cached found) (gethash number *picture-cache*)
    (if found
        cached
        (let ((bytes (picture-bytes number)))
          (setf (gethash number *picture-cache*)
                (when bytes
                  (let ((file (temp-file-path "pic.png")))
                    (unwind-protect
                         (progn (write-binary-file file bytes)
                                (image-file-to-sixel file *picture-width*))
                      (delete-file-if-exists file)))))))))

(defun draw-picture (number)
  "Draw picture NUMBER inline. Returns T when something was drawn."
  (when (and (graphics-available-p) (picture-worth-drawing-p number))
    (handler-case
        (let ((sixel (picture-sixel number)))
          (when sixel
            (fresh-line *standard-output*)
            (write-string sixel *standard-output*)
            (terpri *standard-output*)
            (force-output *standard-output*)
            t))
      (error (e)
        (format *error-output* "~&[picture ~D failed: ~A]~%" number e)
        nil))))

;;; ============================================================
;;; User Interface
;;; ============================================================

(defun show-picture (number)
  "Draw a picture by hand, ignoring the size threshold"
  (cond ((not (resources-loaded-p))
         (format t "No resource file loaded.~%") nil)
        ((not (picture-exists-p number))
         (format t "No picture ~D.~%" number) nil)
        (t
         (multiple-value-bind (w h) (picture-size number)
           (format t "Picture ~D: ~(~A~) ~Dx~D~%" number (picture-kind number) w h))
         (let ((*picture-min-area* 0)
               (*graphics-enabled* (or *graphics-enabled* :auto)))
           (or (draw-picture number)
               (progn (format t "(not drawable here)~%") nil))))))

(defun list-pictures (&optional (n 20))
  "List the pictures that would be drawn during play"
  (let ((numbers (sort (loop for k being the hash-keys of *blorb-pictures*
                             when (picture-worth-drawing-p k) collect k)
                       #'<)))
    (format t "~%=== Illustrations (~D of ~D pictures) ===~%"
            (length numbers) (picture-count))
    (loop for number in numbers
          for i from 1
          while (<= i n)
          do (multiple-value-bind (w h) (picture-size number)
               (format t "  #~4D  ~4D x ~4D~%" number w h)))
    (when (> (length numbers) n)
      (format t "  ... and ~D more~%" (- (length numbers) n)))
    (length numbers)))

(defun graphics-status ()
  "Where pictures come from and whether they can be shown"
  (format t "~%=== Graphics ===~%")
  (format t "Resource file : ~A~%" (or *blorb-path* "none"))
  (format t "Pictures      : ~D~%" (picture-count))
  (format t "Illustrations : ~D (at least ~D pixels)~%"
          (loop for k being the hash-keys of *blorb-pictures*
                count (picture-worth-drawing-p k))
          *picture-min-area*)
  (format t "Terminal      : ~A~%" (if (sixel-terminal-p) "sixel" "no sixel"))
  (format t "img2sixel     : ~A~%"
          (if (img2sixel-available-p) "yes" "no (using built-in encoder)"))
  (format t "Drawing       : ~A~%" (if (graphics-available-p) "on" "off"))
  (format t "Cached        : ~D~%" (hash-table-count *picture-cache*))
  (values))
