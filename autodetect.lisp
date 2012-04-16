#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :asdf-encodings)

;;; Based on code by Douglas Crosher
;;; Examine the octect 'stream and returns three values:
;;;   1. True if valid UTF-8.
;;;   2. True if valid UTF-8 and UTF-8 specific sequences were found.
;;;   3. True if valid UTF-8 and the UTF-8 BOM was found at the start.
(defun detect-utf-8 (file)
  (with-open-file (stream file :direction :input
                         :element-type '(unsigned-byte 8))
    (flet ((extrap (c)
            (= (logand c #xc0) #x80))
          (b2-leading-p (c)
            (= (logand c #xe0) #xc0))
          (b3-leading-p (c)
            (= (logand c #xf0) #xe0))
          (b4-leading-p (c)
            (= (logand c #xf8) #xf0))
          (b5-leading-p (c)
            (= (logand c #xfc) #xf8)))
      (let ((bom-found-p nil)
           (foundp nil))
       (loop
          (let ((b (read-byte stream nil nil)))
            (cond ((not b)
                   (return))
                  ((< b #x80))
                  (t
                   (let ((b1 (read-byte stream nil nil)))
                     (cond ((or (not b1) (not (extrap b1)))
                            (return-from detect-utf-8
                              (values nil nil nil)))
                           ((b2-leading-p b)
                            (setf foundp t))
                           (t
                            (let ((b2 (read-byte stream nil nil)))
                              (cond ((or (not b2) (not (extrap b2)))
                                     (return-from detect-utf-8
                                       (values nil nil nil)))
                                    ((b3-leading-p b)
                                     (setf foundp t)
                                     (when (and foundp
                                                (= b #xef)
                                                (= b1 #xbb)
                                                (= b2 #xbf))
                                       (setf bom-found-p t)))
                                    (t
                                     (let ((b3 (read-byte stream nil nil)))
                                       (cond ((or (not b3) (not (extrap b3)))
                                              (return-from detect-utf-8
                                                (values nil nil nil)))
                                             ((b4-leading-p b)
                                              (setf foundp t))
                                             (t
                                              (let ((b4 (read-byte stream nil nil)))
                                                (cond ((or (not b4) (not (extrap b4)))
                                                       (return-from detect-utf-8
                                                         (values nil nil nil)))
                                                      ((b5-leading-p b)
                                                       (setf foundp t))
                                                      (t
                                                       (return-from detect-utf-8
                                                         (values nil nil nil))))))))))))))))))
       (values t foundp bom-found-p)))))

;;; Examine the 'file to determine the encoding.  In some cases the
;;; encoding can be determined from the coding of the file itself,
;;; otherwise it may be specified in a file options line with the
;;; 'external-format', 'encoding', or 'coding' options.  If the
;;; encoding is not detected or declared but is valid UTF-8 using then
;;; UTF-8 specific characters then :utf-8 is returned, otherwise
;;; :default is returned.
(defvar *detect-lisp-source-encoding* t)
;;;
(defun lisp-source-encoding (file)
  (unless *detect-lisp-source-encoding*
    (return-from lisp-source-encoding :default))
  (let ((initial-encoding nil)
       (declared-encoding nil))
    (with-open-file (s file :element-type '(unsigned-byte 8)
                      :direction :input)
      ;; Buffer a chunk from the start of the file.
      (let* ((buffer (make-array 320 :element-type '(unsigned-byte 8)))
            (available (read-sequence buffer s)))
       (labels ((decode-ascii-encoded-declaration (start size offset)
                  (declare (type fixnum start)
                           (type (integer 1 4) size)
                           (type (integer 0 3) offset))
                  ;; Convert the buffered chunk to ASCII.
                  (let ((ascii (make-string 320 :initial-element #\?))
                        (ascii-end 0))
                    (do ()
                        ((< available (+ start size)))
                      (let* ((code (ecase size
                                     (1
                                      (aref buffer start))
                                     (2
                                      (let ((b0 (aref buffer start))
                                            (b1 (aref buffer (1+ start))))
                                        (ecase offset
                                          (0
                                           (logior (ash b1 8) b0))
                                          (1
                                           (logior (ash b0 8) b1)))))
                                     (4
                                      (let ((b0 (aref buffer start))
                                            (b1 (aref buffer (+ start 1)))
                                            (b2 (aref buffer (+ start 2)))
                                            (b3 (aref buffer (+ start 3))))
                                        (ecase offset
                                          (0
                                           (logior (ash b3 24) (ash b2 16) (ash b1 8) b0))
                                          (1
                                           (logior (ash b1 24) (ash b0 16) (ash b3 8) b2))
                                          (2
                                           (logior (ash b2 24) (ash b3 16) (ash b0 8) b1))
                                          (3
                                           (logior (ash b0 24) (ash b1 16) (ash b2 8) b3))))))))
                        (incf start size)
                        (let ((ch (if (< 0 code #x80) (code-char code) #\?)))
                          (setf (aref ascii ascii-end) ch)
                          (incf ascii-end))))
                    ;; Parse the file options.
                    (let ((found (search "-*-" ascii))
                          (options nil))
                      (when found
                        (block do-file-options
                          (let* ((start (+ found 3))
                                 (end (search "-*-" ascii :start2 start)))
                            (unless end
                              ;; No closing "-*-".  Aborting file options.
                              (return-from do-file-options))
                            (unless (find #\: ascii :start start :end end)
                              ;; Old style mode comment, or empty?
                              (return-from do-file-options))
                            (do ((opt-start start (1+ semi)) colon semi)
                                (nil)
                              (setf colon (position #\: ascii :start opt-start :end end))
                              (unless colon
                                ;; Missing ":".  Aborting file options.
                                (return-from do-file-options))
                              (setf semi (or (position #\; ascii :start colon :end end) end))
                              (let ((option (string-trim '(#\space #\tab)
                                                         (subseq ascii opt-start colon)))
                                    (value (string-trim '(#\space #\tab)
                                                        (subseq ascii (1+ colon) semi))))
                                (push (cons option value) options)
                                (when (= semi end) (return nil)))))))
                      (setf declared-encoding
                            (cond ((cdr (assoc "external-format" options :test 'equalp)))
                                  ((cdr (assoc "encoding" options :test 'equalp)))
                                  ((cdr (assoc "coding" options :test 'equalp)))))))))
         ;; Look at the first four bytes to determine the encoding.
         (cond ((>= available 4)
                (let ((b1 (aref buffer 0))
                      (b2 (aref buffer 1))
                      (b3 (aref buffer 2))
                      (b4 (aref buffer 3)))
                  (cond ((and (= b1 #x00) (= b2 #x00) (= b3 #xFE) (= b4 #xFF))
                         ;; UCS-4, big-endian (1234 order).
                         (setf initial-encoding :ucs-4be)
                         (decode-ascii-encoded-declaration 4 4 3))
                        ((and (= b1 #xff) (= b2 #xfe))
                         (cond ((and (= b3 #x00) (= b4 #x00))
                                ;; UCS-4, little-endian (4321 order).
                                (setf initial-encoding :ucs-4le)
                                (decode-ascii-encoded-declaration 4 4 0))
                               (t
                                ;; UTF-16, little-endian
                                (setf initial-encoding :utf-16le)
                                (decode-ascii-encoded-declaration 2 2 0))))
                        ((and (= b1 #x00) (= b2 #x00) (= b3 #xFF) (= b4 #xFE))
                         ;; UCS-4, order (2143).
                         (decode-ascii-encoded-declaration 4 4 2))
                        ((and (= b1 #xfe) (= b2 #xff))
                         (cond ((and (= b3 #x00) (= b4 #x00))
                                ;; UCS-4, order (3412).
                                (decode-ascii-encoded-declaration 4 4 1))
                               (t
                                ;; UTF-16, big-endian.
                                (setf initial-encoding :utf-16be)
                                (decode-ascii-encoded-declaration 2 2 1))))
                        ((and (= b1 #xEF) (= b2 #xBB) (= b3 #xBF))
                         ;; UTF-8 BOM.
                         (setf initial-encoding :utf-8)
                         (decode-ascii-encoded-declaration 3 1 0))
                        ;;
                        ;; Without a byte order mark, check for ASCII ';'.
                        ((and (= b1 #x3B) (= b2 #x00) (= b3 #x00) (= b4 #x00))
                         (setf initial-encoding :ucs-4le)
                         (decode-ascii-encoded-declaration 0 4 0))
                        ((and (= b1 #x00) (= b2 #x3B) (= b3 #x00) (= b4 #x00))
                         (decode-ascii-encoded-declaration 0 4 1))
                        ((and (= b1 #x00) (= b2 #x00) (= b3 #x3B) (= b4 #x00))
                         (decode-ascii-encoded-declaration 0 4 2))
                        ((and (= b1 #x00) (= b2 #x00) (= b3 #x00) (= b4 #x3B))
                         (setf initial-encoding :ucs-4be)
                         (decode-ascii-encoded-declaration 0 4 3))
                        ;;
                        ;; Check for ASCII ';;'.
                        ((and (= b1 #x3B) (= b2 #x00) (= b3 #x3B) (= b4 #x00))
                         (setf initial-encoding :utf-16le)
                         (decode-ascii-encoded-declaration 0 2 0))
                        ((and (= b1 #x00) (= b2 #x3B) (= b3 #x00) (= b4 #x3B))
                         (setf initial-encoding :utf-16be)
                         (decode-ascii-encoded-declaration 0 2 1))
                        ((and (= b1 #x3B) (= b2 #x3B))
                         (setf initial-encoding :utf-8-auto)
                         (decode-ascii-encoded-declaration 0 1 0))
                        ;;
                        ;; Check for UTF-7 ';'.
                        ((and (= b1 #x2B) (= b2 #x41) (= b3 #x44))
                         (setf initial-encoding :utf-7))
                        ;;
                        ;; Check for ISO-2022-KR.
                        ((and (= b1 #x1B) (= b2 #x24) (= b3 #x29) (= b4 #x43))
                         (setf initial-encoding :iso-2022-kr)
                         (decode-ascii-encoded-declaration 4 1 0))
                        ;;
                        ;; Check for EBCDIC ';;'.
                        ((and (= b1 #x5e) (= b2 #x5e))
                         ;; EBCDIC - TODO read the declaration to determine the code page.
                         (setf initial-encoding :ebcdic-us))
                        (t
                         ;; Not detected and no declaration, detect UTF-8.
                         (setf initial-encoding :utf-8-auto)))))
               ((= available 3)
                (let ((b1 (aref buffer 0))
                      (b2 (aref buffer 1))
                      (b3 (aref buffer 2)))
                  (cond ((and (= b1 #xEF) (= b2 #xBB) (= b3 #xBF))
                         ;; UTF-8 BOM.
                         (setf initial-encoding :utf-8))
                        (t
                         (setf initial-encoding :utf-8-auto)))))
               ((= available 2)
                (let ((b1 (aref buffer 0))
                      (b2 (aref buffer 1)))
                  (cond ((and (= b1 #xff) (= b2 #xfe))
                         ;; UTF-16, little-endian
                         (setf initial-encoding :utf-16le))
                        ((and (= b1 #xfe) (= b2 #xff))
                         ;; UTF-16, big-endian.
                         (setf initial-encoding :utf-16be))
                        (t
                         (setf initial-encoding :default)))))
               (t
                ;; Empty file - just use the default.
                (setf initial-encoding :default))))))
    ;;
    (cond ((and (not initial-encoding) (not declared-encoding))
          :default)
         ((or (and (not declared-encoding)
                   (eq initial-encoding :utf-8-auto))
              (equalp declared-encoding :utf-8-auto))
          (multiple-value-bind (valid-utf-8 utf-8-use utf-8-bom)
              (detect-utf-8 file)
            (cond ((and valid-utf-8 (or utf-8-use utf-8-bom))
                   :utf-8)
                  (t
                   :default))))
         ((or (not declared-encoding)
              (member initial-encoding '(:utf-16le :utf-16be
                                         :ucs-4le :ucs-4be)))
          ;; Use the detected encoding.
          initial-encoding)
         (t
          declared-encoding))))

;;; Based on code by pjb
(defun parse-emacs-variables (line)
  (when (search "-*-" line)
    (flet ((chunk (text start end) (string-trim " " (subseq text start end))))
      (loop
         :with start = (+ 3 (search "-*-" line))
         :with end = (or (search "-*-" line :start2 start) (length line))
         :with result = '()
         :for colon = (and (< start end) (position #\: line :start start))
         :while (and colon (< colon end))
         :do (let ((vend
                    (or (and (< (1+ colon) end)
                             (position #\; line :start (1+ colon) :end end))
                        end)))
               (push (intern (string-upcase (chunk line start colon)) "KEYWORD")
                     result)
               (push (chunk line (1+ colon) vend) result)
               (setf start (1+ vend)))
         :finally (return (nreverse result))))))
