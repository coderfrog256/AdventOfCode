(ql:quickload "cl-ppcre")
(ql:quickload "str")
(ppcre:regex-replace "i" "Hei" "y")

(defvar line-with-slash "Hey \"there\"")
line-with-slash
(setf line-with-slash "Hey \\\"there\\\"")
(format t "~A~%" line-with-slash)

(char-code #\x)
(code-char 120)
(code-char (parse-integer "21" :radix 16))


(defun regex-hex-to-string (match hex)
  "For use in ppcre:regex-replace(-all), takes a match like \x## and decodes the hex"
  (declare (ignore match))
  (string
   (code-char
    (parse-integer hex :radix 16))))

(defun escaped-line-length (in)
  (let* ((no-quote (ppcre:regex-replace-all "\\\\\"" in "\""))
         (no-escape (ppcre:regex-replace-all "\\\\\\\\" no-quote "\\\\"))
         (no-special (ppcre:regex-replace-all "\\\\x([0-9a-f]{2})" no-escape #'regex-hex-to-string :simple-calls t)))
    (length no-special)))

(escaped-line-length "Hey")
(escaped-line-length line-with-slash)
(defvar line-with-escape "Woah\\x21")
(setf line-with-escape "Woah\\x21")
(length line-with-escape)
(ppcre:regex-replace-all "\\\\x[0-9a-f]{2}" line-with-escape "#")
(ppcre:regex-replace-all "\\\\x([0-9a-f]{2})" line-with-escape #'regex-hex-to-string :simple-calls t)
(escaped-line-length line-with-escape)
(escaped-line-length "Hey, \\\\")

(ppcre:regex-replace-all "\\\\x([0-9a-f]{2})" line-with-escape #'(lambda (match &rest groups)
                                                                   (print groups)
                                                                   (first groups))
                         :simple-calls t)

(str:substring 1 -1 "Howdy")

;; not 1344
;; not 1345
;; is 1342
(print (loop for raw in (str:lines (str:from-file "../input/day8.txt"))
             for line = (ppcre:regex-replace-all "\s" raw "")
             for stripped = (str:substring 1 -1 line)
             sum (length line) into raw-len
             sum (escaped-line-length stripped) into escape-len
             do (format t "~A -> ~A: ~D | ~D~%" line (escaped-line-length-debug stripped) (length line) (escaped-line-length stripped))
             finally (return (- raw-len escape-len))))

(with-open-file (f "../input/day8.txt")
  (loop for line = (read f nil)
        while line
        for stripped = (str:substring 1 -1 line)
        do (format t "~A~%" line)
        sum (escaped-line-length stripped) into out
        finally (return out)))


;; Macro to make escaped regexes less painful
(defun read-literal-string (stream delimiter arg)
  (declare (ignore arg))
  (loop for char = (read-char stream nil stream)
        when (eq char stream)
          do (error "hit end of stream")
        until (char= char delimiter)
        collect char into chars
        finally (return (coerce chars 'string))))


(defun escaped-line-length-debug (in)
  (let* ((no-escape (ppcre:regex-replace-all "\\\\\\\\" in "\\\\"))
         (no-quote (ppcre:regex-replace-all "\\\\\"" no-escape "\""))
         (no-special (ppcre:regex-replace-all "\\\\x([0-9a-f]{2})" no-quote #'regex-hex-to-string :simple-calls t)))
    no-special))

(set-dispatch-macro-character #\# #\" #'read-literal-string)
(defvar test #"kyogyogcknbzv\x9f\\\\e")
(escaped-line-length test)
(format t "~A~%" (escaped-line-length-debug test))
