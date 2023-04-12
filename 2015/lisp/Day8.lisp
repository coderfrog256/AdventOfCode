(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defun regex-hex-to-string (match hex)
  "For use in ppcre:regex-replace(-all), takes a match like \x## and decodes the hex"
  (declare (ignore match))
  (string (code-char (parse-integer hex :radix 16))))

(defun escaped-line-length (in)
  (let* ((no-quote (ppcre:regex-replace-all "\\\\\"" in "\""))
         (no-escape (ppcre:regex-replace-all "\\\\\\\\" no-quote "\\\\"))
         (no-special (ppcre:regex-replace-all "\\\\x([0-9a-f]{2})" no-escape #'regex-hex-to-string :simple-calls t)))
    (length no-special)))

(defun file-escaped-line-length (in)
  (loop for line in (str:lines (str:from-file in))
        for stripped = (str:substring 1 -1 line)
        sum (escaped-line-length stripped) into escape-len
        sum (length line) into raw-len
        finally (return (- raw-len escape-len))))

(print (file-escaped-line-length "../input/day8-test.txt"))
(print (file-escaped-line-length "../input/day8.txt"))

;; Part 2
(defun unescaped-line-length (match)
  (let* ((escaped (ppcre:regex-replace-all "\\\\" match "\\\\\\\\") )
         (quoted (ppcre:regex-replace-all "\"" escaped "\\\"")))
    (+ 2 (length quoted))))

(defun file-unescaped-line-length (in)
  (loop for line in (str:lines (str:from-file in))
        sum (unescaped-line-length line) into unescaped-len
        sum (length line) into raw-len
        finally (return (- unescaped-len raw-len))))

(print (file-unescaped-line-length "../input/day8-test.txt"))
(print (file-unescaped-line-length "../input/day8.txt"))
