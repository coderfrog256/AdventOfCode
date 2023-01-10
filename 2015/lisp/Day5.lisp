(ql:quickload '(:str :cl-ppcre :binding-arrows :snakes :alexandria :parseq))
(defpackage :advent (:use :cl :cl-ppcre :binding-arrows))
(in-package :advent)

(defvar *vowels* (list #\a #\e #\i #\o #\u))

(defun at-least-three-vowels (str)
  (let ((vowel-count 0))
    (loop for char across str
          do (if (member char *vowels*)
                 (incf vowel-count))
          do (if (> vowel-count 2)
                 (return-from at-least-three-vowels t))
          finally (return nil))))

(at-least-three-vowels "acd")
(at-least-three-vowels "acdi")
(at-least-three-vowels "acedi")

(defun dupe-character (str)
  (loop with last = nil
        for char across str
        do (if (equal last char)
               (return-from dupe-character t))
        do (setf last char)
        finally (return nil)))

(dupe-character "ABC")
(dupe-character "abbc")
(dupe-character "aabc")
(dupe-character "abcc")

(defvar *bad-strings* (list "ab" "cd" "pq" "xy"))

(defun no-bad-strings (input)
  (loop for bad-string in *bad-strings*
        do (if (search bad-string input)
               (return-from no-bad-strings nil))
        finally (return t)))

(no-bad-strings "carl")
(no-bad-strings "taxi cab")

(defun nice-string (input)
  (and
   (at-least-three-vowels input)
   (dupe-character input)
   (no-bad-strings input)))

(nice-string "ugknbfddgicrmopn")
(nice-string "aaa")
(nice-string "jchzalrnumimnmhp")
(nice-string "haegwjzuvuyypxyu")
(nice-string "dvszwmarrgswjxmb")

(->> "../input/day5.txt"
  (str:from-file)
  (str:lines)
  (remove-if-not #'nice-string)
  (length))

;;; Part 2

(defun two-pairs (input)
  (loop with seen = (make-hash-table :test #'equal)
        for i from 1 below (length input)
        for j from 0
        for char-i = (char input i)
        for char-j = (char input j)

        do (if (> (incf (gethash (list char-j char-i) seen 0)) 1)
               (return-from two-pairs t))
           ;; If starting a three-character sequence, advance by two characters instead of one.
        do (when (and (< i (1- (length input))) (equal char-i char-j) (equal char-i (char input (1+ i))))
             (incf i)
             (incf j))
        finally (return nil)))

(two-pairs "xyxy")
(two-pairs "aabcdefgaa")
(two-pairs "aaa")
(two-pairs "aaaa")

(defun dupe-with-separator (input)
  (loop for i from 2 below (length input)
        for j from 0
        for char-i = (char input i)
        for char-j = (char input j)
        do (if (equal char-i char-j)
               (return-from dupe-with-separator t))
        finally (return nil)))

(dupe-with-separator "xyx")
(dupe-with-separator "cyz")
(dupe-with-separator "abcdefeghi")
(dupe-with-separator "aaa")

(defun is-nice-2 (input)
  (and
   (two-pairs input)
   (dupe-with-separator input)))

(two-pairs "qjhvhtzxzqqjkmpb")
(is-nice-2 "xxyxx")
(is-nice-2 "uurcxstgmygtbstg")
(is-nice-2 "ieodomkazucvgmuy")

(->> "../input/day5.txt"
  (str:from-file)
  (str:lines)
  (remove-if-not #'is-nice-2)
  (length))
