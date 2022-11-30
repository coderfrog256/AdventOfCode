(defvar *vowels* (list #\a #\e #\i #\o #\u))
(member #\e *vowels*)
(member #\h *vowels*)
(length (list 1 2 5))
(defvar mylist nil)
(setf mylist nil)
(print mylist)
(push 4 mylist)
(member 4 mylist)

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

(search "we" "Ah well")
(search "we" "Ah will")

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

(with-open-file (f "../input/day5.txt")
  (loop for line = (read-line f nil)
        while line
        counting (nice-string line) into nice
        finally (return nice)))

;;; Part 2

(defvar *temp-int* 2)
(incf *temp-int*)
(length "Hey")

(defun two-pairs (input)
  (loop with seen = (make-hash-table :test #'equal)
        for i from 1 upto (- (length input) 1)
        for j from 0
        for char-i = (char input i)
        for char-j = (char input j)
        do (if (> (incf (gethash (list char-j char-i) seen 0)) 1)
               (return-from two-pairs t))
        do (when (equal char-i char-j)
             (incf i)
             (incf j))
        finally (return nil)))

(two-pairs "abc")
(two-pairs "abac")
(two-pairs "abcabc")
(two-pairs "abab")
(two-pairs "yyy")
(two-pairs "yyyy")

(defun dupe-with-separator (input)
  (loop for i from 2 upto (- (length input) 1)
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

(is-nice-2 "qjhvhtzxzqqjkmpb")
(dupe-with-separator "qjhvhtzxzqqjkmpb")

(is-nice-2 "xxyxx")
(is-nice-2 "uurcxstgmygtbstg")
(is-nice-2 "ieodomkazucvgmuy")
