input =: cutpara fread '../input/day1-test.txt'
>./ +/"1 ".@}:@> input
+/ 3 {. (\:]) +/"1 ".@}:@> input
NB. Part 1 explanation: Split input into boxes based on paragraphs (two-newline delimited chunks!)
NB.    Build up a chain of operations, applied to each box index (elf)
NB.     1. Unbox item: >
NB.     2. Drop the trailing newline: }:
NB.     3. Parse to list of numbers: ".
NB.     4. Sum each row in matrix: +/"1
NB.     5. Take max of each item: >./
NB. Part 2 explanation: Start with same as above, but stop after row-wise summing.
NB.     1. Sort input ascending via a hook: (\:])
NB.     2. Take the first three elements: 3{.
NB.     3. Sum: +/
NB. For fun, define "p" as a side-effect (lol), refer to it in part 2 to reduce duplication
>./ ]p=: +/"1 ".@}:@> input
+/ 3 {. (\:]) p

cutpara fread '../input/day1-test.txt'
