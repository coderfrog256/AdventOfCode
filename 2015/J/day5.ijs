input =: > cutLF 1!:1@:boxopen<'../input/day5.txt'
has_enough_vowels =: {{3<:#(e.&'aeiou'@:{&y"0 i.#y)#y}} NB. Build a copy of the strings with just vowels. Is it >= 3?
has_dupe_pair =: {{1 e.(=|.!.' ') y}} NB. Hook. Pass string and 1-shifted string into =. Any match?
has_bad_pairs =: {{1 e.('ab','cd','pq',:'xy')e."2}.((({&y)@(_1&+)),{&y)"0 i.$y}} NB. Break input into pairs. Check if a bad pair is a member
is_nice =: (1 1 0 -: has_enough_vowels,has_dupe_pair,has_bad_pairs)"1 NB. Run all checks. Make sure last is false and rest are true
+/ is_nice input NB. How many lines are nice?
