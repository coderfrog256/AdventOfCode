W ← {"zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"}
D ← ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]
I ← ≡(□ ⬚@0↯[↥5⧻] .⊔) ⊜□ ≠ ,@\n &fras "../input/day1.txt"
PartOne ← /+ ≡(+⊃(×10 parse ⊏ 0)(parse ⊏ -1⧻) .▽ > @0 .▽ ≤ @9 .⊔) I
PartTwo ← /+ ≡(+⊃(×10 ⊏ 0)(⊏ -1⧻) .▽ ≠ 0 ./+ + ≡(× ⊙⊗ ⊙: :D ⌕ :⊙. ⊔:⊔) D: ≡(× ⊙⊗ ⊙□ ⊙: :W ⌕ :⊙. ⊔:⊔) W.) I

&p PartOne
&p PartTwo

Explanation ← $ Part one keeps all the characters > 0 and < 9. It uses a fork to run two functions on the filtered list.
              $ The first grabs the first char, parses it to an int, then multiplies it by ten.
              $ The second grabs the last char (len-1) and parses it. The two are added together.
              $ This is done in a per-row function, for each line of the input. The results are them summed together.
              $
              $ Part two involves calling find to discover if a word (W)/digit (D) matches. Find returns a bitmask of matching indexes.
              $ This bitmask is then mutiplied by the word/digit's index in its list, to get the a matrix with its numeric value.
              $ The two value matrices are then added together item-wise and summed row-wise into one array of all numbers.
              $ Lastly the array is processed identically to part one, with a fork to get the digits, summed to get the final value.
              $
              $ I had to do one hack, Uiua's find does not like the needle being longer than the haystack, so I pad out all input lines with 0s.
              $ This makes each line at least five characters long (longest word length), solving the issue.
