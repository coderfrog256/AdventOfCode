Input ← &fras "../input/day6.txt"

# Get bitmask where newline is an element of the input, negate it
# Partition by that bitmask (boxing chars), do a row-wise unbox.
# This gets you a matrix of characters, one row per line.
# NOTE: Input must have the same length on each line.
NewlineSplit ← ≡⊔ ⊜□ ¬ ∊, "\n"

# Sort input, classify (unique index per char), make a group from char to number of occurrences.
# Push the raw char values, and the number of occurrences onto the stack to be sorted.
CharCount ← ⊏ 1 ∶ ⊏ 0 .⍉ ⊕{⊢∶⧻.} ⊛.⊏ ⍏ .

# Sort the above either ascending or descending. Take the first and unbox it.
MostCommon ← ⊔ ⊢ ⊏ ⍖ CharCount
LeastCommon ← ⊔ ⊢ ⊏ ⍏ CharCount

# Perform the Most/Least common row-wise for each line in the transposed input.
≡MostCommon ⍉ NewlineSplit Input
≡LeastCommon ⍉ NewlineSplit Input
