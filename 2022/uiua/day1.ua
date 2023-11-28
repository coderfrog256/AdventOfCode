# Read the input, push clone onto stack. Get rolling windows of two characters, find which ones match "\n\n".
# Flip that bitmask to get the characters to keep in each group (0 indicates a split point)
# Append a 1 on to the matching bitmask, as it does not include the last character.
# Pass the group bitmask and input to group-by to split input
# Next for each group, unbox the string, split by newlines, parse each integer and sum the list.
# Use a fork to run two functions on this list.
# The first is a max reduction (part 1). The second sorts the list, takes the first three, and sums them (part 2)
⊃(/↥)(/+↙3⊏⍖.) ≡(/+ ⊜parse ≠, @\n ⊔) ⊜□ ⊂ ¬≡(≍ "\n\n") :1 ◫2 .&fras "../input/day1.txt"
