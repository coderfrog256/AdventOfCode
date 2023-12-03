Sp ← ⊜□ ≠ ,: : # Split on char
I ← Sp @\n &fras "../input/day2-test.txt"
Debug ← dump∘
≡(□ ≡([□: □:] : ⊃(parse ⊔ ⊏ 0)(⊏ 0 ⊔ ⊏ 1) Sp @ ⊔) Sp @, ⊔) Sp @; ⊔ ⊏ 1 Sp @: ⊔ ⊏ 0 I

# Draw the rest of the owl: You have color+count boxes.
# Find the max per color
# Enumerate the lines to get the game id (starting at 1)
# For part 2 you're done, for part 1 filter out those with more than the limit specified.
