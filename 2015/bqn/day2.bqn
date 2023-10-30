input ← •FChars "../input/day1.txt"
Solution1 ← { +´('('⊸= - ')'⊸=) 𝕩}
Solution2 ← { ⊑ 1+¯1 ⊐˜ +`('('⊸= - ')'⊸=) 𝕩 }
•Show Solution1 input
•Show Solution2 input

input ← "2x31x4"
'x' ((⊢-˜+`×¬)∘=⊔⊢) input
