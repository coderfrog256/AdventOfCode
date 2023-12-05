NB. Input array. first dimension is card, then 2xN where N is the count of owned numbers. Row 1 is winning, Row 2 is owned
input =: {{ {{ ". > y }}  1 2 { ':|' cutopen > y }}"0  (_1 }. LF chopstring freads '../input/day4.txt')
games_won =: {{ +/ (1 { y) e. (0 { y) }}"2 input NB. For each line, check if numbers in 2nd array are elements in 1st, Sum bitmask
] part_one =: +/ <. 2 ^ games_won - 1 NB. Part 1: For each result, raise 2^(result-1), absolute to remove losing rounds, sum the result.

count_cards =: {{                          NB. Wow, a fold!
    'cards remaining' =. y                 NB. Destructure the box, running card count is the first cell, array of remaining copies is 2nd.
    count =. {. remaining                  NB. Pull the current round's number of copies from thet top of the list
    remaining =. }. remaining              NB. Drop it from the remaining list
    range =. i. ($ remaining) <. x         NB. Range of adjustment vector. Min of matching numbers or remaining lines.
    adj =. count + (range { remaining)     NB. Adjustment vector. Take the number of cards you won, increment them by copy count.
    (cards + count) ; adj (i.x)} remaining NB. Box up state for next iteration, new running total and adjusted remaining list
}}
NB. Run fold. Initial state is 0 cards and all 1s remaining. run above function, finalizing by unboxing the first cell
] part_two =: (0;($ games_won) $ 1) >&(0&{)F.. count_cards games_won
