input =: {{ {{ ". > y }}  1 2 { ':|' cutopen > y }}"0  (_1 }. LF chopstring freads '../input/day4.txt')
games_won =: {{ +/ (1 { y) e. (0 { y) }}"2 input
] part_one =: +/ <. 2 ^ games_won - 1

count_cards =: {{
    'wins remaining' =. y
    count =. {. remaining
    remaining =. }. remaining
    range =. i. ($ remaining) <. x
    adj =. count + (range { remaining)
    (wins + count) ; adj range} remaining
}}
] part_two =: (0;($ games_won) $ 1) >&(0&{)F.. count_cards games_won
