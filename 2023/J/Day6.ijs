run =: {{ */ {{ +/ (1{y) {{ x < (y * time - y) }}"0 i. 1 + time =: 0 { y }}"1 |: y }}
inputp1 =: {{ ". > }. ' ' cutopen > y }}"0 (_1 }. LF chopstring freads '../input/day6.txt')
inputp2 =: {{ ". > }. ':' cutopen (' ';'') stringreplace > y }}"0 (_1 }. LF chopstring freads '../input/day6.txt')
run inputp1 NB. Part 1
run inputp2 NB. Part 2, same as above, just with spaces replaced and splitting on the : instead to merge the numbers.
