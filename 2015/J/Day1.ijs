data =: 1!:1 < '../input/day1.txt'
({:@:-&1,1&+@:i.&_1) (+/\1:`_1:@.=&')'"0 data)

NB. New find: \ does a scan for the preceeding operation.
NB. +/\ gets you a list of the sum as it's being applied.
NB. Also "0 is needed to do per-element mapping.
NB. Using a fork we can pass the list to get the last element (total sum) and
NB. the first element matching -1
NB. Not too happy with needing to -+1 the final functions. Noisy...
