input =: 1!:1<'../input/day7.txt'

AND =: {{ #. ((16$2) #: x) *. ((16$2) #: y) }}
OR =: {{ #. ((16$2) #: x) +. ((16$2) #: y) }}
NOT =: {{ #. -. ((16$2) #: y) }}
parsed =: 3 : (('AND';' AND ';'OR';' OR ';'NOT';' NOT ';'33b.';'33 b.') stringreplace"1 ;"1> (_2&|.@;:@:,&' =: '&.>) cutLF ('LSHIFT';'(33 b.)~ ';'RSHIFT ';'(33 b.)~ _';' -> ';' ';'y';'yy') stringreplace input)

parsed
parsed ''

NB. This is as far as I got before things just got silly. it works for sequential input!
NB. But fails when there's dependencies that have not been met yet.
NB. One possible solution is sorting the operations in the order they need to be evaluated.
NB. There's also this hack involving shuffling I thought of (dependency sorting is better. This is worth revisiting when I'm better at J)

parsed =: ('AND';' AND ';'OR';' OR ';'NOT';' NOT ';'33b.';'33 b.') stringreplace"1 ;"1> (_2&|.@;:@:,&' =: '&.>) cutLF ('LSHIFT';'(33 b.)~ ';'RSHIFT ';'(33 b.)~ _';' -> ';' ';'y';'yy') stringreplace input
check =: {{ a >: 0 }} NB. The example doesn't have an "a" wire. Swap this out with "i" if you are running the test input
hack =: {{
     loops =: 0
     tries =. *: # y
     while. tries >: 0 do.
        loops =: loops + 1
        func =. 3 : y
        try.
            func ''
            if. check '' do. 1 return. end.
        catch.
            y =. y {~?~@# y
            tries =. tries - 1
        end.
     end.
     0
}}

NB. Part one, call "hack parsed", and look at the "a" variable.
hack parsed
a

NB. For part 2, restart the REPL, and run the below (replace the redefinition of b with the above result). Again look for the "a" variable.
hack ((('^b=:';'ignoreme=:') rxrplc"1 parsed), ('b=:3176'))
a
