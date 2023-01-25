NB. Truly awful J! I ended up resorting to a for. Very procedural, even if the actual operations are still array ones.
input =: 1!:1<'../input/day6.txt'
parsed =: ".> cutLF ('turn off ';'0 ';'turn on ';'1 ';'toggle';'2 ';' through ';' ') stringreplace input

turn_off =: {{
  'ignored start_x start_y end_x end_y' =. y
  0 (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )} x
}}

turn_on =: {{
  'ignored start_x start_y end_x end_y' =. y
  1 (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )} x
}}

toggle =: {{
  'ignored start_x start_y end_x end_y' =. y
  subarray =. (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )
  toggled =. -. subarray { x
  toggled subarray} x
}}

work =: {{
  for_command. y do.
    select. {. command
        case. 0 do. x=. x turn_off command
        case. 1 do. x=. x turn_on command
        case. 2 do. x=. x toggle command
    end.
  end.
  x
}}
+/, (1000 1000 $ 0) work parsed

NB. Test cases (,: to make a list-of-list like input)
+/, (1000 1000 $ 0) work ,: 1 0 0 999 999
+/, (1000 1000 $ 0) work ,: 1 0 0 999 0
+/, (1000 1000 $ 0) work ,: 1 499 499 500 500
NB. Part 2.
turn_off_2 =: {{
  'ignored start_x start_y end_x end_y' =. y
  subarray =. (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )
  updated =. 0>.-&1 subarray { x
  updated subarray} x
}}

turn_on_2 =: {{
  'ignored start_x start_y end_x end_y' =. y
  subarray =. (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )
  updated =. 1+ subarray { x
  updated subarray} x
}}

toggle_2 =: {{
  'ignored start_x start_y end_x end_y' =. y
  subarray =. (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )
  updated =. 2+ subarray { x
  updated subarray} x
}}

work_2 =: {{
  for_command. y do.
    select. {. command
        case. 0 do. x=. x turn_off_2 command
        case. 1 do. x=. x turn_on_2 command
        case. 2 do. x=. x toggle_2 command
    end.
  end.
  x
}}
+/, (1000 1000 $ 0) work_2 parsed

NB. Part 2 tests
+/, (1000 1000 $ 0) work_2 ,: 1 0 0 0 0
+/, (1000 1000 $ 0) work_2 ,: 2 0 0 999 999
