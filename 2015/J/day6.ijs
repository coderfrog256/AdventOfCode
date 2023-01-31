NB. Truly awful J! I ended up resorting to a for. Very procedural, but at least the actual array transformations leverage array operations
input =: 1!:1<'../input/day6.txt'
parsed =: ".> cutLF ('turn off ';'0 ';'turn on ';'1 ';'toggle';'2 ';' through ';' ') stringreplace input

work =: {{
  NB. for_command. y do. x =. x (turn_off`turn_on`toggle) @.(0{command) }. command end.
  for_command. y do. x =. x (turn_off`turn_on`toggle) @.(0{command) }. command end.
  x
}}

turn_off =: {{
  'start_x start_y end_x end_y' =. y
  0 (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )} x
}}

turn_on =: {{
  'start_x start_y end_x end_y' =. y
  1 (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )} x
}}

toggle =: {{
  'start_x start_y end_x end_y' =. y
  subarray =. (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )
  toggled =. -. subarray { x
  toggled subarray} x
}}

+/, (1000 1000 $ 0) work parsed
NB. Part 2, redefine functions.
turn_off =: {{
  'start_x start_y end_x end_y' =. y
  subarray =. (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )
  updated =. 0>.-&1 subarray { x
  updated subarray} x
}}

turn_on =: {{
  'start_x start_y end_x end_y' =. y
  subarray =. (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )
  updated =. 1+ subarray { x
  updated subarray} x
}}

toggle =: {{
  'start_x start_y end_x end_y' =. y
  subarray =. (< (start_y+i.(1+end_y-start_y)) ; (start_x+i.(1+end_x-start_x)) )
  updated =. 2+ subarray { x
  updated subarray} x
}}
+/, (1000 1000 $ 0) work parsed
