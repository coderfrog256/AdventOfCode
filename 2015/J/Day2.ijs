input =: ".>'x' chopstring"1 (}: > (LF chopstring (1!:1<'../input/day2.txt')))
get_dimensions =: {{+/ (2&* , <./) ({. * {:) > (0&|.; 1&|.; 2&|.) y}}
+/get_dimensions"1 input

get_ribbon =: {{(*/y) + (2* +/ 0 1 { /:~ y)}}
+/get_ribbon"1 input
