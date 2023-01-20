input =: }: 1!:1<'../input/day3.txt'
NB. Map input to a list of 1d offsets.
commands =: {{ (1 0 , 0 1, _1 0 ,: 0 _1) {~ '^>v<' i. y }}
NB. Sum up offsets. Use ~. to make list unique, and +/\ for partial sums.
]ANSWER_ONE =: # ~. +/\ commands input

actor_commands =: {{
    NB. Split commands by actor based on if an index is even/odd. Make a 3d list of actor offsets.
    parsed =. commands y
    even =. (=&0@(2&|)) i. # parsed
    santa_commands =. even # parsed
    robo_commands =. (-. even) # parsed
    santa_commands ,: robo_commands
}}
NB. Sum up offsets. Note the rank-2 to sum 2d lists independently, and ,/ to flatten the output.
]ANSWER_TWO =: # ~. ,/ +/\"2 actor_commands input
