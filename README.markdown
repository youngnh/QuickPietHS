I haven't tested this code, but if you'd like to, then the following will get you on your way:
    $ ghc --make Main.hs -o qp

which will then allow you to interpret QuickPiet scripts:
    $ ./qp filename.qp

where filename.qp is some script file you want to run

Good Luck!

__QuickPiet__

These commands follow the actions available with the Piet programming languages with some
small changes to allow algorithms to be tested without the need of creating valid Piet images.
Original Piet information can be found at http://www.dangermouse.net/esoteric/piet.html
 
Just as in Piet, this language spec assumes a single "infinite" stack and a linear command execution order.
Blank lines should be ignored.
An implicit "end" command is present at the bottom of the document.
 
 
`push X` Pushes the value of `X` onto the stack. `X` should be a positive integer
 
`pop` Pops the top value of the stack and discards
 
`duplicate` Pushes a copy of the top value of the stack onto the stack
 
`roll` Pops the top two values, and "rolls" the remaining stack entries to a depth equal to the second value popped ...
- By a number of rolls equal to the first value popped ...
- A single roll to depth n is defined as burying the top value on the stack n deep ...
- And bringing all values above it up by 1 place ...
- A negative number of rolls rolls in the opposite direction
 
`in` Read a single value from STDIN and push it onto the stack; characters are read as their ASCII value
 
`out` Pop the top value from the stack and output it to STDOUT in it's ASCII character value
 
`add` Pops the top two values, adds them, and pushes the result
 
`subtract` Pops the top two values, subtracts the top value from the second top value, and pushes the result
 
`multiply` Pops the top two values, multiplies them, and pushes the result
 
`divide` Pops the top two values, integer divides the second top value by the top value, and pushes the result
 
`mod` Pops the top two values, calculates the second top value modulo the top value, and pushes the result
 
`not` Replaces the top value of the stack with 0 if it is non-zero, and 1 if it is zero
 
`greater` Pops the top two values, pushes 1 on to the stack if the second top value is greater than the top value, 0 otherwise
 
`end` Stop program execution, values left on the stack are discarded
 
`# LINE COMMENT` Line comment must begin wit a # and may contains zero or more characters of any type
 
`:label` Line label must begin with a ":" character and at least one alpha-numeric character
 
`goto label label` Pops the top value from the stack ...
- If the value is equal to 1, program execution switches to the first label ...
- If the value equals 3, program execution switches to the second label ...
- If the value does not equal 1 or 3, program execution continues to the next line
