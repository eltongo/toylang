# toylang
A toy programming language implemented in Python.

# How to Use

Write your code to a file, and then run `python toylang.py source_file` to execute your code.

# Sample Code

```
// this is a comment

// declaring a variable called number, and assigning the value 5 to it
number = 5

// common math operators are supported
square = number * number
one = number / number
twice = number + number
remainder = number % 3
zero = number - number

// boolean values are supported too
higherThan25 = square > 25
notTrue = false

// if and while statements too
if !higherThan25 {
  // there's a print statement, and of course strings are also supported
  print "Not higher than 25"
}

while number < 50 {
  number = number + 5
  // numbers and booleans can be concatenated with strings
  print "Number is now " + number
}

// you can use 'or', 'and' operators to compose logical expressions
if higherThan25 or 2 + 5 > 6 and number >= 50 {
  print "Our condition is true"
}
```
