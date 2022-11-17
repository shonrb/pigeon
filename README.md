# Pigeon
This is an interpreter for a toy programming language I designed. I built this as a programming exercise, and also to learn about language implementation and design. The name comes from linguistics, where a pidgin is an incomplete language with vocabulary and grammar from multiple other languages.

## Building
Build using cargo:

```cargo build --release```

## Features
Full code examples can be found in /examples

### Control flow
Pigeon is a structured language. It supports basic control flow structures:
```py
# Conditional 
if foo or bar {
    ...
} elif bar and baz {
    ...
} else {
    ...
}

# While
while foo {
    ...
}

# For 
range = 0 to 10
for i in range {
    ...
}
```
### Functions
Functions in Pigeon are first class citizens. They are created with a function literal and must be bound to a variable to be used as a named function 
```py
# literal
(arg1, arg2) -> {}

# Calling a function literal
() -> {
    println("foo")
}()

# Assigning to name
foo = () -> {
    println("bar")
}
foo()
```
### Variables & Typing
Variables are declared and assigned at the same time, similar to python or ruby. Pigeon is also dynamically typed, so variables can be assigned twice to different types.
```py
x = 1
x = true
x = "string"
```
Pigeon supports nine basic datatypes:
```py
x = 123                   # int
x = 12.3                  # float
x = 'c'                   # char
x = "str"                 # string
x = nothing               # nothingtype
x = true                  # bool
x = [1, "hello", nothing] # list
x = () -> {}              # function
```
The ninth datatype is ```typeid```, which allows runtime type checking and type conversion.
```py
t = function # typeid literal for a function type
x = 12
y = type(x) # y == int
z = type(y) # z == typeid
int(3.0) # 3
```
Pigeon is strongly typed, so conversions must be done explicitly in most cases.
```py
123 + 123.4    # Ok
123 + 'a'      # Error
123 + int('a') # Ok
```
Pigeon also supports duck typing.
```py
f = (a, b) -> {
    return a + b
}
println(f(2, 3))               # 5
println(f("Hello, ", "World!")) # Hello, World!
```
### Expressions
In Pigeon, every statement is an expression. Statements without a value default to ```nothing```.
```py
x = break # nothing
func = () -> {}
x = func() # nothing
x = () # Nothing
```
Scoped statements also evaluate to  ```nothing``` by default, but this can be changed using the ```yield``` keyword.
```py
x = if true {} else {}                   # x == nothing
x = if true { yield 0 } else { yield 1 } # x == 0
x = {
    y = 2
    yield y
} # x == 2
```
List comprehension is supported via ```yield``` in loop blocks
```py
i = 0
x = while i < 4 {
    i += 2
    yield i
}
# x == [2, 4]

x = for i in 0 to 5 { yield i }
# x == [0, 1, 2, 3, 4]

x = for i in x {}
# x == nothing
```
Assignments and return statements evaluate to their right hand side value
```py
x = y = 1  
# x is 1 and y is 1

x = (y += 1) + 2 
# x is 4 and y is 2

x = {
    yield y = 1
}
# x is 1 and y is 1

f = () -> {
    return y = 0
}
x = f()
# x is 0 and y is 0
```
## Improvements
This project is my first attempt at implementing a language, so naturally there's a lot of room for improvement. Here are some things I'd do differently if I were to do this project again.
* I'd implement a better parser by implementing some kind of generic grammar system, rather than just parsing directly in code. This would make it far quicker to add new language features, and would probably be more concise and less error-prone.
* I'd have a more elegant way of handling runtime errors, rather than just exit()'ing the program.
* This project consists of mostly my own solutions. I did this on purpose for the sake of learning, but it would be better to look further into how actual languages are implemented.