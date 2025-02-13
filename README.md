# cortex

Cortex is a Rust crate providing interactibility with a simple interpreted programming language called Cortex. This is not intended to be a standalone language, but one that works with Rust to give basic scripting functionality. It is easy to inject Cortex modules/structs/functions into the interpreter, to easily write Cortex code that calls back the source Rust code.

This file consists of documentation of the Cortex Language itself, and examples on how to use the tools provided in this package to run Cortex code.

## Cortex Language Documentation
### Comments
Comments can be used by starting a line with '//' or enclosed within '/* */' like in most C-style languages. Whitespace is irrelevant; all statements must end with a semicolon anyway.

### Data Types
There are 5 built-in data types in Cortex: number, boolean, string, void, and null. Numbers are stored as a 64-bit floating point number (Rust's `f64`); booleans are stored as a boolean (1 byte); strings are stored in Rust `String`s (so all Rust `String` rules apply); and void and null each don't store any additional data. Void should be used for functions that return no data; null should indicate the explicit absense of data.

### Variables
You can declare a variable with syntax like the following:

    let x: number = 5;

As you can see, there are a few key things:
* `let` keyword (you can use `const` instead for a constant)
* The variable name
* The variable type
  - This can be omitted and the type will be inferred based on the initial value
* The initial value of the variable
* A semicolon - required to terminate *all* statements

### Nullability
All variables have a type. There is syntax for creating something known as a *nullable* type, which allows for any value of that base type, and also `null`. For example:

    let x: number? = 5;
    x = null;
    let y: number = 0;
    y = null; // There will be an error here

There are cases where you may want to assert that a variable with a nullable type is not the null value. Use the bang operation like so:

    let x: number? = 5;
    let y: number = x!;

If you use the bang operation on a null value, the interpreter will return an error and stop execution.

### Operators
### If Statements
### Loops
### Functions
### Structs
### Modules

## Crate Documentation
