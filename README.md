# cortex

Cortex is a Rust crate providing interactibility with a simple interpreted programming language called Cortex. This is not intended to be a standalone language, but one that works with Rust to give basic scripting functionality. It is easy to inject Cortex modules/structs/functions into the interpreter, to easily write Cortex code that calls back the source Rust code.

!! NOTE: This is a very unstable package! This tool is in the early stages of development, and breaking changes may be released. The goal is to allow for backwards compatibility at all time, but breaking changes may be unavoidable!

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
There are several binary operators you can use. Here is each one, and how they work:

#### Addition
number + number OR string + string

Example: 

5 + 2 = 7

"foo" + "bar" = "foobar"

#### Subtraction
number - number

Example:

5 - 2 = 3

#### Multiplication
number * number OR number * string OR string * number

Example:

5 * 2 = 10

"foo" * 2 = "foofoo"

2 * "foo" = "foofoo"

#### Division
number / number

Example:

5 / 2 = 2.5

#### Remainder
number % number

Example:

5 % 2 = 1

#### Logical And
bool && bool

Example:

true && false = false

#### Logical Or
bool || bool

Example:

true || false = true

#### Equals
any == any

Example:

5 == 2 = false

"foo" == "foo" = true

#### Not Equals
any != any

Example:

5 != 2 = true

"foo" != "foo" = false

#### Number Comparison Operators
number < number

number > number

number <= number

number >= number

Example:

5 < 2 = false

5 > 2 = true

5 <= 5 = true

5 >= 5 = true

### If Expressions
If expressions are *expressions*, meaning they evaluate to a value. If bodies are similar to function bodies, in that you can add an expression at the end (that isn't semicolon-terminated) to return that value. All arms of an if expression must return the same type (however, if an arm returns `null` and others do not, the interpreter can infer the type as a nullable type rather than giving an error). If expressions look like this:

    if *condition* {
        // Body...
    } elif *condition* {
        // Body...
    } elif *condition* {
        // Body...
    } else {
        // Body...
    };

You do not have to include any `elif` or `else` arms. However, if any arms return something other than `void`, then an `else` arm is required:

    if foo {
        5
    }; // Error: You need an else arm in this context

Being expressions, if expressions must be semicolon-terminated.
### Loops
You can construct while loops to repeat code (for loops currently do not exist). While loops have syntax like so:

    while *condition* {
        // statements...
    }

For example:

    let x = 0;
    while x < 10 {
        x += 1;
    }

Currently, there is no "break" construct to exit from loops early.

### Functions
Functions can be defined in a top-level context. Functions, like variables, do not have to be named (the only reason to not name a function is if your Rust code is handling it in a custom way; unnamed functions cannot be called from within Cortex code). Function definitions look like this:

    fn *name*(*param*: *type*, *param*: *type*, ...): *return_type* {
        // Body...
    }

The body consists of statements, but if you end the body with an expression (and no semicolon), the function will return that expression (otherwise, it returns void). There is no `return` statement in Cortex.

You can call a function with `name(args...)`. For example:

    fn add(n: number, m: number): number {
        n+m
    }
    let x = add(5,2); // x=7

Here is an unnamed function:

    fn ~(n: number, m: number): number {
        n*m
    }

This function cannot be called within Cortex, but you can use parse functions to read a `Function` object into the calling Rust code and use this function there.

### Structs
You can define (or inject) data types using *structs*. Structs consist of fields of a certain type, and struct instances (or composite values) consist of the values of those fields. Keep in mind *all* values are currently pass-by-value, meaning that passing a value into a function or assigning it to a variable *duplicates* the fields of structs. Make sure structs are cheap to copy, and if not, give them a way to hook into your Rust implementation where the actual complex object is stored. In the future, copy-by-reference values are planned to be added.

Define structs like so:

    struct Time {
        hour: number;
        minute: number;
        second: number;
    }

You can then create an instance like this:

    let time: Time = Time {
        hour: 5,
        minute: 5,
        second: 5,
    };

You can access and change fields like this:

    let hour = time.hour; // 5
    time.hour = 1;
    time.hour += 3; // time.hour = 4

The type name to refer to a struct is the struct name itself (or path if it is in a module).

### Modules
Modules are a way to package types and functions (or even other modules) under a shared namespace. Items in modules require paths to access them. Here's an example of a module:

    module time { // Convention is to use camelCase module names
        struct Time {
            hour: number;
            minute: number;
            second: number;
        }

        fn addTimes(t1: Time, t2: Time): Time {
            Time {
                hour: t1.hour + t2.hour,
                minute: t1.minute + t2.minute,
                second: t1.second + t2.second,
            }
        }
    }

    // From outside, we use the module like this:
    let time = time::Time {
        hour: 1,
        minute: 0,
        second: 0,
    };
    let doubled = time::addTimes(time, time);

Modules can be constructed and injected from outside of the interpreter, which is the most common way to interface your Cortex code with Rust code. However, you can use the syntax above to create modules in Cortex code itself.

### Throwing Errors
There are many errors that can naturally occur when running code. However, if you want to produce an error in your Cortex code, you can use the `throw` statement and provide any `CortexValue`. Here's an example:

    throw 5;

This will return an `InterpreterError::ProgramThrow` with the `CortexValue` provided by the user, so you can handle the error in your Rust code.

## Crate Documentation
