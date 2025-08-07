# Cortex Language Documentation
## Comments
Comments can be used by starting a line with '//' or enclosed within '/* */' like in most C-style languages. Whitespace is irrelevant; all statements must end with a semicolon anyway.

## Data Types
There are 6 built-in data types in Cortex: number, boolean, string, void, none, and list. Numbers are stored as a 64-bit floating point number (Rust's `f64`); booleans are stored as a boolean (1 byte); strings are stored in Rust `String`s (so all Rust `String` rules apply); and void and none each don't store any additional data. Void should be used for functions that return no data; none should indicate the explicit absense of data.

### References
References currently can only be created to values stored on the heap (which can happen explicitly with the `heap` keyword). References can be thought of like pointers in other languages. However, references are special in that they denote if a value is mutable or not - either `&T` or `&mut T`. Mutability propagates, so fields of structs you have an immutable reference to will also be immutable, and you cannot call functions that require `&mut this` on immutable references.

You can dereference any reference value with the `@` operator: 

    let x = heap 5;
    let y = @x; // y = 5

Keep in mind that this will *copy* data from the heap onto the stack. So for large structs, this may cause issues - only do this if you know what you're doing!

### List
Lists are a special data type. Internally, they use a Rust `Vec`. Lists require a type argument denoting which type they store. Lists can be created using list construction syntax:

    let myList = [1, 2, 3];

Lists are created on the heap, and therefore the type is a reference to the list. This is the actual type of the above list:

    let myList: &mut list<number> = [1, 2, 3];

Lists define a few functions:
* __indexGet (see below)
* __indexSet (see below)
* add(&mut this, item: T): void - Adds an item to the end of the list
* insert(&mut this, index: number, item: T): void - Adds an item at the specified index
* remove(&mut this, index: number): void - Removes the item at the specified index
* find(&this, item: T): number? - Returns the index of the specified item, or `none` if the value was not found
* contains(&this, item: T): bool - Returns `true` if the item is in the list, and `false` if not
* len(&this): number - Returns the length/size of the list

## Variables
You can declare a variable with syntax like the following:

    let x: number = 5;

As you can see, there are a few key things:
* `let` keyword (you can use `const` instead for a constant)
* The variable name
* The variable type
  - This can be omitted and the type will be inferred based on the initial value
* The initial value of the variable
* A semicolon - required to terminate *all* statements

## Optional Values
All variables have a type. There is syntax for creating something known as a *optional* type, which allows for any value of that base type, and also `none`. For example:

    let x: number? = 5;
    x = none;
    let y: number = 0;
    y = none; // There will be an error here

There are cases where you may want to assert that a variable with a optional type is not the none value. Use the bang operation like so:

    let x: number? = 5;
    let y: number = x!;

If you use the bang operation on a none value, the interpreter will return an error and stop execution.

## Operators
There are several binary operators you can use. Here is each one, and how they work:

### Addition
number + number OR string + string

Example: 

5 + 2 = 7

"foo" + "bar" = "foobar"

### Subtraction
number - number

Example:

5 - 2 = 3

### Multiplication
number * number OR number * string OR string * number

Example:

5 * 2 = 10

"foo" * 2 = "foofoo"

2 * "foo" = "foofoo"

### Division
number / number

Example:

5 / 2 = 2.5

### Remainder
number % number

Example:

5 % 2 = 1

### Logical And
bool && bool

Example:

true && false = false

### Logical Or
bool || bool

Example:

true || false = true

### Equals
any == any

Example:

5 == 2 = false

"foo" == "foo" = true

### Not Equals
any != any

Example:

5 != 2 = true

"foo" != "foo" = false

### Number Comparison Operators
number < number

number > number

number <= number

number >= number

Example:

5 < 2 = false

5 > 2 = true

5 <= 5 = true

5 >= 5 = true

## If Expressions
If expressions are *expressions*, meaning they evaluate to a value. If bodies are similar to function bodies, in that you can add an expression at the end (that isn't semicolon-terminated) to return that value. All arms of an if expression must return the same type (however, if an arm returns `none` and others do not, the interpreter can infer the type as a optional type rather than giving an error). If expressions look like this:

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

## Loops
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

## The Heap
You can allocate values on the heap explicitly, using the `heap` expression. Values are garbage-collected at runtime, so they don't have to be deallocated manually. Allocating on the heap is as simple as placing the word `heap` in front of the value:

    let x = heap 5;
    let time = heap Time {
        hours: 5,
        minutes: 10,
        seconds: 52,
    };

## Functions
Functions can be defined in a top-level context. Functions, like variables, do not have to be named (the only reason to not name a function is if your Rust code is handling it in a custom way; unnamed functions cannot be called from within Cortex code). Function definitions look like this:

    fn *name*(*param*: *type*, *param*: *type*, ...): *return_type* {
        // Body...
    }

You can omit the return type if the function returns void.

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

Functions can also take in type arguments, like structs. The syntax looks something like this:

    fn identity<T>(item: T): T {
        item
    }

This way, the function `identity` can be called and the function is generic.

You can call generic functions without explicitly providing the type arguments and they will be inferred, but you can also explicitly provide them:

    identity(5); // 5
    identity<string?>(none); // none

## Structs
You can define (or inject) data types using *structs*. Structs consist of fields of a certain type, and struct instances (or composite values) consist of the values of those fields. Keep in mind *all* non-values are pass-by-value, meaning that passing a value into a function or assigning it to a variable *duplicates* the fields of structs. Make sure structs are cheap to copy, and if not, give them a way to hook into your Rust implementation where the actual complex object is stored, or place them on the heap using a `heap` expression.

Define structs like so:

    struct Time {
        hour: number,
        minute: number,
        second: number,
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

Structs can also have functions defined on them, below the list of fields. You have access to a special variable called `this`, which refers to the current instance. For example:

    struct Time {
        hour: number,
        minute: number,
        second: number,

        fn getTotalSeconds(this): number {
            this.hour * 60 * 60 + this.minute * 60 + this.second
        }
    }

Now, you can call `getTotalSeconds` on any `Time` to perform the calculation:

    let time: Time = Time {
        hour: 2,
        minute: 5,
        second: 10,
    };
    time.getTotalSeconds() // returns 7510

For struct functions, you must `this` as the first argument. You cannot modify the calling variable if it is passed by value. Therefore, mutating the value stored in `this` will not have an impact on the value used to call the function. However, there is another variant of member functions that allows you to mutate the caller, in which the caller is passed by *reference*:

    struct Point {
        x: number,
        y: number

        fn increment(&mut this, x: number, y: number) {
            this.x += x;
            this.y += y;
        }

        fn get(&this): (number, number) {
            (this.x, this.y)
        }
    }

Here, we see that `Point`'s two functions take in a reference to itself. In the first case, it can be mutated. In the second, it returns its field values - but a reference to the struct must be used to perform this call. Structs are not coerced into references automatically! If a function uses `&this`, then that reference - and any member references - are immutable.

### Indexing
There are a few special member functions. Cortex features *index syntax*, allowing for things like so:

    let val = myList[5];
    myList[0] = 10;

Indexing is not special, and is really syntax sugar for calling one of two member functions: `__indexGet` and `__indexSet`. Therefore, by defining those functions on your own types, you can add index functionality. You can provide any parameters you like, even multiple parameters, for more index functionality:

    fn __indexGet(&this, i: number, s: string): string
    fn __indexSet(&mut this, i: number, s: string, value: string): void

The last parameter to `__indexSet` will be the value that is assigned.

## Generic Types
Structs can, like functions, have type arguments.

    struct Box<T> {
        item: T,

        fn get(&this): T {
            this.item
        }
        fn set(&mut this, value: T) {
            this.item = value;
        }
    }

You must explicitly provide type args when constructing a struct:

    let box = heap Box<bool> {
        item: false,
    };

## Extensions
You can define extensions on any type to add additional functions. Just don't define functions that already exist on that type - this will cause an error prior to execution.

    extend utils::Time {
        fn toString(&this): string {
            toString(this.hours) + ":" + toString(this.minutes) + ":" + toString(this.seconds)
        }
    }

    extend number {
        fn addTo(this, other: number): number {
            this + other
        }
    }

If you're extending a type that takes in type parameters, you'll need to define those after the `extend` keyword so that they're in scope:

    extend<T> Box<T> {
        // ...
    }

## Modules
Modules are a way to package types and functions (or even other modules) under a shared namespace. Items in modules require paths to access them. Here's an example of a module:

    module Time {
        struct Time {
            hour: number,
            minute: number,
            second: number,
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
    let time = Time::Time {
        hour: 1,
        minute: 0,
        second: 0,
    };
    let doubled = Time::addTimes(time, time);

Modules can be constructed and injected from outside of the interpreter, which is the most common way to interface your Cortex code with Rust code. However, you can use the syntax above to create modules in Cortex code itself.

## Throwing Errors
There are many errors that can naturally occur when running code. However, if you want to produce an error in your Cortex code, you can use the `throw` statement and provide any `CortexValue`. Here's an example:

    throw 5;

This will return an `InterpreterError::ProgramThrow` with the `CortexValue` provided by the user, so you can handle the error in your Rust code.
