# 1.0.0alpha5
* Contracts
  - Contracts are Cortex's form of interfaces
  - Use a `contract [name] {}` block to define a contract
  - Can have types "follow" contracts: `bundle MyBundle follows X { ... }`
  - New type to indicate a value that follows a particular contract:
    > Ex. `let x: follows X = ...;`
  - Contracts can define functions that types must implement in order to follow the contract
    > Offers a guarantee that certain functions exist
  - Please note: There is a known issue with using contracts with list literals
    > Specifically, syntax like this: `let x: list<follows X> = [a, b, c, ...];` is undefined
    > This is planned to be fixed in a future alpha release, alongside new features
* Merging bundles and structs, plus `heap`/dereferencing
  - Bundles no longer exist. Everything is now a struct
  - Struct member functions can take in `this`, `&this`, or `&mut this`
  - Structs can be placed on the heap (acting as bundles did and creating a reference) with the `heap` keyword: `heap Box<number> {item: 5}`
  - The `heap` expression can be used with *any* value to place it on the heap - for example, `heap 5` creates a `&mut number` reference to the value `5` on the heap
  - You can dereference any reference with the unary dereference operator `@`: `@myReference` (read-only)
* Generics are now invariant (so for example `list<number>` is no longer a subtype of `list<number?>`)
* Struct names are no longer optional (can't use `struct ~{...}`)
* Bug fixes

# 1.0.0alpha3 and 1.0.0alpha4
* Critical bug fixes and minor API enhancements
* Range core datatype
* substring function

# 1.0.0alpha2
* Update to Rust 2024 edition
* Extensions:
  - Use the new `extend {type} {...}` block to add functions to a type
  - You can even extend core types, such as `list<T>` or `string`
  - For core types that are not heap-allocated (such as `number`, `bool`, `string`, etc.), the first argument must be `this` rather than `&this` or `&mut this`
    > These types are immutable
* String functions:
  - The following functions have been added, and can be called on strings:
    > The get index function (`"foo"[0]`)
    > `len` and `isEmpty`
    > `startsWith` and `endsWith`
    > `indexOf` and `contains`
    > `trim` and `replace`
    > `reverse`
    > `padStart` and `padEnd`
    > `repeat`
    > `split`
  - Currently, there is no built-in `substring` function (though you can implement it and even add an extension yourself)
    > This is planned for the next alpha release
* Char data type
  - New `char` data type and character literals (C-style `'A'`, `'\n'`, etc.)
  - Returned from or used in some string functions
  - Several functions exist on `char` as well:
    > `isAlpha`, `isDigit`, `isWhitespace`, `isAlphanumeric`
    > `toUpper`, `toLower`
* Tuples
  - Created like so: `(1, 2)`
  - Can have one element only: `(1,)` (trailing comma required in this case)
  - Tuple types: `let x: (number, bool) = (5, true);`
  - Tuple deconstruction:
    > `let (x, y) = (1, 3);`
    > `let ((x, y), flag) = ((1, 3), false);`
  - Reading tuple values:
    > `let a = (1, 2).t0;`
    > Members are always `t` and then the index of the member
    > Can assign to members as well
* Manually passing type arguments:
  - Ex. `genericFn<number>(5);` - previously, these were always inferred and manual syntax did not exist

## Future Plans:
* Contracts - Cortex's trait system, giving way to many more features
* Features implemented via contracts:
  - For loops and iterators
  - Operator overloading
  - Lambda functions
* Function "forwarding" in composites
* Break/continue/early-return
* Implementing import
* Plenty of other fixes and enhancements
