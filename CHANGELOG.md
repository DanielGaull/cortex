# Release 0.1.0
* Resetting the changelog and versions with this release
* While release 0.1.0 represents the first release of Cortex; keep in mind the API remains non-stable
* Versioning will continue as 0.X.Y, instead of 1.0.0alphaX
* However, previous recommendations remain. This language and the Rust API are unstable! They will change frequently between major releases
  - As per [SemVer](https://semver.org/), upgrading from version 0.X.0 to 0.Y.0 can have breaking changes

## Future Plans:
* Features implemented via contracts ("contracts part 2"):
  - For loops and iterators
  - Operator overloading
  - Lambda functions
* Function/Type monomorphization
  - Main difference in usage will be consistency
  - Ex. can now follow the same contract multiple times if different type args are used
* Many small but very necessary features:
  - Static functions (allowing for constructors)
  - Replacing the built-in `list<T>` type with a standard library `List<T>` type - written in pure Cortex
  - Private fields and methods, and ability to prevent outsider from constructing using the literal construct syntax
  - Type aliases
  - `loop` syntax - just equivalent to `while (true)`
* Function "forwarding" in composites
* Break/continue/early-return
* Plenty of other fixes and enhancements
