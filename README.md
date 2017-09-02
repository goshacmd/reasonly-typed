# A simple type system built with ReasonML

## Build

Just build:

```
npm run build
```

Build + watch:

```
npm run watch
```

To run tests:

```
$ node lib/js/src/tests.js
OK: Proper literal types
OK: Proper type for number addition
OK: Error for adding a number with a string
OK: Properly inferred types of function arguments
```

The build/watch commands will also produce a `lib/js/src/demo.js` file which can be run with node:

```
$ node lib/js/src/demo.js
Program:

var x = 10
var name = "Gosha"
var y1 = (name + 2)
var y2 = (x + 2)
var y3 = (name - name)
var y4 = (x - 1)
var add0 = x => (1 + x)
var add1 = x => (x + 1)
var res1 = add1(5)
var res2 = x(5)
var add2 = x => (x + ":)")
var add3 = x => (x + x)
var sub1 = x => (x - 1)
var megaAdd = x => y => (x + y)

Types:

- megaAdd: number => number => number
- sub1: number => number
- add3: number => number
- res1: number
- add1: number => number
- add0: number => number
- y4: number
- y2: number
- name: string
- x: number
- +: number => number => number
- -: number => number => number

Errors:

- Type mismatch in '(name + 2)', expected a number, got a string
- Type mismatch in '(name - name)', expected a number, got a string
- In 'x(5)', 'x' is not a function
- Type mismatch in '(x + ":)")', expected a number, got a string
```

## Todo

* functions over multiple variables
* conditionals
* nullable types
* sum types
* generics
* arrays of A
* no-arg functions
