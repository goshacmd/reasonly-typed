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

Either of these will produce a `lib/js/src/demo.js` file which can be run with node:

```
$ node lib/js/src/demo.js
Program:

var x = 10
var name = "Gosha"
var y1 = (name + 2)
var y2 = (x + 2)
var y3 = (name - name)
var y4 = (x - 1)
var add1 = x => (x + 1)
var res1 = add1(5)
var res2 = x(5)

Types:

- res1: any
- add1: any => any
- y4: number
- y2: number
- name: string
- x: number

Errors:

- Type mismatch in 'x(5)', expected a any => any, got a number
- Type mismatch in '(name - name)', expected a number, got a string
- Type mismatch in '(name + 2)', expected a string, got a number
```

## Todo

* functions over multiple variables
* refine types: (x => x + 1) should have a type of `number => number`, not `any => any`
* conditionals
