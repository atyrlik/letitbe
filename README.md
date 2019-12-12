# letitbe
A toy interpreter written in OCaml.

This is based heavily on the code examples in https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/.

More operations and types have been added.

## Run
Compile with `make` and run with `./main.byte` (or, ideally, with `rlwrap ./main.byte`).

## Examples

```
> let x be 5 in 2 * x
10

> let answer be 6*7
answer:42

> if answer < 100 then "less" else "more"
"less"

> let f be function of x to x * x + 1
answer:42 f:fun 

> f 3
10

> let frec be rec function of x to if (x < 10) then x else frec (x / 2) 
answer:42 f:fun frec:recfun 

> frec 30
7
```
