
/*
Some helper functions for working with the Option type
*/

/* This is used with a function that retuns a definite type */
let map f => fun
| Some v => Some (f v)
| None => None;

/* This is used with a function that retuns another Option type */
let andThen (f: 'a => option 'b) => fun
| Some v => f v
| None => None;

let unwrapUnsafely = fun
| Some v => v
| None => raise (Invalid_argument "Passed `None` to unwrapUnsafely");
