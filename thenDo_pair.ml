(* basic composition example *)

let cube x = (x*x*x);;
let sine x = sin(float_of_int x);;
let compose f g = fun x-> f (g x);;

let sineOfCube=compose sine cube;;
sineOfCube(3);;

(* ----------------------------------------------- *)
(* example of failed composition of simple logging *)

let cube' x=(x*x*x, "cube was called. ");;
let sine' x=(sin(float_of_int x), "sine was called. ");;
cube'(3);;
sine'(27);;
(* composition of (f,string) below does not work 

   sine'(cube'(3));;
   Error: This expression has type int * string
   but an expression was expected of type int

   let sineOfCube'=compose sine' cube';;
   Error: This expression has type int -> int * string
   but an expression was expected of type int -> int
   Type int * string is not compatible with type int 
*)

(* ----------------------------------------------- *)
(* example of composition with pair based logger *)

type ('a, 'b) logger =  'a * string ;;
let result t = fst t;;
let output t = snd t;;


let promoteType x = (x,"");;
promoteType 1;;
promoteType "string";;

let liftFunc f x = promoteType (f(x));;
liftFunc(sine);;

let cube'' x= (x*x*x, "Cube called on "^string_of_int(x)^". ");;

let sine'' x= (sin(float_of_int x), "Sine called on "^string_of_int(x)^". ");;

sine'' 27;;
liftFunc sine' 27;;

let composeWithLogging f g = fun x ->
  let gx=g(x) in
  let gs=output(gx) in
  let gr=result(gx) in
  let fy=f(gr) in
  let fs=output(fy) in
  let fr=result(fy) in
    (fr, gs^fs);;

let (<<) = composeWithLogging;;
let sineOfCube'' =  sine'' << cube'';;
sineOfCube''(3);;


let round'' x= (int_of_float(floor(x +. 0.5)), "Round called on "^string_of_float(x)^". ");;

let thenDo' t g= 
  let fo=output(t) in
  let fr=result(t) in
  let gx=g(fr) in
  let go=output(gx) in
  let gr=result(gx) in
    (gr, fo^go);;
let (>>) = thenDo';;

cube''(3) >> sine'' >> round'';;







