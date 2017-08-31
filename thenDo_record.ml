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
(* example of composition with record based logger *)


(* type ('a,'b) logger = {out:'a; res:'b};; 
   a nice advantage of monads... I simply reversed
   out and res in the above record to the logger type 
   below to match your example which has result first
*)

type ('a,'b) logger = {res:'b; out:'a };;
let output t = t.out;;
let result t = t.res;;

let promoteType x = {res=x; out=""};;

promoteType 1;;
promoteType "string";;

let liftFunc f x = promoteType (f(x));;

liftFunc sine;;
liftFunc sine' 27;;

let cube'' x= {res= x*x*x ; 
               out="Cube called on "^string_of_int(x)^". "};;
let sine'' x = {res=sin(float_of_int x); 
                out="Sine called on "^string_of_int(x)^". "};;

cube'' 3;;
sine'' 27;;

let composeWithLogging f g = fun x ->
  let gx=g(x) in
  let gs=output(gx) in
  let gr=result(gx) in
  let fy=f(gr) in
  let fs=output(fy) in
  let fr=result(fy) in
    (gs^fs, fr);;

let sineOfCube'' =  composeWithLogging sine'' cube'';;
sineOfCube''(3);;


let round'' x= {res=int_of_float(floor(x +. 0.5)); 
                out="Round called on "^string_of_float(x)^". "};;

let thenDo' t g= 
  let fo=output(t) in
  let fr=result(t) in
  let gx=g(fr) in
  let go=output(gx) in
  let gr=result(gx) in
    {out=fo^go; res=gr};;
let (>>) = thenDo';;

cube''(3) >> sine'' >> round'';;
promoteType(3) >> cube''>> sine'' >> round'';;










