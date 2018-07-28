(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type Expr = 
  | CstI of int
  | Var of string
  | Prim of string * Expr * Expr
  | If of Expr * Expr * Expr;;

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

let e4 = Prim("max", Prim("+", Var "b", CstI 5), Var "c");;

let e5 = Prim("min", e4, e3);;

let e6 = Prim("==", e1, e4);;

let e7 = Prim("==", CstI 6, CstI 6);;

let e8 = If(CstI 10, CstI 1, CstI 2);;

let e9 = If(CstI 0, CstI 1, CstI 2);;

let e10 = If(Var "a", CstI 11, CstI 22);;

(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "+" -> i1 + i2
        | "*" -> i1 * i2
        | "-" -> i1 - i2
        | "max" -> 
            if i1>i2 then i1 else i2
        | "min" -> 
            if i1<i2 then i1 else i2
        | "==" ->
            if i1 = i2 then 1 else 0
        | _            -> failwith "unknown primitive"
    | If(e1,e2,e3) ->
        let i1 = eval e1 env
        let e = if i1 = 0 then e3 else e2
        eval e env;;     

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;
let e4v  = eval e4 env;;
let e5v  = eval e5 env;;
let e6v  = eval e6 env;;
let e7v  = eval e7 env;;
let e8v  = eval e8 env;;
let e9v  = eval e9 env;;
let e10v  = eval e10 env;;
