module Chapter1

type AExpr = 
  | CstI of int
  | Var of string
  | Add of AExpr * AExpr
  | Mul of AExpr * AExpr
  | Sub of AExpr * AExpr;;

let e1 = Sub(Var "v", Add(Var "w", Var "z"));;
let e2 =  Mul(CstI 2,e1);;

let rec fmt e = 
    let format ope e1 e2 = sprintf "(%s %s %s)" (fmt e1) ope (fmt e2)
    match e with
    | CstI x -> string x
    | Var x -> x
    | Add(e1, e2) -> format "+" e1 e2
    | Mul(e1, e2) -> format "*" e1 e2
    | Sub(e1, e2) -> format "-"  e1 e2;;
    
let e1f = fmt e1;;
let e2f = fmt e2;;

let zero = CstI 0;;
let one = CstI 1;;
let rec simplify e =
    let apSimplify e1 e2 op = 
        let e1' = simplify e1
        let e2' = simplify e2
        if e1 <> e1' || e2 <> e2' then
            simplify <| op(e1',e2')
        else        
            op(e1', e2')

    match e with
    | Add(e1, CstI 0) -> simplify e1
    | Add(CstI 0, e2) -> simplify e2
    | Mul(_, CstI 0) -> zero
    | Mul(CstI 0, _) -> zero
    | Mul(e1, CstI 1) -> simplify e1
    | Mul(CstI 1, e2) -> simplify e2
    | Sub(e1, CstI 0) -> simplify e1
    | Sub(e1, e2) when e1 = e2 -> zero
    | Add(e1, e2) -> apSimplify e1 e2 Add
    | Mul(e1, e2) -> apSimplify e1 e2 Mul
    | Sub(e1, e2) -> apSimplify e1 e2 Sub
    | CstI x -> CstI x
    | Var x -> Var x;;

let e3 = Add(Var "x", zero);;
let e4 = Add(one, zero);;
let e5 = Mul(e4,e3);;
let check1 = (simplify e3) = Var "x";;
let check2 = (simplify e4) = one;;
let check3 = (simplify e5) = Var "x";;