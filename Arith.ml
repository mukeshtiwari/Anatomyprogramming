open List
open Set

module SS = Set.Make(String)

type expr = Number of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Var of string


let rec eval_expr (s : expr -> int) = function
  | Var x -> s (Var x)  (* not found error *)
  | Number n -> n
  | Add (n1, n2) -> eval_expr s n1 + eval_expr s n2
  | Sub (n1, n2) -> eval_expr s n1 - eval_expr s n2
  | Mul (n1, n2) -> eval_expr s n1 * eval_expr s n2
  | Div (n1, n2) -> let n = eval_expr s n2 in 
    if n == 0 then raise (Failure "Divison by Zero")
    else eval_expr s n1 / n 


type var = string
type op = Plus | Mult
type exp = 
  | Int_e of int
  | Op_e of exp * op * exp
  | Var_e of var
  | Let_e of var * exp * exp 


let e : exp = Let_e ("a", Int_e 30, Let_e ("a", Let_e ("a", Int_e 3, Op_e(Var_e "a", Mult, Int_e 4)), Op_e(Var_e "a", Plus, Var_e "a")))
   

let rec free_var = function
| Int_e i -> SS.empty
| Var_e v -> SS.singleton v 
| Op_e(e1, _, e2) -> SS.union (free_var e1) (free_var e2)
| Let_e(v, e1, e2) -> SS.remove v (SS.union (free_var e1) (free_var e2)) 

let print_set s = 
  SS.iter print_endline s;;

let rec replace (x : var) (y : var) = function
| Int_e i  -> Int_e i 
| Var_e v ->  if v = x then Var_e y else Var_e v
| Op_e(e1, o, e2) -> Op_e(replace x y e1, o, replace x y e2)
| Let_e(v, e1, e2) -> if v = x then Let_e (y, e1, replace x y e2) else Let_e (v, replace x y e1, replace x y e2)


let rec rename : exp -> exp = function
| Int_e i  -> Int_e i 
| Var_e v ->  Var_e v
| Op_e(e1, o, e2) -> Op_e(rename e1, o, rename e2)
| Let_e(v, e1, e2) -> 
   let fv = (free_var e2) in  (* I am assuming here that v does not appear in e1. *)
   let r = string_of_int (1+ (Random.int 999999)) in 
   if SS.mem v fv then 
     Let_e(v ^ r, rename e1, rename (replace v (v ^ r) e2)) 
   else Let_e(v, rename e1, rename e2)     