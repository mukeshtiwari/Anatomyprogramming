open List

(*
type expr =
  | Number   of int
  | Add      of expr * expr
  | Subtract of expr * expr
  | Multiply of expr * expr
  | Divide   of expr * expr
  | Variable of string
  | Declare  of string * expr * expr 

let t =  Subtract ( ( Subtract ( Number 3, Number ( -2 ) ) ), Number ( -7 ) )

type token = 
  | Digit  of int
  | Symbol of string
 *)


type exp =
  | Literal of value
  | Unary of unaryop * exp
  | Binary of binaryop * exp * exp
  | If of exp * exp * exp
  | Variable of string
  | Declare  of string * exp * exp 
  | Function of string * exp
  | Call of exp * exp

and value = 
  | IntV of int
  | BoolV of bool
  | ClosureV of string * exp * env 

and env = ( string * value )  list

and binaryop =
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | GT
  | LT
  | LE
  | GE
  | EQ

and unaryop = 
  | Neg
  | Not



(*
type funct =
  | Function of ( string list ) * exp

type funenv = ( string * funct ) list 
type program = Program of funenv * exp 
*)


exception Zeroexception

let rec evaluate  expr  env  = 
  match expr with
   | Literal v            -> v 
   | Unary  ( op, a )     -> unary op  ( evaluate a env )
   | Binary ( op, a, b )  -> binary op ( evaluate a env ) 
				       ( evaluate b env )
   | Variable x           -> assoc x env
   | Declare ( x, exp, body ) -> 
      let
	 newenv = ( x, evaluate exp env ) :: env 
      in evaluate body newenv 
   | If ( a, b, c ) ->
      let
	 BoolV f = evaluate a env
      in if f then evaluate b env
	 else evaluate c env
   | Function ( x, body ) -> ClosureV ( x, body, env )
   | Call ( func, arg ) -> 
      let
	ClosureV ( x, body, cloenv ) = evaluate func env
      in let
	newenv = ( x, evaluate arg env ) :: cloenv
      in evaluate body newenv

and unary op value = 
  match ( op, value ) with 
  | ( Not, BoolV b ) -> BoolV ( not b )
  | ( Neg, IntV n ) -> IntV ( -n )

and binary op exp exp = 
  match ( op, exp, exp ) with
  | ( Add, IntV a, IntV b ) -> IntV ( a + b )
  | ( Sub, IntV a, IntV b ) -> IntV ( a - b )
  | ( Mul, IntV a, IntV b ) -> IntV ( a * b )
  | ( Div, IntV a, IntV b ) -> IntV ( a / b )
  | ( And, BoolV a, BoolV b ) -> BoolV ( a && b )
  | ( Or,  BoolV a, BoolV b ) -> BoolV ( a || b )
  | ( LT,  IntV a, IntV b ) -> BoolV ( a < b  )
  | ( LE,  IntV a, IntV b ) -> BoolV ( a <= b )
  | ( GE,  IntV a, IntV b ) -> BoolV ( a >= b )
  | ( GT,  IntV a, IntV b ) -> BoolV ( a > b  )
  | ( EQ,  IntV a, IntV b ) -> BoolV ( a = b  )



type typ = 
  | TInt
  | TBool
 

type tenv = ( string * typ )  list


let typecheck exp tenv = 
    match exp with
    | Literal ( IntV _  ) -> TInt
    | Literal ( BoolV _ ) -> TBool
    | Unary   ( op, a )   -> check_unary op ( typecheck a tenv )
    | Binary  ( op, a, b) -> check_binary op ( typecheck a tenv ) ( typecheck b tenv)
    | Variable x          -> assoc x tenv
    | Declare ( x, exp, body ) ->
       let
	  newenv = ( x, typecheck exp tenv ) :: env
       in typecheck body newenv
    | If (a, b, c ) ->
      if TBool = typecheck a tenv then 
	if typecheck b tenv = typecheck c tenv then
	   typecheck b tenv
	else TBool
      else TBool					 

and check_unary op typ = 
  match ( op, typ ) with
  | ( Not, TBool ) -> TBool
  | ( Neg, TInt  ) -> TInt
 

and check_binary op tyone tytwo = 
  match ( op, tyone, tytwo ) with
  | ( Add, TInt, TInt ) -> TInt
  | ( Sub, TInt, TInt ) -> TInt
  | ( Mul, TInt, TInt ) -> TInt
  | ( Div, TInt, TInt ) -> TInt
  | ( And, TBool, TBool ) -> TBool
  | ( Or,  TBool, TBool ) -> TBool
  | ( LT, TInt, TInt ) -> TBool
  | ( GT, TInt, TInt ) -> TBool


(*

substitute_one ( var, value ) expr  = 
  let rec subst exp = 
    match exp with
    | Number i            -> Number i 
    | Add      ( ef, es ) -> Add      ( subst ef, subst es )
    | Subtract ( ef, es ) -> Subtract ( subst ef, subst es )
    | Multiply ( ef, es ) -> Multiply ( subst ef, subst es )
    | Divide   ( ef, es ) -> Divide   ( subst ef, subst es )
    | Variable name       -> if var = name 
		             then Number value 
		             else Variable name
    | Declare ( x, exp', body ) -> 
       let 
	 body' = if x = var 
		 then body 
		 else subst body 
       in Declare ( x, ( subst exp' ), body')
      
  in subst expr




let w = substitute_one ( "x", 5 ) ( Add ( Variable "x", Number 2 ) )

let rename_one ( var, newvar ) expr = 
  let rec rename exp = 
    match exp with
    | Number i            -> Number i
    | Add      ( ef, es ) -> Add      ( rename ef, rename es )
    | Subtract ( ef, es ) -> Subtract ( rename ef, rename es )
    | Multiply ( ef, es ) -> Multiply ( rename ef, rename es )
    | Divide   ( ef, es ) -> Divide   ( rename ef, rename es )
    | Variable name -> if name = var 
		       then Variable newvar 
		       else Variable name
  in rename expr 

let w = rename_one ( "x", "y" ) ( Add ( Variable "x", Number 2 ) )



let substitute env expr = 
   let rec subst exp = 
    match exp with
    | Number i            -> Number i 
    | Add      ( ef, es ) -> Add      ( subst ef, subst es )
    | Subtract ( ef, es ) -> Subtract ( subst ef, subst es )
    | Multiply ( ef, es ) -> Multiply ( subst ef, subst es )
    | Divide   ( ef, es ) -> Divide   ( subst ef, subst es )
    | Variable name -> match mem_assoc name env with
		       | true  ->  Number ( assoc name env )
		       | false ->  Variable name
  in subst expr

let v = substitute [ ( "x", 5 ) ; ( "y", 10 ) ] 
           ( Add  ( Variable "x", Variable "y" )  )




 *)
