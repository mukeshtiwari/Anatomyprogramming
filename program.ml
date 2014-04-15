open List

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


exception Zeroexception

let rec evaluate  ( expr : expr ) : int  = 
  match expr with
   | Number i            -> i 
   | Add      ( ef, es ) -> evaluate ef  + evaluate es 
   | Subtract ( ef, es ) -> evaluate ef  - evaluate es 
   | Multiply ( ef, es ) -> evaluate ef  * evaluate es 
   | Divide   ( ef, es ) -> evaluate ef  / evaluate es 
	     

let substituteOne ( var  , value ) expr  = 
  let rec subst exp = 
    match exp with
    | Number i            -> Number i 
    | Add      ( ef, es ) -> Add      ( subst ef, subst es )
    | Subtract ( ef, es ) -> Subtract ( subst ef, subst es )
    | Multiply ( ef, es ) -> Multiply ( subst ef, subst es )
    | Divide   ( ef, es ) -> Divide   ( subst ef, subst es )
    | Variable name -> if var = name 
		       then Number value 
		       else Variable name
     | Declare ( x, exp', body ) -> 
       let 
	 body' = if x = var 
		 then body 
		 else subst body 
       in Declare ( x, ( subst exp' ), body')
      
  in subst expr


let w = substituteOne ( "x", 5 ) ( Add ( Variable "x", Number 2 ) )

let renameOne ( var, newvar ) expr = 
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

let w = renameOne ( "x", "y" ) ( Add ( Variable "x", Number 2 ) )

type env = ( string * int )  list

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

let w = substitute [ ( "x", 5 ) ; ( "y", 10 ) ] 
           ( Add  ( Variable "x", Variable "y" )  )


