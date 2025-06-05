(* Definição dos tipos de operações binárias *)
type bop =  
  | Sum | Sub | Mul | Div   (* operações aritméticas *)
  | Eq  | Neq | Lt | Gt     (* operações relacionais *)
  | And | Or                (* operações lógicas *) 

(* Definição dos tipos de dados *)
type tipo = 
  | TyInt                   
  | TyBool                   
  | TyRef of tipo            
  | TyUnit                   

(* Definição das expressões da linguagem *)
type expr = 
  | Num of int               
  | Bool of bool            
  | Id of string            

  | If of (expr * expr * expr)            (* if-then-else *)
  | Binop of (bop * expr * expr)          (* operação binária *)
  | Wh of (expr * expr)                   (* while *)
  | Asg of (expr * expr)                  (* atribuição *)
  | Let of (string * tipo * expr * expr)  (* nome, tipo, valor, corpo *)
  | New of expr                           (* alocação de nova referência *)
  | Deref of expr                         (* desreferenciar uma referência *)
  | Unit                                  (* Referância à '()' *)
  | Seq of expr * expr                    (* sequência *)
  | Read                                  (* Read [Unit/()] *)
  | Print of expr           


(* typeInfer : expr -> tipo option *)
let rec typeInfer (e:expr) : tipo option =  
  match e with 
  | Num _       -> Some TyInt
  | Bool _      -> Some TyBool
  | Id _        -> Some string                        (* (NS) Identificadores não têm tipo definido, daí coloquei o tipo real. *) 

  | If (e1,e2,e3) -> (
    match typeInfer e1 with
      | Some Bool -> (
        match (typeInfer e2,typeInfer e3) with
          | (Some t2, Some t3) -> if t2=t3 then Some t2 
                                  else None
        | _ -> None)
    | _ -> None)  
    
  | Binop (op,e1,e2) -> (
    match (typeInfer e1, typeInfer e2) with
      | (Some TyInt, Some TyInt) -> (
        match op with
          | Sum | Sub | Mul | Div -> Some TyInt
          | Eq  | Neq | Lt | Gt -> Some TyBool
          | _ -> None)
      | (Some TyBool, Some TyBool) -> (
        match op with
          | And | Or -> Some TyBool
          | _ -> None)
      | _ -> None)

  | Wh (e1, e2) -> (
    match typeInfer e1 with
      | Some TyBool -> (
        match typeInfer e2 with
          | Some TyUnit -> Some TyUnit
          | _ -> None)
      | _ -> None)

  
  | Asg (Id _, e1) -> (
    match (typeInfer Id_,typeInfer e1) with
      | (Some (TyRef t1), Some t2) -> if t1 = t2 then Some TyUnit 
                                      else None
      | _ -> None)

  | Let (var, t, e1, e2) -> (                         (* (NS) não tenho certeza se a ordem de verificação está correta.*)
    match typeInfer var with
      | Some string -> (
        match (typeInfer t, typeInfer e1) with
          | (Some t1, Some t2) -> if t1 = t2 then 
                                    match typeInfer e2 with
                                      | Some t3 -> Some t3
                                      | _ -> None
                                  else None
          | _ -> None) 
      | _ -> None)

  | New e1 -> (
    match typeInfer e1 with
      | Some t -> Some (TyRef t)
      | _ -> None)
  
  | Deref e1 -> (
    match typeInfer e1 with
      | Some (TyRef t) -> Some t
      | _ -> None)
  
  | Unit -> Some TyUnit

  | Seq (e1, e2) -> (
    match typeInfer e1 with
      | Some TyUnit -> (
        match typeInfer e2 with
          | Some t2 -> Some t2
          | _ -> None)
      | _ -> None)

  | Read -> Some TyInt                  

  | Print e1 -> (
    match typeInfer e1 with
      | Some TyInt -> Some TyUnit
      | _ -> None)
   

(* value : expr -> bool *)
let rec value (e:expr) : bool =
  match e with
  | Num _ -> true
  | Bool _ -> true
  | _ -> false


(* step : expr -> expr option *)                      (* (NF) Ainda não comecei. *)
let rec step (e:expr) : term option =  
  match e with 
  |
;;
                      
(* steps : expr -> expr *)
let rec steps (e:expr) : expr =
  match step e with
  | None -> e
  | Some e' -> steps e'
  








(* ----------------------------------------------- *)
(* Programa para calcular fatorial de um número    *)
(* ----------------------------------------------- *)

(* !z > 0 *)
let cndwhi = Binop(Gt, Deref (Id "z"), Num 0)

(* y := !y * !z *)
let asgny = Asg(Id "y", Binop(Mul, Deref (Id "y"), Deref (Id "z")))
(* z := !z - 1 *)
let asgnz = Asg(Id "z", Binop(Sub, Deref (Id "z"), Num 1))
(* Executa a multiplicação e depois o decremento *)
let bdwhi = Seq(asgny, asgnz)

(* Enquanto !z > 0, executa o corpo *)
let whi = Wh(cndwhi, bdwhi)
(* imprime o valor atual de y *)
let prt = Print(Deref (Id "y"))
(* Sequência de comandos: executa o while e depois imprime o resultado *)
let seq = Seq(whi, prt)


(* Outra forma de fazer a mesma coisa *)
(*let fat = 
  Let("x", TyInt, Read,                     [let  x: int     =  read() in]
  Let("z", TyRef TyInt, New (Id "x"),       [let  z: ref int = new x in  ]  
  Let("y", TyRef TyInt, New (Num 1),        [let  y: ref int = new 1 in  ]  
  seq)))                                    [(while (!z > 0) (
                                                  y :=  !y * !z;
                                                  z :=  !z - 1);
                                                print (! y))             ]*)

  
    