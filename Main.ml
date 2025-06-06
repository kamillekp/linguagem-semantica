(* bop = binary operation *)
type bop =  
  | Sum | Sub | Mul | Div               (* operações aritméticas *)
  | Eq  | Neq | Lt | Gt                 (* operações relacionais *)
  | And | Or                            (* operações lógicas *)

(* tipo = tipos da linguagem *)
type tipo = 
  | TyInt                              (* inteiro *)
  | TyBool                             (* booleano *)
  | TyRef of tipo                      (* referência para algum tipo *)
  | TyUnit                             (* tipo unitário *)
  | TyList of tipo                     (* lista de elementos de algum tipo *)

(* expr = expressões da linguagem *)
type expr = 
  | Num of int                          (* número inteiro *)
  | Bool of bool                        (* booleano *)
  | Id of string                        (* identificador *)
  | If of expr * expr * expr            (* if-then-else *)
  | Binop of bop * expr * expr          (* operação binária *)
  | Wh of expr * expr                   (* while *)
  | Asg of expr * expr                  (* atribuição *)
  | Let of string * tipo * expr * expr  (* nome, tipo, valor, corpo *)
  | New of expr                         (* alocação de nova referência *)
  | Deref of expr                       (* desreferenciar uma referência *)
  | Unit                                (* referência à '()' *)
  | Seq of expr * expr                  (* sequência *)
  | Read                                (* Read [Unit/()] *)
  | Print of expr                       (* print *)

  | Nil                                 (* [] *)
  | Cons of expr * expr                 (* n :: l *)
  | Prefix of expr * expr               (* n.l *)
  | Suffix of expr * expr               (* l.n *)

(* typeInfer : expr -> tipo option *)
let rec typeInfer (e : expr) : tipo option =  
  match e with 
  | Num _       -> Some TyInt
  | Bool _      -> Some TyBool
  | Id _        -> None                 (* (NS) ID seria apenas um valor para (des)referenciar? *)

  | If (e1, e2, e3) -> 
    (match typeInfer e1 with
     | Some TyBool -> 
       (match (typeInfer e2, typeInfer e3) with
        | (Some t2, Some t3) when t2 = t3 -> Some t2
        | _ -> None)
     | _ -> None)

  | Binop (bop, e1, e2) -> 
    (match (typeInfer e1, typeInfer e2) with
     | (Some TyInt, Some TyInt) -> 
       (match bop with
        | Sum | Sub | Mul | Div -> Some TyInt
        | Eq | Neq | Lt | Gt -> Some TyBool
        | _ -> None)
     | (Some TyBool, Some TyBool) -> 
       (match bop with
        | And | Or -> Some TyBool
        | _ -> None)
     | _ -> None)

  | Wh (e1, e2) -> 
    (match typeInfer e1 with
     | Some TyBool -> 
       (match typeInfer e2 with
        | Some TyUnit -> Some TyUnit
        | _ -> None)
     | _ -> None)

  | Asg (e1, e2) -> 
    (match typeInfer e1 with
     | Some (TyRef t1) -> 
       (match typeInfer e2 with
        | Some t2 when t1 = t2 -> Some TyUnit
        | _ -> None)
     | _ -> None)

  | Let (name, ty, e1, e2) -> 
    (match typeInfer e1 with
     | Some t1 when t1 = ty -> typeInfer e2
     | _ -> None)

  | New e1 -> 
    (match typeInfer e1 with
     | Some t -> Some (TyRef t)
     | _ -> None)

  | Deref e1 -> 
    (match typeInfer e1 with
     | Some (TyRef t) -> Some t
     | _ -> None)

  | Unit -> Some TyUnit

  | Seq (e1, e2) -> 
    (match typeInfer e1 with
     | Some TyUnit -> typeInfer e2
     | _ -> None)

  | Read -> Some TyInt

  | Print e1 -> 
    (match typeInfer e1 with
     | Some TyInt -> Some TyUnit
     | _ -> None)

  | Nil -> Some (TyList TyInt)

  | Cons (e1, e2) -> 
    (match (typeInfer e1, typeInfer e2) with
     | (Some TyInt, Some (TyList TyInt)) -> Some (TyList TyInt)
     | _ -> None)

  | Prefix (e1, e2) -> 
    (match (typeInfer e1, typeInfer e2) with
     | (Some TyInt, Some (TyList TyInt)) -> Some (TyList TyInt)
     | _ -> None)

  | Suffix (e1, e2) -> 
    (match (typeInfer e1, typeInfer e2) with
     | (Some (TyList TyInt), Some TyInt) -> Some (TyList TyInt)
     | _ -> None)

(* value : expr -> bool *)
let rec value (e : expr) : bool =
  match e with
  | Num _ | Bool _ -> true
  | _ -> false

(* step : expr -> expr option *)                      
let rec step (e : expr) : expr option =  
  match e with 
  | Num _ | Bool _ | Unit | Id _ -> None

  | If (Bool true, e2, _) -> Some e2
  | If (Bool false, _, e3) -> Some e3
  | If (e1, e2, e3) -> 
    (match step e1 with
     | Some e1' -> Some (If (e1', e2, e3))
     | None -> None)

  | Binop (Sum, Num v1, Num v2) -> Some (Num (v1 + v2))
  | Binop (Sub, Num v1, Num v2) -> Some (Num (v1 - v2))
  | Binop (Mul, Num v1, Num v2) -> Some (Num (v1 * v2))
  | Binop (Div, Num v1, Num v2) -> Some (Num (v1 / v2))
  | Binop (Eq, Num v1, Num v2) -> Some (Bool (v1 = v2))
  | Binop (Neq, Num v1, Num v2) -> Some (Bool (v1 <> v2))
  | Binop (Lt, Num v1, Num v2) -> Some (Bool (v1 < v2))
  | Binop (Gt, Num v1, Num v2) -> Some (Bool (v1 > v2))
  | Binop (And, Bool b1, Bool b2) -> Some (Bool (b1 && b2))
  | Binop (Or, Bool b1, Bool b2) -> Some (Bool (b1 || b2))
  | Binop (bop, v1, e2) when value v1 -> 
    (match step e2 with
     | Some e2' -> Some (Binop (bop, v1, e2'))
     | None -> None)
  | Binop (bop, e1, e2) -> 
    (match step e1 with
     | Some e1' -> Some (Binop (bop, e1', e2))
     | None -> None)

  | Wh (e1, e2) -> Some (If (e1, Seq (e2, Wh (e1, e2)), Unit))

  (*| Asg ->*)

  | Let (string, tipo, e1, e2) ->
    (match step e1 with
     | Some e1' -> Some (Let (string, tipo, e1', e2))
     | None -> None)
  (* (NF) LET PARA APLICAÇÃO *)

  (*| New () ->
  | Deref () ->
  | Unit () ->
  | Seq () ->
  | Read () ->                       
  | Print () ->*)
  
  | Cons (Num n, Nil) -> Some (Cons (Num n, Nil))
  | Cons (Num n, e2) -> 
    (match step e2 with
     | Some e2' -> Some (Cons (Num n, e2'))
     | None -> None)
  | Cons (e1, e2) -> 
    (match step e1 with
     | Some e1' -> Some (Cons (e1', e2))
     | None -> None)

  | Prefix (Num n, Nil) -> Some (Cons (Num n, Nil))
  | Prefix (Num n, e2) -> 
    (match step e2 with
     | Some e2' -> Some (Prefix (Num n, e2'))
     | None -> None)
  | Prefix (e1, e2) -> 
    (match step e1 with
     | Some e1' -> Some (Prefix (e1', e2))
     | None -> None)

  | Suffix (Nil, Num n) -> Some (Cons (Num n, Nil))
  | Suffix (Cons (Num h, t), Num n) -> 
    Some (Cons (Num h, Suffix (t, Num n)))
  | Suffix (e1, e2) -> 
    (match step e1 with
     | Some e1' -> Some (Suffix (e1', e2))
     | None -> 
       (match step e2 with
        | Some e2' -> Some (Suffix (e1, e2'))
        | None -> None))



(* ----------------------------------------------- *)
(* Programa para calcular fatorial de um número    *)
(* ----------------------------------------------- *)

(*
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
let fat = 
  Let("x", TyInt, Read,                     (* let x: int = read() in *)
  Let("z", TyRef TyInt, New (Id "x"),       (* let z: ref int = new x in *)
  Let("y", TyRef TyInt, New (Num 1),        (* let y: ref int = new 1 in *)
  seq)))                                    (* while (!z > 0) (
                                               y := !y * !z;
                                               z := !z - 1);
                                             print (!y) *)
*)