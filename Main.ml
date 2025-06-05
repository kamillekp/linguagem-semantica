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


(* Outra forma de fazer a mesma coisa?*)
(*let fat = 
  Let("x", TyInt, Read,                     [let  x: int     =  read() in]
  Let("z", TyRef TyInt, New (Id "x"),       [let  z: ref int = new x in  ]  
  Let("y", TyRef TyInt, New (Num 1),        [let  y: ref int = new 1 in  ]  
  seq)))                                    [(while (!z > 0) (
                                                  y :=  !y * !z;
                                                  z :=  !z - 1);
                                                print (! y))             ]*)

  
    