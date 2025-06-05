(* Definição dos tipos de operações binárias *)
type bop =  
  | Sum | Sub | Mul | Div   (* operações aritméticas *)
  | Eq  | Neq | Lt | Gt     (* operações relacionais *)
  | And | Or                 (* operações lógicas *) 

(* Definição dos tipos de dados *)
type tipo = 
  | TyInt                    (* inteiro *)
  | TyBool                   (* booleano *)
  | TyRef of tipo            (* referência a um tipo *)
  | TyUnit                   (* tipo unitário, sem valor *)

(* Definição das expressões da linguagem *)
type expr = 
  | Num of int               (* número inteiro *)
  | Bool of bool            (* valor booleano *)
  | Id of string            (* identificador de variável *)
  | If of (expr * expr * expr)  (* condicional: if-then-else *)
  | Binop of (bop * expr * expr) (* operação binária *)
  | Wh of (expr * expr)     (* laço while: condição e corpo *)
  | Asg of (expr * expr)    (* atribuição *)
  | Let of (string * tipo * expr * expr)  (* declaração de variável: nome, tipo, valor, corpo *)
  | New of expr             (* alocação de nova referência *)
  | Deref of expr           (* desreferenciar uma referência *)
  | Unit                    (* valor unitário *)
  | Seq of expr * expr      (* sequência de duas expressões *)
  | Read                    (* comando de leitura *)
  | Print of expr           (* comando de impressão *)

(* ----------------------------------------------- *)
(* Programa para calcular fatorial de um número    *)
(* ----------------------------------------------- *)

(* Condição do while: !z > 0 *)
let cndwhi = Binop(Gt, Deref (Id "z"), Num 0)
(* Interpretação: enquanto o valor de z for maior que zero *)

(* Primeiro comando dentro do while: y := !y * !z *)
let asgny = Asg(Id "y", Binop(Mul, Deref (Id "y"), Deref (Id "z")))
(* Interpretação: atualiza y multiplicando seu valor atual pelo de z *)

(* Segundo comando: z := !z - 1 *)
let asgnz = Asg(Id "z", Binop(Sub, Deref (Id "z"), Num 1))
(* Interpretação: decrementa z em 1 *)

(* Corpo do while: sequência de asgny e asgnz *)
let bdwhi = Seq(asgny, asgnz)
(* Interpretação: executa a multiplicação e depois o decremento *)

(* Estrutura while completa: condição e corpo *)
let whi = Wh(cndwhi, bdwhi)
(* Enquanto !z > 0, executa o corpo *)

(* Comando para imprimir o resultado: print(!y) *)
let prt = Print(Deref (Id "y"))
(* Interpretação: imprime o valor atual de y *)

(* Sequência de comandos: executa o while e depois imprime o resultado *)
let seq = Seq(whi, prt)

(* Programa principal *)
let fat = 
  Let("x", TyInt, Read,               (* lê um número e armazena em x *)
  Let("z", TyRef TyInt, New (Id "x"), (* cria referência z com valor de x *)
  Let("y", TyRef TyInt, New (Num 1),  (* cria referência y com valor inicial 1 *)
  seq)))                              (* executa a sequência: calcula fatorial e imprime *)

(*--------------------------------------------------DESCRIÇÃO EM CÓDIGO--------------------------------------------------------------------            
let  x: int     =  read() in 
let  z: ref int = new x in 
let  y: ref int = new 1 in 

(while (!z > 0) (
    y :=  !y * !z;
    z :=  !z - 1);
print (! y))     
-----------------------------------------------------------------------------------------------------------------------------------------*)

  
    