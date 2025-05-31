(*TERMS*)
type term =
  (*Values*)
  Int of int (* Precisa representar os inteiros, se já tem int no Ocaml? *)
  | TT
  | FF

  (*Operations*)
  | Add of (term * term)
  | LessThan of (term * term)

  (*Terms*)
  | IFTE of (term * term * term) 
  (* | X --> como representar variável de entrada? *)
  | Let of (string * tp * term * term) (* Precisa de let, sendo que o Ocaml já tem?*)
  | Atr of (term * term) (* := --> atribution *)
  | New of term 
  (* | () *)
  | WhileDo of (term * term)
  | Seq of (term * term) (* ; --> sequence *)
  | Location of int (* ou Int?*)
  | Read (* Chamada: Read() ? *)
  | Print of term
;;

(*TYPES*)
type tp =
    Int 
  | Bool
  | Ref tp
  | Unit
;;


(* step : term -> term option *)
let rec step (e:term) : term option =  
  match e with     
  | IFTE(TT,e2,e3) -> Some e2
  | IFTE(FF,e2,e3) -> Some e3
  | IFTE(e1,e2,e3) -> (match step e1 with 
      | None -> None
      | Some e1' -> Some (IFTE(e1',e2,e3)));;
     

