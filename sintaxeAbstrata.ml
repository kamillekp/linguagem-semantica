(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
                                                                    (*TERMS*)         
(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
type term =
  (*Values*)
  N                                   (* (NS) "N of Int", "N of int", "N" ou não precisa de N? *)
  | TT
  | FF
  | Location of term                  (* (NS) Usei "term" só para preencher que tem que ir algo ai, mas não estou certa do quê. *)
  | UnitValue                         (* (NS) representando "()". *)

  (*Operations*)
  | Add of (term * term)
  | LessThan of (term * term)

  (*Terms*)
  | IFTE of (term * term * term) 
  | X                                 (* (NS) Como representar variável de entrada? *)
  | Let of (term * tp * term * term)  (* (NS) Let x:T = e1 in e2 --> x (term) T (tp) e1 (term) e2 (term). *)
  | Atr of (term * term)              (* (NS) := --> atribution. *)
  | New of term 
  | WhileDo of (term * term)
  | Seq of (term * term)              (* (NS) ; --> sequence *)
  | Read                              (* (NS) não faço ideia de como seria a chamada sem a representação do "()". *)
  | Print of term
;;


(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
                                                                    (*TYPES*)         
(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
type tp =
    Int 
  | Bool
  | Ref tp
  | Unit
;;


(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
                                                                 (*TYPE-INFER*)         
(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* typeInfer : term -> tp option *)
let rec typeInfer (e:term) : tp option =  
  match e with

  | N  -> Some Int                    (* (NS) não tenho certeza do N nem aqui e nem nos termos. *)
  | TT -> Some Bool 
  | FF -> Some Bool
  | Location e1 ->                    (* (NS) Location recebe um Int? A saída é a referência de um tipo qualquer ou ao end. de memória (Int)? *)
    match typeInfer e1 with
    | Some Int -> Some (Ref tp)
    | None -> None
  | UnitValue -> Some Unit

  | Add (e1, e2) -> 
    match (typeInfer e1, typeInfer e2) with
    | (Some Int, Some Int) -> Some Int
    | _ -> None
  | LessThan (e1, e2) ->
    match (typeInfer e1, typeInfer e2) with
    | (Some Int, Some Int) -> Some Bool
    | _ -> None

  | IFTE (e1,e2,e3) -> (
    match typeInfer e1 with
    | Some Bool -> (match (typeInfer e2, typeInfer e3) with
      | (Some t2, Some t3) -> 
        if t2=t3 then Some t2 
        else None
      | _                  -> None)
    | _         -> None) 
  | X -> Some tp                      (* (NS) Apenas não sei --> já questionando a diferença entre mim e um neandertal. *)
                                      (* (NF) Let aqui *)
  | Atr (e1, e2) -> 
    match (typeInfer e1, typeInfer e2) with
    | (Some t1, Some t2) when t1 = t2 -> Some t1
    | _ -> None
  | New e1 ->                         (* (NS) suponho que senha assim porque pode "criar" uma memória pra uma variável de qualquer tipo. *)
    match typeInfer e1 with
    | Some tp -> Some (Ref tp)
    | None -> None
  | WhileDo (e1, e2) ->
    match typeInfer e1 with
    | Some Bool -> (
      match typeInfer e2 with
      | Some t2 -> Unit               (* (NS) Talvez seja Unit por não retornar nada útil. Só vai ter código. *)
      | None -> None)
    | _ -> None
                                      (* (NF) Seq aqui *)
  | Read -> Some (Ref tp)             (* (NS) supondo que seja assim porque vai ler algum lugar da memória e não tenho certeza se deveria ser Int. *)
  | Print e1 ->                       (* (NS) supondo que seja assim porque não sei se o que vai imprimir. *)    
    match typeInfer e1 with
    | Some tp -> Some Unit          
    | _ -> None

(* value : term -> bool *)
let rec value (e:term) : bool =
  match e with
  | N           -> true
  | TT          -> true
  | FF          -> true
  | Location _  -> true
  | UnitValue   -> true
  | _           -> false

(* step : term -> term option *)
let rec step (e:term) : term option =  
  match e with 
  | 
;;


(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
                                                                 (*SMALL-STEP*)         
(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* steps : term -> term *)
let rec steps (e:term) : term =
  match step e with
  | None -> e
  | Some e' -> steps e'

