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
                                      (* LOCATION AQUI *)
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

  | Let (X, tp, e1, e2) ->            (* (NS) Totalmente sugerido por Copilot. *)
    match typeInfer e1 with
    | Some t1 when t1 = tp -> 
      match typeInfer e2 with
      | Some t2 -> Some t2
      | None -> None
    | _ -> None

  | Atr (e1, e2) -> 
    match (typeInfer e1, typeInfer e2) with
    | (Some (Ref t1), Some t2) when t1 = t2 -> Some Unit  
    | _ -> None

  | New e1 ->                         
    match typeInfer e1 with
    | Some tp -> Some (Ref tp)
    | None -> None                    (* (NS) como pode ser qualquer tipo, coloquei None -> None porque teria que ser bem claro que NÃO tem nenhum tipo.*)

  | WhileDo (e1, e2) ->
    match typeInfer e1 with
    | Some Bool -> (
      match typeInfer e2 with
      | Some Unit -> Unit               
      | None -> None)                 (* (NS) deixei None -> None porque seria código, então: ou tem algo, ou não tem nada.*)
    | _ -> None
  
  | Seq (e1, e2) ->
    match typeInfer e1 with
    | Some Unit -> (
      match typeInfer e2 with
      | Some t2 -> Some t2
      | None -> None)                 (* (NS) deixei None -> None porque seria código, então: ou tem algo, ou não tem nada.*)
    | _ -> None

  | Read -> Some Int          
  
  | Print e1 ->                           
    match typeInfer e1 with
    | Some Int -> Some Unit          
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
  | TT -> None
  | FF -> None
  | N -> None
  | UnitValue -> None
  | Location _ -> None
  | X -> None
  | Read -> None
  | Print _ -> None

  | Let (X,tp,e1,e2) ->               (* (NS) Sugerido pelo Copilot. *)
    if value e1 then Some (subst X e1 e2) 
    else (match step e1 with 
      | None -> None
      | Some e1' -> Some (Let (X,tp,e1',e2)))

  | IFTE(TT,e2,e3) -> Some e2
  | IFTE(FF,e2,e3) -> Some e3
  | IFTE(e1,e2,e3) -> (match step e1 with 
      | None -> None
      | Some e1' -> Some (IFTE(e1',e2,e3)))

;;


(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
                                                                 (*SMALL-STEP*)         
(*-------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(* steps : term -> term *)
let rec steps (e:term) : term =
  match step e with
  | None -> e
  | Some e' -> steps e'

