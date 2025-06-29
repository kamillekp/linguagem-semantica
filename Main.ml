(* Tipos básicos *)
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
  | TyArray of tipo                    (* array de elementos de algum tipo *)

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
  | Loc of int                          (* localização na memória *)

  | Nil                                 (* [] *)
  | Cons of expr * expr                 (* n :: l *)
  | Prefix of expr * expr               (* n.l *)
  | Suffix of expr * expr               (* l.n *)
  
  | MkArray of expr * expr  			(* MkArray(tamanho, valor_inicial) *)
  | Get of expr * expr      			(* Get(array, indice) para ler um valor *)

(*------------------------------------------------------------------------------------------------*)
(* AMBIENTE DE TIPOS - Necessário para Type Inference *)
(*------------------------------------------------------------------------------------------------*)
type type_env = (string * tipo) list

(* Busca tipo de uma variável no ambiente *)
let rec lookup_type (var: string) (env: type_env) : tipo option =
  match env with
  | [] -> None
  | (x, t) :: rest -> if x = var then Some t else lookup_type var rest

(* Adiciona uma variável e seu tipo ao ambiente *)
let extend_env (var: string) (ty: tipo) (env: type_env) : type_env =
  (var, ty) :: env

(* Função auxiliar para criar ambiente inicial *)
let empty_env : type_env = [] 

(*------------------------------------------------------------------------------------------------*)
(* typeInfer : expr -> tipo option *)
(*------------------------------------------------------------------------------------------------*)
let rec typeInfer (env: type_env) (e : expr) : tipo option =  
  match e with 
  | Num _       -> Some TyInt
  | Bool _      -> Some TyBool
  | Unit        -> Some TyUnit
  | Read        -> Some TyInt
  | Id x        -> lookup_type x env                  (* Variáveis, seu tipo é buscado no ambiente de tipos *)
  | Loc _       -> Some (TyRef TyInt)                 (* Localizações, a memória só armazena inteiros? *)

  (* T-if: Γ ⊢ e₁ : bool, Γ ⊢ e₂ : T, Γ ⊢ e₃ : T *)
  | If (e1, e2, e3) -> 
    (match typeInfer env e1 with
     | Some TyBool -> 
       (match (typeInfer env e2, typeInfer env e3) with
        | (Some t2, Some t3) when t2 = t3 -> Some t2
        | _ -> None)
     | _ -> None)

  (* T-op+, T-op< e outras operações *)
  | Binop (bop, e1, e2) -> 
    (match (typeInfer env e1, typeInfer env e2) with
     | (Some TyInt, Some TyInt) -> 
       (match bop with
        | Sum | Sub | Mul | Div -> Some TyInt           (* Operações aritméticas *)
        | Eq | Neq | Lt | Gt -> Some TyBool             (* Comparações *)
        | _ -> None)
     | (Some TyBool, Some TyBool) -> 
       (match bop with
        | And | Or -> Some TyBool                       (* Operações lógicas *)
        | Eq | Neq -> Some TyBool                       (* Comparações *)
        | _ -> None)
     | _ -> None)

  (* T-while: Γ ⊢ e₁ : bool, Γ ⊢ e₂ : unit *)
  | Wh (e1, e2) -> 
    (match typeInfer env e1 with
     | Some TyBool -> 
       (match typeInfer env e2 with
        | Some TyUnit -> Some TyUnit
        | _ -> None)
     | _ -> None)

  (* Em Main.ml, na função 'typeInfer', SUBSTITUA o bloco 'Asg' por este: *)

  | Asg (e1, e2) ->
      (match e1 with
        (* CASO 1: O lado esquerdo é um acesso de array, ex: arr[i] := ... *)
        | Get (arr_expr, idx_expr) ->
            (match (typeInfer env arr_expr, typeInfer env idx_expr) with
              | (Some (TyRef (TyArray t_arr)), Some TyInt) ->
                  (match typeInfer env e2 with
                    | Some t_val when t_arr = t_val -> Some TyUnit
                    | _ -> None)
              | _ -> None)
        
        (* CASO 2: O lado esquerdo é qualquer outra coisa (esperamos um Id que seja ref) *)
        | _ ->
            (match typeInfer env e1 with
              | Some (TyRef t1) ->
                  (match typeInfer env e2 with
                    | Some t2 when t1 = t2 -> Some TyUnit
                    | _ -> None)
              | _ -> None)
      )

  (* ... continue com os outros casos (Nil, Cons, etc) *)
  
  (* T-let: Γ ⊢ e₁ : T, Γ, x ↦ T ⊢ e₂ : T' *)
  (* Adicionamos a variável ao ambiente para verificar tipagem dentro do corpo (escopo da variável) *)
  | Let (name, ty, e1, e2) -> 
    (match typeInfer env e1 with
     | Some t1 when t1 = ty -> 
         let env' = extend_env name ty env in
         typeInfer env' e2
     | _ -> None)
  
  (* T-new: Γ ⊢ e : T *)
  | New e1 -> 
    (match typeInfer env e1 with
     | Some t -> Some (TyRef t)
     | _ -> None)

  (* T-deref: Γ ⊢ e : ref T *)
  | Deref e1 -> 
    (match typeInfer env e1 with
     | Some (TyRef t) -> Some t
     | _ -> None)
  
  (* T-seq: Γ ⊢ e₁ : unit, Γ ⊢ e₂ : T *)
  | Seq (e1, e2) -> 
    (match typeInfer env e1 with
     | Some TyUnit -> typeInfer env e2
     | _ -> None)

  (* T-print: Γ ⊢ e : int *)
  | Print e1 -> 
    (match typeInfer env e1 with
     | Some TyInt -> Some TyUnit
     | _ -> None)

  (* Inferência para operações de lista *)
  (* Lista vazia pode ser de qualquer tipo - por simplicidade assumimos TyInt *)
  | Nil -> Some (TyList TyInt)  (* Idealmente seria polimórfico *)

  (* Cons: primeiro elemento deve ser do mesmo tipo que os elementos da lista *)
  | Cons (e1, e2) -> 
    (match (typeInfer env e1, typeInfer env e2) with
     | (Some t1, Some (TyList t2)) when t1 = t2 -> Some (TyList t1)
     | _ -> None)

  (* Prefix: adiciona elemento no início (mesmo que Cons) *)
  | Prefix (e1, e2) -> 
    (match (typeInfer env e1, typeInfer env e2) with
     | (Some t1, Some (TyList t2)) when t1 = t2 -> Some (TyList t1)
     | _ -> None)

  (* Suffix: adiciona elemento no final *)
  | Suffix (e1, e2) -> 
    (match (typeInfer env e1, typeInfer env e2) with
     | (Some (TyList t1), Some t2) when t1 = t2 -> Some (TyList t1)
     | _ -> None)
	 
  (* T-MkArray: Γ ⊢ e₁ : int, Γ ⊢ e₂ : T  =>  Γ ⊢ MkArray(e₁, e₂) : ref (array T) *)
  | MkArray (size_expr, init_expr) ->
    (match typeInfer env size_expr with
      | Some TyInt -> 
        (match typeInfer env init_expr with
          | Some t -> Some (TyRef (TyArray t)) (* Retorna uma referência para o array *)
          | None -> None)
      | _ -> None)
	  
   (* T-Get: Γ ⊢ e₁ : ref (array T), Γ ⊢ e₂ : int  =>  Γ ⊢ Get(e₁, e₂) : T *)
  | Get (arr_expr, idx_expr) ->
    (match (typeInfer env arr_expr, typeInfer env idx_expr) with
      | (Some (TyRef (TyArray t)), Some TyInt) -> Some t
      | _ -> None)

(*------------------------------------------------------------------------------------------------*)
(* Função auxiliar para verificar tipos antes da execução *)
(*------------------------------------------------------------------------------------------------*)
let type_check (e: expr) : bool =
  match typeInfer empty_env e with
  | Some _ -> true
  | None -> false


(*------------------------------------------------------------------------------------------------*)
(* Definição do Estado Global *)
(*------------------------------------------------------------------------------------------------*)
type location = int

type value = 
  | VNum of int
  | VBool of bool
  | VUnit
  | VLoc of location
  | VList of value list
  | VArray of value array

type state = {
  store: (location * value) list;       (* memória: mapeamento localização -> valor *)
  input: int list;                      (* lista de entrada para read *)
  output: int list;                     (* lista de saída para print *)
  next_loc: int;                        (* próxima localização livre *)
}

(* Estado inicial *)
let initial_state = {
  store = [];
  input = [];
  output = [];
  next_loc = 0;
}

(*------------------------------------------------------------------------------------------------*)
(* Funções Auxiliares para Gerenciamento de Estado *)
(*------------------------------------------------------------------------------------------------*)
(* l ∉ Dom(σ) - aloca nova localização *)
let allocate (v: value) (state: state) : location * state =
  let l = state.next_loc in
  let new_store = (l, v) :: state.store in
  (l, {state with store = new_store; next_loc = l + 1})

(* l ∈ Dom(σ), σ(l) = v - busca valor em localização *)
let lookup (l: location) (state: state) : value option =
  List.assoc_opt l state.store

(* σ[l ↦ v] - atualiza localização *)
let update (l: location) (v: value) (state: state) : state =
  let new_store = (l, v) :: (List.remove_assoc l state.store) in
  {state with store = new_store}

(* Converte expressão para valor *)
let expr_to_value (e: expr) : value =
  match e with
  | Num n -> VNum n
  | Bool b -> VBool b
  | Unit -> VUnit
  | Loc l -> VLoc l
  | _ -> failwith "Not a value"

(* Converte valor para expressão *)
let value_to_expr (v: value) : expr =
  match v with
  | VNum n -> Num n
  | VBool b -> Bool b
  | VUnit -> Unit
  | VLoc l -> Loc l
  | VList _ -> failwith "Cannot convert list value to expression"
  | VArray _ -> failwith "Cannot convert array value to expression"

(*------------------------------------------------------------------------------------------------*)
(* Função de Substituição: {v/x}e *)
(*------------------------------------------------------------------------------------------------*)
let rec substitute (var: string) (replacement: expr) (expr: expr) : expr =
  match expr with
  | Num _ | Bool _ | Unit | Nil | Read | Loc _ -> expr
  
  | Id x -> if x = var then replacement else Id x
  
  | If (e1, e2, e3) -> 
      If (substitute var replacement e1,
          substitute var replacement e2,
          substitute var replacement e3)
  
  | Binop (op, e1, e2) -> 
      Binop (op, 
             substitute var replacement e1,
             substitute var replacement e2)
  
  | Wh (e1, e2) -> 
      Wh (substitute var replacement e1,
          substitute var replacement e2)
  
  | Asg (e1, e2) -> 
      Asg (substitute var replacement e1,
           substitute var replacement e2)
  
  | Let (x, ty, e1, e2) -> 
      let e1' = substitute var replacement e1 in
      let e2' = if x = var then e2 else substitute var replacement e2 in
      Let (x, ty, e1', e2')
  
  | New e -> New (substitute var replacement e)
  
  | Deref e -> Deref (substitute var replacement e)
  
  | Seq (e1, e2) -> 
      Seq (substitute var replacement e1,
           substitute var replacement e2)
  
  | Print e -> Print (substitute var replacement e)
  
  | Cons (e1, e2) -> 
      Cons (substitute var replacement e1,
            substitute var replacement e2)
  
  | Prefix (e1, e2) -> 
      Prefix (substitute var replacement e1,
              substitute var replacement e2)
  
  | Suffix (e1, e2) -> 
      Suffix (substitute var replacement e1,
              substitute var replacement e2)

  | MkArray (e1, e2) ->
      MkArray (substitute var replacement e1,
               substitute var replacement e2)
  
  | Get (e1, e2) ->
      Get (substitute var replacement e1,
           substitute var replacement e2)

(*------------------------------------------------------------------------------------------------*)
(* value : expr -> bool *)
(*------------------------------------------------------------------------------------------------*)
(* Verifica se uma expressão é um valor (ou seja, não pode ser mais reduzida) *)
let rec value (e : expr) : bool =
  match e with
  | Num _ | Bool _ | Unit | Loc _ -> true
  | Nil -> true
  | Cons (e1, e2) -> value e1 && value e2  (* Lista é valor se todos elementos forem valores *)
  | _ -> false

(*------------------------------------------------------------------------------------------------*)
(* step : expr -> state -> (expr * state) option *)
(*------------------------------------------------------------------------------------------------*)
let rec step (e : expr) (state : state) : (expr * state) option =  
  match e with 
  (* Valores não podem dar mais passos *)
  (*| Num _ | Bool _ | Unit | Id _ -> None*)
  | Num _ | Bool _ | Unit | Id _ | Nil | Loc _ -> None

  (* Regras para If *)
  | If (Bool true, e2, _) -> Some (e2, state)
  | If (Bool false, _, e3) -> Some (e3, state)
  | If (e1, e2, e3) -> 
    (match step e1 state with
     | Some (e1', state') -> Some (If (e1', e2, e3), state')
     | None -> None)

  (* Regras para operações binárias *)
  (* (op+): [[n₁]] = [[n₁]] + [[n₂]] *)
  | Binop (Sum, Num v1, Num v2) -> Some (Num (v1 + v2), state)
  | Binop (Sub, Num v1, Num v2) -> Some (Num (v1 - v2), state)
  | Binop (Mul, Num v1, Num v2) -> Some (Num (v1 * v2), state)
  | Binop (Div, Num v1, Num v2) -> Some (Num (v1 / v2), state)
  
  (* (op<true): [[n₁]] < [[n₂]] *)
  | Binop (Lt, Num v1, Num v2) -> Some (Bool (v1 < v2), state)
  | Binop (Gt, Num v1, Num v2) -> Some (Bool (v1 > v2), state)
  | Binop (Eq, Num v1, Num v2) -> Some (Bool (v1 = v2), state)
  | Binop (Neq, Num v1, Num v2) -> Some (Bool (v1 <> v2), state)
  
  (* Operações booleanas *)
  | Binop (And, Bool b1, Bool b2) -> Some (Bool (b1 && b2), state)
  | Binop (Or, Bool b1, Bool b2) -> Some (Bool (b1 || b2), state)
  | Binop (Eq, Bool b1, Bool b2) -> Some (Bool (b1 = b2), state)
  | Binop (Neq, Bool b1, Bool b2) -> Some (Bool (b1 <> b2), state)
  
  (* (op2): v op e₂ → v op e₂' *)
  | Binop (bop, v1, e2) when value v1 -> 
    (match step e2 state with
     | Some (e2', state') -> Some (Binop (bop, v1, e2'), state')
     | None -> None)
  
  (* (op1): e₁ op e₂ → e₁' op e₂ *)
  | Binop (bop, e1, e2) -> 
    (match step e1 state with
     | Some (e1', state') -> Some (Binop (bop, e1', e2), state')
     | None -> None)

  (* Regra para While *)
  | Wh (e1, e2) -> Some (If (e1, Seq (e2, Wh (e1, e2)), Unit), state)

  (* Regras para Let *)
  (* Cria variável*)
  (* 'ty' é o tipo de da variável, que é definido explicitamente na inicialização *)
  | Let (x, ty, v, e2) when value v ->                                   (* e-let2, associa valor à variável *)
      Some (substitute x v e2, state)
  | Let (x, ty, e1, e2) ->                                               (* e-let1, avalia expressão do lado esquerdo *)
    (match step e1 state with
     | Some (e1', state') -> Some (Let (x, ty, e1', e2), state')
     | None -> None)
  
  (* Regras para Atribuição *)
  (* Salva valor na memória *)
  (* Cria novo estado com local de mémoria 'l' atualizado com o valor de v *)
  (* AÇÃO 1: Atribuição a um índice de array. Lado esquerdo e direito são valores. *)
  | Asg (Get (Loc l, Num i), v) when value v ->
      (match lookup l state with
        | Some (VArray arr) ->
            if i < 0 || i >= Array.length arr then failwith "Index out of bounds"
            else
              let v_val = expr_to_value v in
              arr.(i) <- v_val;
              let new_store = (l, VArray arr) :: (List.remove_assoc l state.store) in
              Some (Unit, { state with store = new_store })
        | _ -> failwith "Assignment to a non-array location")

  (* AÇÃO 2: Atribuição a uma referência simples. Lado esquerdo e direito são valores. *)
  | Asg (Loc l, v) when value v ->
      let v_val = expr_to_value v in
      let state' = update l v_val state in
      Some (Unit, state')

  (* REDUÇÃO 1: Lado esquerdo é um endereço de array, avalia o lado direito. *)
  | Asg (Get (Loc l, Num i), e2) ->
      (match step e2 state with
        | Some (e2', state') -> Some (Asg (Get (Loc l, Num i), e2'), state')
        | None -> None)
  
  (* REDUÇÃO 2: Lado esquerdo é um endereço simples, avalia o lado direito. *)
  | Asg (Loc l, e2) ->
      (match step e2 state with
        | Some (e2', state') -> Some (Asg (Loc l, e2'), state')
        | None -> None)

  (* REDUÇÃO 3: O lado esquerdo ainda não é um endereço, então o avalia. *)
  | Asg (e1, e2) ->
      (match step e1 state with
        | Some (e1', state') -> Some (Asg (e1', e2), state')
        | None -> None)
  
  (* Regras para New *)
  (* Aloca nova localização na memória e retorna o ponteiro (Loc l) da localização e o estado atualizado *)
  | New v when value v ->                                                (* new1 *)
      let v_val = expr_to_value v in
      let (l, state') = allocate v_val state in
      Some (Loc l, state')
  | New e ->                                                             (* new *)
    (match step e state with
     | Some (e', state') -> Some (New e', state')
     | None -> None)

  (* Regras para Desreferenciamento *)
  (* *)
  | Deref (Loc l) ->                                                     (* deref1, lê o valor de 'l' de maneira não destrutiva *)
    (match lookup l state with
     | Some v -> Some (value_to_expr v, state)
     | None -> failwith ("Location not found: " ^ string_of_int l))
  | Deref e ->                                                           (* deref, avalia expressão *)
    (match step e state with
     | Some (e', state') -> Some (Deref e', state')
     | None -> None)

  (* Regras para Sequência *)
  (* Executa comandos em sequência *)
  (* Se 'e1' retornou 'Unit' (Null ou valor qualquer) já terminou, então 'e2' vira 'e1' e continua a execução. *)
  | Seq (Unit, e2) -> Some (e2, state)                                   (* seq1, prepara para a avaliação da próxima expressão *)
  | Seq (e1, e2) ->                                                      (* seq, avalia expressão inicial *)
    (match step e1 state with
     | Some (e1', state') -> Some (Seq (e1', e2), state')
     | None -> None)

  (* Regras para Print *)
  (* Concatena 'n' no final de output *)
  | Print (Num n) ->                                                     (* print-n, imprime número *)
      let state' = {state with output = state.output @ [n]} in
      Some (Unit, state')
  | Print e ->                                                           (* print, avalia 'e' *)
    (match step e state with
     | Some (e', state') -> Some (Print e', state')
     | None -> None)

  (* Regra para Read *)
  (* Extrai e retorna primeiro elemento do input (n), atualiza entrada com n removido (input = rest) *)
  | Read ->                                                              (* read *)
    (match state.input with
     | n :: rest -> 
         let state' = {state with input = rest} in
         Some (Num n, state')
     | [] -> failwith "No input available")
  
  (* Regras para lista*)
  (* Cons: constrói lista *)
  | Cons (v1, v2) when value v1 && value v2 -> None  (* Já é valor *)
  | Cons (v1, e2) when value v1 -> 
    (match step e2 state with
     | Some (e2', state') -> Some (Cons (v1, e2'), state')
     | None -> None)
  | Cons (e1, e2) -> 
    (match step e1 state with
     | Some (e1', state') -> Some (Cons (e1', e2), state')
     | None -> None)

  (* Prefix: adiciona no início (reduz para Cons) *)
  | Prefix (v1, v2) when value v1 && value v2 -> 
      Some (Cons (v1, v2), state)
  | Prefix (v1, e2) when value v1 -> 
    (match step e2 state with
     | Some (e2', state') -> Some (Prefix (v1, e2'), state')
     | None -> None)
  | Prefix (e1, e2) -> 
    (match step e1 state with
     | Some (e1', state') -> Some (Prefix (e1', e2), state')
     | None -> None)

  (* Suffix: adiciona no final *)
  | Suffix (Nil, v) when value v -> Some (Cons (v, Nil), state)
  | Suffix (Cons (h, t), v) when value h && value t && value v -> 
      Some (Cons (h, Suffix (t, v)), state)
  | Suffix (v1, v2) when value v1 && value v2 ->
      (* Caso geral para valores que não são listas *)
      None  (* Erro de tipo *)
  | Suffix (v1, e2) when value v1 -> 
    (match step e2 state with
     | Some (e2', state') -> Some (Suffix (v1, e2'), state')
     | None -> None)
  | Suffix (e1, e2) -> 
    (match step e1 state with
     | Some (e1', state') -> Some (Suffix (e1', e2), state')
     | None -> None)

  | Loc _ -> None  (* Locations são valores *)
  
  (* Avaliação do MkArray *)
  | MkArray (Num size, v) when value v -> (* mka-1, argumentos avaliados, pronto para alocar *)
      if size < 0 then failwith "Array size cannot be negative"
      else
        let v_val = expr_to_value v in
        let ocaml_array = Array.make size v_val in
        let (l, state') = allocate (VArray ocaml_array) state in
        Some (Loc l, state')

  | MkArray (Num size, init_expr) -> (* mka-2, avalia a expressão de inicialização *)
      (match step init_expr state with
        | Some (init_expr', state') -> Some (MkArray (Num size, init_expr'), state')
        | None -> None)

  | MkArray (size_expr, init_expr) -> (* mka-3, avalia a expressão de tamanho *)
      (match step size_expr state with
        | Some (size_expr', state') -> Some (MkArray (size_expr', init_expr), state')
        | None -> None)

  (* Avaliação do Get (leitura) *)
  | Get (Loc l, Num i) -> (* get-1, array e índice prontos, realiza a leitura *)
      (match lookup l state with
        | Some (VArray arr) -> 
            if i >= 0 && i < Array.length arr then
              Some (value_to_expr arr.(i), state)
            else
              failwith ("Index out of bounds: " ^ string_of_int i)
        | _ -> failwith "Trying to Get from a non-array location")
  
  | Get (Loc l, idx_expr) -> (* get-2, avalia a expressão do índice *)
      (match step idx_expr state with
        | Some (idx_expr', state') -> Some (Get (Loc l, idx_expr'), state')
        | None -> None)

  | Get (arr_expr, idx_expr) -> (* get-3, avalia a expressão do array *)
      (match step arr_expr state with
        | Some (arr_expr', state') -> Some (Get (arr_expr', idx_expr), state')
        | None -> None)

(* ----------------------------------------------- *)
(* Função auxiliar para executar múltiplos passos *)
(* ----------------------------------------------- *)
let rec rec_step (e: expr) (state: state) : expr * state =
  match step e state with
  | Some (e', state') -> rec_step e' state'
  | None -> (e, state)

let eval (e: expr) (state: state) : (expr * state) option =
  if type_check e then Some (rec_step e state)
  else None

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
let fat = 
  Let("x", TyInt, Read,                     (* let x: int = read() in *)
  Let("z", TyRef TyInt, New (Id "x"),       (* let z: ref int = new x in *)
  Let("y", TyRef TyInt, New (Num 1),        (* let y: ref int = new 1 in *)
  seq)))                                    (* while (!z > 0) (
                                               y := !y * !z;
                                               z := !z - 1);
                                             print (!y) *)


