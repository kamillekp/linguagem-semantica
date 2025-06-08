(* ============================================================================ *)
(* TESTS.ML - Arquivo separado para testes *)
(* ============================================================================ *)

open Main (* Importa todas as definições do Main.ml *)

(* Tipo para representar um caso de teste *)
type test_case = {
  name: string;
  expr: expr;
  input: int list;
  expected_result: expr option;
  expected_output: int list;
  expected_input_remaining: int list;
}

(* Função para comparar expressões (simplified) *)
let expr_equal (e1: expr) (e2: expr) : bool =
  match (e1, e2) with
  | (Num n1, Num n2) -> n1 = n2
  | (Bool b1, Bool b2) -> b1 = b2
  | (Unit, Unit) -> true
  | (Loc l1, Loc l2) -> l1 = l2
  | _ -> false

(* Função para executar um teste e verificar resultado *)
let run_single_test (test: test_case) : bool =
  let test_state = {
    store = [];
    input = test.input;
    output = [];
    next_loc = 0;
  } in
  
  Printf.printf "Executando: %s... " test.name;
  
  match eval test.expr test_state with
  | Some (result, final_state) ->
    let result_ok = match test.expected_result with
      | Some expected -> expr_equal result expected
      | None -> true (* Não verifica resultado específico *)
    in
    let output_ok = final_state.output = test.expected_output in
    let input_ok = final_state.input = test.expected_input_remaining in
    
    if result_ok && output_ok && input_ok then (
      Printf.printf "✓ PASSOU\n";
      true
    ) else (
      Printf.printf "✗ FALHOU\n";
      if not result_ok then Printf.printf "  - Resultado esperado vs obtido diferem\n";
      if not output_ok then Printf.printf "  - Output esperado: [%s], obtido: [%s]\n" 
        (String.concat "; " (List.map string_of_int test.expected_output))
        (String.concat "; " (List.map string_of_int final_state.output));
      if not input_ok then Printf.printf "  - Input restante diferente\n";
      false
    )
  | None ->
    Printf.printf "✗ ERRO DE EXECUÇÃO\n";
    false

(* ============================================================================ *)
(* DEFINIÇÃO DOS CASOS DE TESTE *)
(* ============================================================================ *)

let test_cases = [
  (* Testes básicos *)
  {
    name = "Soma simples";
    expr = Binop(Sum, Num 5, Num 3);
    input = [];
    expected_result = Some (Num 8);
    expected_output = [];
    expected_input_remaining = [];
  };
  
  {
    name = "Multiplicação complexa";
    expr = Binop(Mul, Binop(Sum, Num 2, Num 3), Num 4);
    input = [];
    expected_result = Some (Num 20);
    expected_output = [];
    expected_input_remaining = [];
  };
  
  {
    name = "Comparação menor que";
    expr = Binop(Lt, Num 5, Num 10);
    input = [];
    expected_result = Some (Bool true);
    expected_output = [];
    expected_input_remaining = [];
  };
  
  {
    name = "If-then-else verdadeiro";
    expr = If(Bool true, Num 42, Num 0);
    input = [];
    expected_result = Some (Num 42);
    expected_output = [];
    expected_input_remaining = [];
  };
  
  {
    name = "If-then-else falso";
    expr = If(Bool false, Num 42, Num 0);
    input = [];
    expected_result = Some (Num 0);
    expected_output = [];
    expected_input_remaining = [];
  };
  
  (* Testes de variáveis *)
  {
    name = "Let simples";
    expr = Let("x", TyInt, Num 42, Binop(Sum, Id "x", Num 8));
    input = [];
    expected_result = Some (Num 50);
    expected_output = [];
    expected_input_remaining = [];
  };
  
  {
    name = "Let aninhado";
    expr = Let("x", TyInt, Num 10, Let("y", TyInt, Num 20, Binop(Sum, Id "x", Id "y")));
    input = [];
    expected_result = Some (Num 30);
    expected_output = [];
    expected_input_remaining = [];
  };
  
  (* Testes de referências *)
  {
    name = "Referência simples";
    expr = Let("r", TyRef TyInt, New (Num 42), Deref (Id "r"));
    input = [];
    expected_result = Some (Num 42);
    expected_output = [];
    expected_input_remaining = [];
  };
  
  {
    name = "Atribuição";
    expr = Let("r", TyRef TyInt, New (Num 10), Seq(Asg(Id "r", Num 20), Deref (Id "r")));
    input = [];
    expected_result = Some (Num 20);
    expected_output = [];
    expected_input_remaining = [];
  };
  
  (* Testes de I/O *)
  {
    name = "Read simples";
    expr = Read;
    input = [42; 10];
    expected_result = Some (Num 42);
    expected_output = [];
    expected_input_remaining = [10];
  };
  
  {
    name = "Print simples";
    expr = Print (Num 42);
    input = [];
    expected_result = Some Unit;
    expected_output = [42];
    expected_input_remaining = [];
  };
  
  {
    name = "Read e Print";
    expr = Let("x", TyInt, Read, Print (Binop(Sum, Id "x", Num 10)));
    input = [5];
    expected_result = Some Unit;
    expected_output = [15];
    expected_input_remaining = [];
  };
  
  (* Testes de sequência *)
  {
    name = "Sequência de prints";
    expr = Seq(Print (Num 1), Print (Num 2));
    input = [];
    expected_result = Some Unit;
    expected_output = [1; 2];
    expected_input_remaining = [];
  };
  
  (* Teste de while simples *)
  {
    name = "While contador";
    expr = Let("x", TyRef TyInt, New (Num 3),
           Seq(
             Wh(Binop(Gt, Deref (Id "x"), Num 0),
                Seq(Print (Deref (Id "x")), 
                    Asg(Id "x", Binop(Sub, Deref (Id "x"), Num 1)))),
             Print (Num 0)
           ));
    input = [];
    expected_result = Some Unit;
    expected_output = [3; 2; 1; 0];
    expected_input_remaining = [];
  };
  
  (* Teste de fatorial simples (n=3) *)
  {
    name = "Fatorial de 3";
    expr = Let("n", TyInt, Num 3,
           Let("result", TyRef TyInt, New (Num 1),
           Let("i", TyRef TyInt, New (Id "n"),
             Seq(
               Wh(Binop(Gt, Deref (Id "i"), Num 0),
                  Seq(
                    Asg(Id "result", Binop(Mul, Deref (Id "result"), Deref (Id "i"))),
                    Asg(Id "i", Binop(Sub, Deref (Id "i"), Num 1))
                  )),
               Deref (Id "result")
             ))));
    input = [];
    expected_result = Some (Num 6);
    expected_output = [];
    expected_input_remaining = [];
  };
]

(* ============================================================================ *)
(* FUNÇÕES DE EXECUÇÃO DOS TESTES *)
(* ============================================================================ *)

let run_tests () =
  Printf.printf "=== EXECUTANDO TESTES ===\n\n";
  let total = List.length test_cases in
  let passed = List.fold_left (fun acc test ->
    if run_single_test test then acc + 1 else acc
  ) 0 test_cases in
  
  Printf.printf "\n=== RESUMO ===\n";
  Printf.printf "Total: %d\n" total;
  Printf.printf "Passou: %d\n" passed;
  Printf.printf "Falhou: %d\n" (total - passed);
  Printf.printf "Taxa de sucesso: %.1f%%\n" (100.0 *. (float_of_int passed) /. (float_of_int total));
  
  if passed = total then (
    Printf.printf "🎉 TODOS OS TESTES PASSARAM!\n";
    exit 0
  ) else (
    Printf.printf "❌ ALGUNS TESTES FALHARAM\n";
    exit 1
  )

(* Função principal *)
let () = run_tests ()