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
let rec expr_equal (e1: expr) (e2: expr) : bool =
  match (e1, e2) with
  | (Num n1, Num n2) -> n1 = n2
  | (Bool b1, Bool b2) -> b1 = b2
  | (Unit, Unit) -> true
  | (Loc l1, Loc l2) -> l1 = l2
  | (Nil, Nil) -> true
  | (Cons (h1, t1), Cons (h2, t2)) -> expr_equal h1 h2 && expr_equal t1 t2
  | (Prefix (a1, b1), Prefix (a2, b2)) -> expr_equal a1 a2 && expr_equal b1 b2
  | (Suffix (a1, b1), Suffix (a2, b2)) -> expr_equal a1 a2 && expr_equal b1 b2
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
  
match eval test.expr test_state, test.expected_result with
| None, None ->  (* Teste esperava falha de tipagem e falhou corretamente *)
    Printf.printf "✓ PASSOU\n";
    true
| None, Some _ ->
    Printf.printf "✗ ERRO DE EXECUÇÃO (esperava resultado válido)\n";
    false
| Some _, None ->
    Printf.printf "✗ ESPERAVA FALHA DE TIPO, MAS EXECUTOU\n";
    false
| Some (result, final_state), Some expected ->
    let result_ok = expr_equal result expected in
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
  (* Testes Esperando Some com Comportamento Correto *)
{
  name = "Sequência com efeitos colaterais";
  expr = Seq(Print (Num 1), Binop(Sum, Num 2, Num 3));
  input = [];
  expected_result = Some (Num 5);
  expected_output = [1];
  expected_input_remaining = [];
};

{
  name = "While com condição falsa inicial";
  expr = Wh(Bool false, Print (Num 1));
  input = [];
  expected_result = Some Unit;
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "New seguido de atribuição";
  expr = Let("r", TyRef TyInt, New (Num 0),
         Seq(Asg(Id "r", Num 42), Deref(Id "r")));
  input = [];
  expected_result = Some (Num 42);
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Read, cálculo e print";
  expr = Let("x", TyInt, Read,
         Print(Binop(Mul, Id "x", Num 2)));
  input = [6];
  expected_result = Some Unit;
  expected_output = [12];
  expected_input_remaining = [];
};
(* Testes com listas *)
{
  name = "Lista: construção simples";
  expr = Cons(Num 1, Cons(Num 2, Nil));
  input = [];
  expected_result = Some (Cons(Num 1, Cons(Num 2, Nil)));
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Prefix adiciona elemento na lista";
  expr = Prefix(Num 0, Cons(Num 1, Nil));
  input = [];
  expected_result = Some (Cons(Num 0, Cons(Num 1, Nil)));
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Suffix adiciona elemento no final da lista";
  expr = Suffix(Cons(Num 1, Nil), Num 2);
  input = [];
  expected_result = Some (Cons(Num 1, Cons(Num 2, Nil)));
  expected_output = [];
  expected_input_remaining = [];
};
(* Testes de tipo *)
{
  name = "Erro de tipo: soma Num e Bool";
  expr = Binop(Sum, Num 1, Bool true);
  input = [];
  expected_result = None;
  expected_output = [];
  expected_input_remaining = [];
};
{
  name = "Erro de tipo: atribuição incorreta";
  expr = Let("r", TyRef TyInt, New (Num 0), Seq(Asg(Id "r", Bool true), Deref (Id "r")));
  input = [];
  expected_result = None;
  expected_output = [];
  expected_input_remaining = [];
};
(* Teste shadowing de variáveis *)
{
  name = "Shadowing de variáveis";
  expr = Let("x", TyInt, Num 1,
           Let("x", TyInt, Num 2,
             Binop(Sum, Id "x", Num 1)));
  input = [];
  expected_result = Some (Num 3);  (* Deve usar o "x" mais interno *)
  expected_output = [];
  expected_input_remaining = [];
};
(* Teste com expressão sequencial e efeitos colaterais *)
{
  name = "Sequência com Print e Let";
  expr = Seq(
          Print (Num 10),
          Let("x", TyInt, Num 5, Binop(Sum, Id "x", Num 1))
        );
  input = [];
  expected_result = Some (Num 6);
  expected_output = [10];
  expected_input_remaining = [];
};
(* Teste para a expressão Unit em sequência *)
{
  name = "Sequência com Unit";
  expr = Seq(Unit, Num 42);
  input = [];
  expected_result = Some (Num 42);
  expected_output = [];
  expected_input_remaining = [];
};
(* Testes fatorial *)
{
    name = "Fatorial de 0";
    expr = fat;
    input = [0];
    expected_result = Some Unit;
    expected_output = [1];
    expected_input_remaining = [];
  };

  {
    name = "Fatorial de 1";
    expr = fat;
    input = [1];
    expected_result = Some Unit;
    expected_output = [1];
    expected_input_remaining = [];
  };

  {
    name = "Fatorial de 3";
    expr = fat;
    input = [3];
    expected_result = Some Unit;
    expected_output = [6];
    expected_input_remaining = [];
  };

  {
    name = "Fatorial de 5";
    expr = fat;
    input = [5];
    expected_result = Some Unit;
    expected_output = [120];
    expected_input_remaining = [];
  };
  (* Testes fat com funçoes diferentes *)
    {
    name = "Fatorial fixo: 0";
    expr =
      Let("y", TyRef TyInt, New (Num 1),
      Let("z", TyRef TyInt, New (Num 0),
      Seq(whi, prt)));
    input = [];
    expected_result = Some Unit;
    expected_output = [1];
    expected_input_remaining = [];
  };

  {
    name = "Fatorial fixo: 1";
    expr =
      Let("y", TyRef TyInt, New (Num 1),
      Let("z", TyRef TyInt, New (Num 1),
      Seq(whi, prt)));
    input = [];
    expected_result = Some Unit;
    expected_output = [1];
    expected_input_remaining = [];
  };

  {
    name = "Fatorial fixo: 3";
    expr =
      Let("y", TyRef TyInt, New (Num 1),
      Let("z", TyRef TyInt, New (Num 3),
      Seq(whi, prt)));
    input = [];
    expected_result = Some Unit;
    expected_output = [6];
    expected_input_remaining = [];
  };

  {
    name = "Fatorial fixo: 5";
    expr =
      Let("y", TyRef TyInt, New (Num 1),
      Let("z", TyRef TyInt, New (Num 5),
      Seq(whi, prt)));
    input = [];
    expected_result = Some Unit;
    expected_output = [120];
    expected_input_remaining = [];
  };
  (* Testes de Sucesso (Happy Path) *)
{
  name = "Array: Criação e leitura de valor inicial";
  expr =
    Let("arr", TyRef (TyArray TyInt), MkArray (Num 3, Num 0),
      Get (Id "arr", Num 1));
  input = [];
  expected_result = Some (Num 0);
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Array: Atribuição e leitura simples";
  expr =
    Let("a", TyRef (TyArray TyInt), MkArray (Num 5, Num 0),
      Seq(
        Asg(Get (Id "a", Num 2), Num 42),
        Get(Id "a", Num 2)
      ));
  input = [];
  expected_result = Some (Num 42);
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Array: Múltiplas atribuições e cálculo";
  expr =
    Let("a", TyRef (TyArray TyInt), MkArray (Num 10, Num 0),
      Seq(
        Asg(Get (Id "a", Num 0), Num 100),
        Seq(
          Asg(Get (Id "a", Num 9), Num 50),
          Binop(Sub, Get(Id "a", Num 0), Get(Id "a", Num 9))
        )
      ));
  input = [];
  expected_result = Some (Num 50);
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Array: Usando uma variável como índice";
  expr =
    Let("idx", TyInt, Num 3,
      Let("arr", TyRef (TyArray TyInt), MkArray (Num 5, Num 1),
        Seq(
          Asg(Get (Id "arr", Id "idx"), Num 99),
          Get(Id "arr", Num 3)
        )
      )
    );
  input = [];
  expected_result = Some (Num 99);
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Array: Iterar com While para preencher e somar";
  expr =
    Let("arr", TyRef (TyArray TyInt), MkArray (Num 5, Num 0),
      Let("i", TyRef TyInt, New (Num 0),
        Seq(
          Wh(Binop(Lt, Deref (Id "i"), Num 5),
            Seq(
              Asg(Get (Id "arr", Deref (Id "i")), Binop(Sum, Deref(Id "i"), Num 10)),
              Asg(Id "i", Binop(Sum, Deref (Id "i"), Num 1))
            )
          ),
          (* O array agora deve ser [10, 11, 12, 13, 14] *)
          Binop(Sum, Get(Id "arr", Num 1), Get(Id "arr", Num 4)) (* 11 + 14 *)
        )
      )
    );
  input = [];
  expected_result = Some (Num 25);
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Array: Criação de array de booleanos";
  expr =
    Let("bool_arr", TyRef (TyArray TyBool), MkArray(Num 2, Bool false),
      Seq(
        Asg(Get(Id "bool_arr", Num 1), Bool true),
        Get(Id "bool_arr", Num 1)
      )
    );
  input = [];
  expected_result = Some (Bool true);
  expected_output = [];
  expected_input_remaining = [];
};

(* Testes de Falha de Tipo (Sad Path) *)
{
  name = "Array Erro Tipo: Tamanho não é inteiro";
  expr = MkArray (Bool true, Num 0);
  input = [];
  expected_result = None;
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Array Erro Tipo: Atribuição de tipo incorreto";
  expr =
    Let("a", TyRef (TyArray TyInt), MkArray (Num 5, Num 0),
      Asg(Get (Id "a", Num 2), Bool false)
    );
  input = [];
  expected_result = None;
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Array Erro Tipo: Índice não é inteiro";
  expr =
    Let("a", TyRef (TyArray TyInt), MkArray (Num 5, Num 0),
      Get(Id "a", Bool true)
    );
  input = [];
  expected_result = None;
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Array Erro Tipo: Get em uma referência não-array";
  expr =
    Let("r", TyRef TyInt, New (Num 42),
      Get(Id "r", Num 0)
    );
  input = [];
  expected_result = None;
  expected_output = [];
  expected_input_remaining = [];
};

{
  name = "Array Erro Tipo: Let com tipo de array incorreto";
  expr =
    Let("arr", TyRef (TyArray TyBool), MkArray (Num 3, Num 0), (* Diz que é array de bool, mas inicializa com int *)
      Get (Id "arr", Num 1));
  input = [];
  expected_result = None; (* A verificação de `Let` deve falhar *)
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