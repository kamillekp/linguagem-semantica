# Notação 
## Regras Semânticas
e, σ, in, out → e', σ', in', out' 

- e = expressão atual
- σ (sigma) = store/memória (mapeamento de localizações para valores)
- in = lista de entrada (input)
- out = lista de saída (output)
- → = "transiciona para" (um passo de avaliação)
- e' = expressão após um passo
- σ', in', out' = novos estados da memória, entrada e saída

## Alguns símbolos do OCaml

- @ (concatenação de listas):  
    [1; 2] @ [3; 4] = [1; 2; 3; 4]  
    state.output @ [n]  (* adiciona n ao final da lista de saída *)

- :: (cons - adiciona elemento no início):  
    1 :: [2; 3] = [1; 2; 3]  
    n :: rest   (* n é o primeiro elemento, rest é o resto *)

- . (acesso a campo de record):  
    ocamlstate.input   (* acessa o campo 'input' do record 'state' *)  
    state.output  (* acessa o campo 'output' do record 'state' *)

# Memória
A linguagem trabalha em cima de um "estado global" que vai sendo alterado.
## Definição do Estado:

    type state = {  
        store: (location * value) list;  (* memória *) <-- A memória é um dicionário ou mapeamento de locais (endereços) para valores.  
        input: int list;                 (* entrada *)  
        output: int list;                (* saída *)  
        next_loc: int;                   (* próximo endereço livre *) <-- É um counter para o próximo endereço livre da memória.
    }  

## Operações
- σ[l ↦ v] = atualiza localização l com valor v
- σ(l) = obtém valor na localização l
- Dom(σ) = conjunto de localizações definidas

## Função de substituição
Após fazer uma atribuição a uma variável ela deve ser substituida pelo seu valor no seu escopo.  
Exemplo:  

    let x : int = 5 in (x + x)  
    (* Após avaliação de 5, substitui x por 5: *)  
    {5/x}(x + x) = (5 + 5)  

## Conversão de expressão para valor 
As expressões são a sintaxe da linguagem, fazem parte da AST (Árvore de Sintaxe Abstrata) e não são um valor em sí.  
Por isso a memória (store) deve armazenar value, não expr. Métodos auxiliares são usados para fazer essa conversão.  
Exemplo:  

    (* Expressões da linguagem *)
    type expr = Num of int | Bool of bool | ...

    (* Valores armazenados na memória *)
    type value = VNum of int | VBool of bool | VLoc of location | ...

## Ambiente de Tipos
É necessário manter informações sobre os tipos de variáveis para que a tipagem possa ser verificada dentro do escopo onde a variável existe. Quando uma variável é instânciada ela é adicionada ao ambiente de tipos.

# Exemplo de Programa

    (* Programa: let x = new 42 in !x *)
    let prog = Let ("x", TyRef TyInt, New (Num 42), Deref (Id "x"))

    (* Estado inicial *)
    let initial_state = {store = []; input = []; output = []}

    (* Execução passo a passo *)
    (* Passo 1: *)
    Let ("x", TyRef TyInt, New (Num 42), Deref (Id "x")), {store=[]; input=[]; output=[]}
    → Let ("x", TyRef TyInt, Location 0, Deref (Id "x")), {store=[(0,42)]; input=[]; output=[]}

    (* Passo 2: substituição *)
    → Deref (Location 0), {store=[(0,42)]; input=[]; output=[]}

    (* Passo 3: desreferenciamento *)
    → Num 42, {store=[(0,42)]; input=[]; output=[]}
