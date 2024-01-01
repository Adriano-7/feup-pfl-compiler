# 2º Projeto PFL

Grupo T08_G02:
- Adriano Alexandre dos Santos Machado (up202105352) - [TBD]
- Tomás Alexandre Soeiro Vicente (up202108717) - [TBD]

## Descrição do trabalho
Este projeto encontra-se dividido em duas partes. Num primeiro momento, foi-nos pedido que implementássemos uma máquina de baixo nível que suportasse instruções de cálculo aritmético, de cálculo booleano e de controlo de fluxo. Posteriormente, foi-nos solicitado a implementação de um compilador, com a finalidade de compilar uma linguagem imperativa para a máquina de baixo nível previamente desenvolvida.

## Parte 1: Implementação de uma máquina de baixo nível
### Estrutura de Dados

**Inst:**

A estrutura de dados Inst representa as instruções da máquina.

```haskell
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
```
**State:**

O estado é representado por uma bst, onde cada nó contém uma chave (que corresponderá ao nome de uma variável), um valor associado e duas sub-árvores. 

```haskell	
data State = Empty 
            | Node String String State State
```
Esta estrutura de dados suporta as seguintes operações:
- newState: cria um novo estado vazio
- fromList: cria um novo estado a partir de uma lista de pares (variável, valor)
- insert: insere um novo par (variável, valor) no estado
- load: retorna o valor associado a uma variável
- toList: retorna uma lista de pares (variável, valor) a partir de um estado
- toStr: retorna uma string a partir de um estado

**Stack:**

A stack é representada por uma lista de strings. 

```haskell
newtype Stack = Stk [String] deriving Show
```

Nesta estrutura de dados existem as seguintes operações:
- newStack: cria uma nova stack vazia
- fromList: cria uma nova stack a partir de uma lista de strings
- push: insere uma nova string na stack
- pop: remove a string no topo da stack
- top: retorna a string no topo da stack
- isEmpty: verifica se a stack está vazia

### Lógica do programa	

A função `run` recebe os argumentos `(code, stack, state)`, enquanto o code não for uma lista vazia, a função `run` executa a instrução que se encontra no topo da lista code e chama recursivamente a função `run` com a lista de instruções restantes.

As instruções suportadas pela máquina são as seguintes:

- Push n: insere o valor n na stack
- Add: soma os dois valores no topo da stack e insere o resultado na stack
- Mult: multiplica os dois valores no topo da stack e insere o resultado no topo da stack
- Sub: subtrai os dois valores no topo da stack e insere o resultado no topo da stack
- Tru: insere o valor tt na stack
- Fals: insere o valor ff na stack
- Equ: verifica se os dois valores no topo da stack são iguais e insere o resultado na stack
- Le: verifica se o segundo valor no topo da stack é menor que o primeiro e insere o resultado na stack
- And: verifica se os dois valores no topo da stack são iguais a tt e insere o resultado na stack
- Neg: verifica se o valor no topo da stack é igual a tt e insere o resultado na stack
- Fetch var: insere o valor associado à variável var na stack
- Store var: remove o valor no topo da stack e insere o valor associado à variável var no estado
- Noop
- Branch c1 c2: se o valor no topo da stack for tt, executa a lista de instruções c1, caso contrário executa a lista de instruções c2
- Loop c1 c2: executa a lista de instruções c1, caso o valor no topo da stack seja tt, executa a lista de instruções c2 e chama recursivamente a função `run`

## Parte 2: Compilador de uma linguagem imperativa
Nesta parte do projeto, foi-nos pedido que implementássemos um compilador para uma linguagem imperativa. Para tal, foram necessárias três etapas.

<img src="docs/Lexer-Parser-Compiler.png" alt="drawing" width="500"/>

### Lexer 
Responsável por atribuir tokens a cada elemento da linguagem. 
No nosso caso, a função `lexer` recebe uma string e retorna uma lista de tokens.

**Tokens:**
A nossa linguagem suporta os seguintes tokens:

```haskell
data Token = TokAssign          -- ':='
           | TokSemicolon       -- ';'
           | TokVar String      -- var name
           | TokNumber Integer  -- number
           | TokOpenParen       -- '('
           | TokCloseParen      --')'
           | TokAdd             -- '+'
           | TokSub             -- '-'
           | TokMul             -- '*'
           | TokIf              -- 'if'
           | TokThen            -- 'then'
           | TokElse            -- 'else'
           | TokWhile           -- 'while'
           | TokDo              -- 'do'
           | TokBoolEqu         -- '='
           | TokIntEqu           -- '=='
           | TokLE              -- '<=' 
           | TokNot             -- 'not'
           | TokAnd             -- 'and'
           | TokTrue            -- 'True'
           | TokFalse           -- 'False'    
           deriving (Show, Eq)
```

O nosso lexer funciona da seguinte forma:
- **Espaços:** são ignorados
- **Letras:** a função `lexIdentifier` verifica se a string é uma palavra reservada ou uma variável(começada por uma letra minúscula) e retorna o token correspondente
- **Números:** a função `lexNumber` retorna o token TokNumber (número)
- **Operadores de um caracter, parênteses e ponto e vírgula:** a função `lexer` adiciona o token correspondente à lista de tokens 
- **Operadores com mais do que um caracter:** operadores como o `:=`, `==` e o `<=` são tratados pelas funções `lexAssign`, `lexEqual` e `lexLessEqual` respetivamente

Após cada um dos passos anteriores, a função `lexer` chama recursivamente a função `lexer` com a string restante até que a string seja vazia.

```haskell
--Exemplo do resultado da função lexer
x := 5; x := x - 1;
[AssignStm "x" (NumExp 5), AssignStm "x" (SubExp (VarExp "x") (NumExp 1))]
```

### Parser
Responsável por transformar a lista de tokens numa árvore sintática. É nesta etapa que tratamos a precedência dos operadores. Definimos três estruturas de dados distintas para representar expressões aritméticas, expressões booleanas e instruções.

```haskell
data Aexp = NumExp Integer      -- Número inteiro  
          | VarExp String        -- Variável
          | AddExp Aexp Aexp    -- Soma  
          | SubExp Aexp Aexp    -- Subtração       
          | MulExp Aexp Aexp    -- Multiplicação  
          deriving Show

data Bexp = TrueExp             --Verdadeiro     
          | FalseExp            --Falso
          | EqArExp Aexp Aexp   -- Igualdade entre duas expressões aritméticas
          | EqBoolExp Bexp Bexp -- Igualdade entre duas expressões booleanas   
          | LeExp Aexp Aexp     -- Menor ou igual entre duas expressões aritméticas    
          | NotExp Bexp         -- Negação de uma expressão booleana
          | AndExp Bexp Bexp    -- Conjunção entre duas expressões booleanas
          deriving Show

data Stm = AssignStm String Aexp -- Atribuição 
          | SeqStm [Stm]         -- Sequência de instruções
          | IfStm Bexp Stm Stm   
          | WhileStm Bexp Stm    
          deriving Show
```

A função `parser` aplica a função `lexer` ao código e chama a função `buildData` com a lista de tokens resultante. A função `buildData` recebe uma lista de tokens e retorna uma árvore sintática.

```haskell 
parser :: String -> Program
parser = buildData . lexer
```

A função `buildData` chama recursivamente a função `parseStm` até que a lista de tokens seja vazia e verifica se a lista de tokens foi processada na totalidade. Caso contrário, é lançado um erro.

```haskell
buildData :: [Token] -> Program
buildData tokens = 
  case parseStm tokens of
    Just (stm, []) -> [stm] 
    Just (stm, restTokens) -> stm : buildData restTokens
    _ -> error $ "Unexpected error parsing statement (buildData): " ++ show tokens
```

#### Parser de instruções
Por sua vez, a função `parseStm` é responsável por processar as instruções, que podem ser de quatro tipos distintos: atribuição, if-then-else, while e sequência de instruções. Dependendo do tipo de instrução a função `parseStm` chama recursivamente a função `parseAexp`, `parseBexp` ou `parseSeqStm` como podemos observar do excerto de código seguinte.

```haskell
data Stm = AssignStm String Aexp 
          | SeqStm [Stm]      
          | IfStm Bexp Stm Stm   
          | WhileStm Bexp Stm    
          deriving Show

parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm tokens = case tokens of
  TokVar var : TokAssign : restTokens ->
    case parseAexp restTokens of
      ...

  TokIf : restTokens1 ->
    case parseBexp restTokens1 of
      ...
          case parseStm restTokens3 of
            ...
              case parseStm restTokens4 of
                ...
  TokWhile : restTokens1 ->
    case parseBexp restTokens1 of
        ...
          case parseStm restTokens3 of
              ...

  TokOpenParen : restTokens1 ->
    case parseSeqStm restTokens1 of
      ...
```

#### Parser de expressões aritméticas
No parsing de funções aritméticas usamos um conjunto de funções auxiliares que nos permitem tratar a precedência dos operadores. Os operadores de maior precedência (Multiplicação e parênteses) têm que ser processados primeiro. 

A função `parseAexp` recebe uma lista de tokens, chama a função `parseSumOrDifOrProdOrIntOrPar` e verifica se a lista de tokens foi processada na totalidade. Caso contrário, é lançado um erro.

De forma a tratar a precedência dos operadores, a função `parseSumOrDifOrProdOrIntOrPar` chama a função `parseProdOrIntOrPar` que trata os operadores de maior precedência. Só após o processamento dos operadores de maior precedência é que é chamada a função `parseSumOrDiff` que trata os operadores de menor precedência.

#### Parser de expressões booleanas
A função `parseBexp` recebe uma lista de tokens, chama a função `parseAndOrMore` e verifica se a lista de tokens foi processada na totalidade. Caso contrário, é lançado um erro.

```haskell
parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens = case parseAndOrMore tokens of
  Just (bexp, []) -> Just (bexp, [])
  Just (bexp, TokThen:rest) -> Just (bexp, TokThen:rest)
  Just (bexp, TokElse:rest) -> Just (bexp, TokElse:rest)
  Just (bexp, TokDo:rest) -> Just (bexp, TokDo:rest)
  Just ( _, rest) -> error $ "Unparsed tokens (parseB): " ++ show rest
  _ -> error $ "Unexpected error parsing boolean expression: " ++ show tokens
```
A função `parseAndOrMore` verifica se a lista de tokens começa com um parênteses, neste caso chama a função `parseConstOrParen`. Caso contrário, chama a função `parseBoolEqOrMore` que trata os operadores de menor precedência.

```haskell
parseAndOrMore :: [Token] -> Maybe (Bexp, [Token])
parseAndOrMore (TokOpenParen:rest) = parseConstOrParen (TokOpenParen:rest)
parseAndOrMore tokens = case parseBoolEqOrMore tokens of
  Just (bexp, TokAnd : restTokens) -> case parseAndOrMore restTokens of
    Just (bexp2, restTokens2) -> Just (AndExp bexp bexp2, restTokens2)
  result -> result
```

### Compilador
O compilador será responsável pelo processamento da AST, gerando o código para a máquina de baixo nível implementada na primeira parte do projeto.

Foram implementadas duas funções auxiliares, `compA` e `compB`, que recebem uma expressão aritmética ou booleana, respetivamente, e retornam o código correspondente.

A função `compile` recebe uma AST e retorna o código correspondente. 
- No caso de uma atribuição, a função `compile` chama a função `compA` com a expressão aritmética e adiciona o token Store à lista de instruções.
- No caso de uma sequência de instruções, a função `compile` chama recursivamente a função `compile` com cada uma das instruções.
- No caso de um if, a função `compile` chama a função `compB` com a expressão booleana e adiciona o token Branch à lista de instruções.
- No caso de um while, a função `compile` chama a função `compB` com a expressão booleana e adiciona o token Loop à lista de instruções.

No caso da função `compileA` temos os seguintes casos:
- NumExp: adiciona o token Push à lista de instruções
- VarExp: adiciona o token Fetch à lista de instruções
- AddExp: chama recursivamente a função `compileA` com as duas expressões aritméticas e adiciona o token Add à lista de instruções
- SubExp: chama recursivamente a função `compileA` com as duas expressões aritméticas e adiciona o token Sub à lista de instruções
- MulExp: chama recursivamente a função `compileA` com as duas expressões aritméticas e adiciona o token Mult à lista de instruções

No caso da função `compileB` temos os seguintes casos:
- TrueExp: adiciona o token Tru à lista de instruções
- FalseExp: adiciona o token Fals à lista de instruções
- EqArExp: chama recursivamente a função `compileA` com as duas expressões aritméticas e adiciona o token Equ à lista de instruções
- EqBoolExp: chama recursivamente a função `compileB` com as duas expressões booleanas e adiciona o token Equ à lista de instruções
- LeExp: chama recursivamente a função `compileA` com as duas expressões aritméticas e adiciona o token Le à lista de instruções
- NotExp: chama recursivamente a função `compileB` com a expressão booleana e adiciona o token Neg à lista de instruções
- AndExp: chama recursivamente a função `compileB` com as duas expressões booleanas e adiciona o token And à lista de instruções

## Execução do código
Para proceder à execução do programa, é necessário ter instalado o [GHC](https://www.haskell.org/ghc/). Após a instalação, basta executar o seguinte comando na pasta src:

```haskell
ghci .\main.hs
```
