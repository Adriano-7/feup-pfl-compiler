# 2º Projeto PFL

Grupo T08_G02:
- Adriano Alexandre dos Santos Machado (up202105352) - [TBD]
- Tomás Alexandre Soeiro Vicente (up202108717) - [TBD]

## Descrição do trabalho
Este projeto encontra-se dividido em duas partes. Num primeiro momento, foi-nos pedido que implementássemos uma máquina de baixo nível que suportasse instruções aritméticas, booleanas e de controlo de fluxo. Posteriormente, foi-nos solicitado que implementássemos um compilador para uma linguagem imperativa que compilasse para a máquina de baixo nível implementada na primeira parte.

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

O estado é representado por uma bst, onde cada nó contém uma variável, um valor associado e duas sub-árvores. 

```haskell	
data State = Empty 
            | Node String String State State
```
Nesta estrutura de dados existem as seguintes operações:
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
A função `run` recebe os argumentos (code, stack, state), 


## Parte 2: Compilador de uma linguagem imperativa

## Execução do código
Para proceder à execução do programa, é necessário ter instalado o [GHC](https://www.haskell.org/ghc/). Após a instalação, basta executar o seguinte comando na pasta src:

```haskell
ghci .\main.hs
```
