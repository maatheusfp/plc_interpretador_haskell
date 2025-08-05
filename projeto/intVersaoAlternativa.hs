-- Interpretador orientado a objetos: estrutura base robusta
module InterpretacaoDeOrientacaoDeObjetos where

-- === Tipos base ===
type Id = String
type Numero = Double
type Booleano = Bool

-- === Representação dos termos da linguagem ===
data Termo = Identifier Id
           | LiteralNum Numero
           | LiteralBool Booleano
           | This
           | If Termo Termo Termo
           | New Id [Termo]
           | While Termo Termo
           | For Id Termo Termo Termo
           | Call Termo Id [Termo]
           | Class Id [Id] [(Id, Termo)] [(Id, [Id], Termo)]
           | Assign Termo Termo
           | Mul Termo Termo
           | Add Termo Termo
           | InstanceOf Termo Id
           | Fun Id [Id] Termo
           deriving (Eq, Show)

-- === Representação de valores avaliados ===
data Valor
  = VNum Numero
  | VBool Booleano
  | VAddr Endereco                            -- Referência a objeto no heap
  | VFun ([Valor] -> Estado -> Heap -> (Valor, Estado, Heap))
  | Erro

-- === Objeto instanciado ===
data Objeto = Objeto {
  classe :: Id,
  atributos :: [(Id, Valor)]
} deriving (Eq, Show)

-- === Definição de classe ===
data ClasseDef = Classe {
  nomeClasse :: Id,
  superClasses :: [Id],
  atributosClasse :: [(Id, Termo)],
  metodosClasse :: [(Id, [Id], Termo)]
} deriving (Eq, Show)

-- === Estruturas de Execução ===
type Endereco = Int

-- Estado: variáveis no escopo de execução
type Estado = [(Id, Valor)]

-- Heap: memória dinâmica de objetos
type Heap = [(Endereco, Objeto)]

-- Ambiente de classes: todas as classes disponíveis
type AmbienteClasse = [(Id, ClasseDef)]

-- === Funções auxiliares para Estado ===
busca :: Id -> Estado -> Valor
busca x [] = Erro
busca x ((y,v):r) = if x == y then v else busca x r

escreve :: (Id, Valor) -> Estado -> Estado
escreve (x,v) [] = [(x,v)]
escreve (x,v) ((y,u):r)
  | x == y    = (x,v):r
  | otherwise = (y,u):escreve (x,v) r

-- === Funções auxiliares para Heap ===
buscaObjeto :: Endereco -> Heap -> Maybe Objeto
buscaObjeto _ [] = Nothing
buscaObjeto n ((e,obj):r)
  | n == e    = Just obj
  | otherwise = buscaObjeto n r

alocaObjeto :: Objeto -> Heap -> (Endereco, Heap)
alocaObjeto obj heap =
  let novoEnd = if null heap then 0 else fst (last heap) + 1
  in (novoEnd, heap ++ [(novoEnd, obj)])

-- === Funções auxiliares para AmbienteClasse ===
buscaClasse :: Id -> AmbienteClasse -> Maybe ClasseDef
buscaClasse _ [] = Nothing
buscaClasse nome ((idClasse, def):resto)
  | nome == idClasse = Just def
  | otherwise        = buscaClasse nome resto

-- === Impressão de valores ===
instance Show Valor where
  show (VNum n)     = show n
  show (VBool b)    = show b
  show (VAddr ref)  = "<ref:" ++ show ref ++ ">"
  show (VFun _)     = "<função>"
  show Erro         = "Erro"
