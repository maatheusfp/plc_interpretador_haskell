


module InterpretacaoDeOrientacaoDeObjetos where


type Id = String
type Numero = Double
type Booleano = Bool
data Termo = Identifier Id
                | LiteralNum Numero
                | LiteralBool Booleano
                | This 
                | If Termo Termo Termo -- Condição, then, else
                | New Id [Termo] -- Nome da classe, argumentos
                | While Termo Termo -- Condição, corpo
                | For Id Termo Termo Termo -- Inicialização, condição, atualização, corpo
                | Call Termo Id [Termo]  -- objeto, método, argumentos
                | Class Id [Id] [(Id,Termo)] [(Id, [Id], Termo)] -- Nome, heranças, atributos, métodos
                | Assign Termo Termo -- Atribuição de variável
                | Mul Termo Termo -- Multiplicação
                | Add Termo Termo -- adição
                | InstanceOf Termo Id -- Verifica se um objeto é uma instância de uma classe
                | Fun Id [Id] Termo -- Nome, parâmetros, corpo



-- Valores possíveis no interpretador 
data Valor = VNum Numero                     -- Número
           | VBool Booleano                  -- Booleano
           | Endereco Identifier             -- Endereço
           | VFun ([Valor] -> Estado -> (Valor, Estado)) -- Função com estado
           | Erro                            -- Erro de execução



-- Definição de uma classe no ambiente de classes
data ClasseDef = Classe {
  nomeClasse :: Id,                          -- Nome da classe
  superClasses :: [Id],                      -- Lista de superclasses (herança)
  atributosClasse :: [(Id, Termo)],          -- Atributos com valores iniciais
  metodosClasse :: [(Id, [Id], Termo)]       -- Métodos da classe
}

-- Representação de um objeto 
data Objeto = Objeto {
  classe :: Id,                              -- Nome da classe do objeto
  atributos :: [(Id, Valor)],                -- Atributos da instância
  metodos :: [(Id, ([Id], Termo))]           -- Métodos: nome, parâmetros, corpo
}


type Estado = [(Id, Valor)]                  -- Memória (variável, valor)
type AmbienteClasses = [(Id, ClasseDef)]     -- Todas as classes disponíveis

-- Como imprimir os valores
instance Show Valor where
  show (VNum n) = show n
  show (VBool b) = show b
  show (Endereco i) = "<Endereço:" ++ i ++ ">"
  show (VFun _) = "<função>"
  show Erro     = "Erro"




-- === Funções auxiliares para o Estado ===
busca :: Id -> Estado -> Valor
busca x [] = Erro
busca x ((y,v):r) = if x == y then v else busca x r

escreve :: (Id, Valor) -> Estado -> Estado
escreve (x,v) [] = [(x,v)]
escreve (x,v) ((y,u):r)
  | x == y    = (x,v):r
  | otherwise = (y,u):escreve (x,v) r

-- === Funções auxiliares previstas para o AmbienteClasse ===
buscaClasse :: Id -> AmbienteClasse -> Maybe ClasseDef
buscaClasse _ [] = Nothing
buscaClasse nome ((idClasse, def):resto)
  | nome == idClasse = Just def
  | otherwise        = buscaClasse nome resto


ambientesimples = [("+",VFun (\x -> (VFun (\y -> somaValorFun x y)))),
                    ("*",VFun (\x -> (VFun (\y -> multValorFun x y))))]

somaValorFun (Numero x) (Numero y) = Numero (x+y)
somaValorFun _ _ = Excecao

multValorFun (Numero x) (Numero y) = Numero (x*y)
multValorFun _ _ = Excecao


aplica _ _ = Excecao



-- Interpretar classe adicionando no ambiente
intTermo a h ac (Class nome sup atrs metodos) = 
  let def = Classe nome sup atrs metodos
  in (VNum 0, a, h, (nome, def) : ac)  -- retorna novo ambiente de classes

-- New
intTermo a h ac (New nome args) = 
  -- Avalia args, pega a classe do ambiente, inicializa atributos, aloca objeto no heap
  ...

-- Call
intTermo a h ac (Call obj termoMetodo args) = 
  -- Avalia obj, avalia args, busca método, cria ambiente com this e args, executa corpo
  ...

-- While
intTermo a h ac (While cond corpo) = 
  -- Enquanto intTermo a h ac cond == VBool True, executa corpo
  ...

-- If
intTermo a h ac (If cond t1 t2) = 
  -- Se cond é True executa t1, senão t2
  ...

-- InstanceOf
intTermo a h ac (InstanceOf obj nomeClasse) =
  -- Avalia obj, verifica se classe do objeto é ou herda nomeClasse
  ...

-- This
-- No ambiente de execução 'a', 'this' está definido. Ao interpretar 'This', retorna o valor associado.

-- For
intTermo a h ac (For id init cond incr corpo) = 
  -- Executa init (escreve em estado), enquanto cond True executa corpo e incr
  ...

-- Fun
intTermo a h ac (Fun nome args corpo) = 
  -- Cria VFun que recebe argumentos, atualiza estado, executa corpo
  ...