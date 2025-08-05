


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





ambientesimples = [("+",VFun (\x -> (VFun (\y -> somaValorFun x y)))),
                    ("*",VFun (\x -> (VFun (\y -> multValorFun x y))))]

somaValorFun (Numero x) (Numero y) = Numero (x+y)
somaValorFun _ _ = Excecao

multValorFun (Numero x) (Numero y) = Numero (x*y)
multValorFun _ _ = Excecao

-- Temos agora duas funções de interpretação, uma para termos e uma
-- para programas. A de termos simplesmente lê o ambiente. A de programa
-- propaga alterações no ambiente, para acumular as funções definidas.

intTermo a (Identifier i) = getValor i a
intTermo a (Literal l) = Numero l
intTermo a (Lambda i t) = Funcao (\v -> intTermo ((i,v):a) t)
intTermo a (Aplicacao t1 t2) = aplica v1 v2
                                where v1 = intTermo a t1
                                      v2 = intTermo a t2

intPrograma a [] = Excecao
intPrograma a [(Def i t)] = intTermo a t
intPrograma a ((Def i t):ds) = intPrograma ((i,v):a) ds
                               where v = intTermo a t

getValor i [] = Excecao
getValor i ((j,v):l) = if i == j then v else getValor i l

aplica (Funcao f) v = f v
aplica _ _ = Excecao
