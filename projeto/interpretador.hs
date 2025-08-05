


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


data Objeto = Objeto {
  classe :: Id,
  atributos :: [(Id, Valor)],
  metodos :: [(Id, ([Id], Termo))] -- nome do método, parâmetros, corpo
}                


data Valor = VNum Numero
        | VBool Booleano
        | VObj Objeto
        | VFun ([Valor] -> Estado -> (Valor, Estado))
        | Erro     

data ClasseDef = Classe {
  nomeClasse :: Id,
  superClasses :: [Id],
  atributosClasse :: [(Id, Termo)],
  metodosClasse :: [(Id, [Id], Termo)]
}

type Estado = [(Id, Valor)] 

type AmbienteClasses = [(Id, ClasseDef)]


instance Show Valor where
  show (VNum n) = show n
  show (VBool b) = show b
  show (VObj (Objeto c _)) = "<objeto:" ++ c ++ ">"
  show (VFun _) = "<função>"
  show Erro     = "Erro"




