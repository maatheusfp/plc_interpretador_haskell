


module InterpretacaoDeOrientacaoDeObjetos where


type Id = String
type Numero = Double
type Booleano = Bool
data Termo = Identifier Id
                | LiteralNum Numero
                | LiteralBool Booleano
                | This -- Referência ao objeto atual
                | If Termo Termo Termo -- Condição, then, else
                | New Id [Termo] -- Nome da classe, argumentos
                | While Termo Termo -- Condição, corpo
                | For Termo Termo Termo Termo -- Inicialização, condição, atualização, corpo
                | Call Termo Id [Termo]  -- objeto, método, argumentos
                | Class Id [Id] [(Id,Termo)] [(Id, [Id], Termo)] -- Nome, superclasse, atributos, métodos
                | Attr Id Termo -- Atribuição de atributo
                | Assign Id Termo -- Atribuição de variável
                | Mul Termo Termo -- Multiplicação
                | InstanceOf Termo Id -- Verifica se um objeto é uma instância de uma classe
                | Fun Id [Id] Termo -- Nome, parâmetros, corpo
                | Return Termo -- Retorna um valor


             

               

                




