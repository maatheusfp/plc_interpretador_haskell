


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
                


data Valor = VNum Numero
        | VBool Booleano
        | VObj Objeto
        | VFun ([Valor] -> Estado -> (Valor, Estado))
        | Erro     

               
type Estado = [(Id, Valor)] 




