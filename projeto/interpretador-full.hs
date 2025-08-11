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
          | For Id Termo Termo Termo Termo -- Inicialização, condição, atualização, corpo
          | Call Termo Id [Termo]  -- objeto, método, argumentos
          | Class Id [Id] [(Id,Termo)] [(Id, [Id], Termo)] -- Nome, heranças, atributos, métodos
          | Assign Termo Termo -- Atribuição de variável
          | Mul Termo Termo -- Multiplicação
          | Add Termo Termo -- adição
          | InstanceOf Termo Id -- Verifica se um objeto é uma instância de uma classe
          | Fun Id [Id] Termo -- Nome, parâmetros, corpo
          | LessThan Termo Termo
          | GreaterThan Termo Termo
          | Equal Termo Termo
          | Seq Termo Termo  -- Sequência de termos
          deriving (Eq, Show)

data Valor
  = VNum Numero
  | VBool Booleano
  | VAddr Endereco
  | VFun ([Valor] -> Estado -> Heap -> AmbienteClasse -> (Valor, Estado, Heap, AmbienteClasse))
  | Erro String

instance Show Valor where
  show (VNum n)    = show n
  show (VBool b)   = show b
  show (VAddr a)   = "<ref:" ++ show a ++ ">"
  show (VFun _)    = "<função>"
  show (Erro msg)  = "Erro: " ++ msg

type Endereco = Int
type Estado = [(Id, Valor)]
type Heap = [(Endereco, Objeto)]
type AmbienteClasse = [(Id, ClasseDef)]

data Objeto = Objeto {
  classeObj :: Id,
  atributosObj :: [(Id, Valor)]
} deriving ( Show)

data ClasseDef = Classe {
  nomeClasse :: Id,
  superClasses :: [Id],
  atributosClasse :: [(Id, Termo)],
  metodosClasse :: [(Id, [Id], Termo)]
} deriving (Eq, Show)

-- Busca valor no Estado
busca :: Id -> Estado -> Valor
busca x [] = Erro ("Variável não encontrada: " ++ x)
busca x ((y,v):r) = if x == y then v else busca x r

-- Atualiza Estado
escreve :: (Id, Valor) -> Estado -> Estado
escreve (x,v) [] = [(x,v)]
escreve (x,v) ((y,u):r)
  | x == y    = (x,v):r
  | otherwise = (y,u):escreve (x,v) r

-- Busca objeto no heap
buscaObjeto :: Endereco -> Heap -> Maybe Objeto
buscaObjeto _ [] = Nothing
buscaObjeto n ((e,obj):r)
  | n == e    = Just obj
  | otherwise = buscaObjeto n r

-- Aloca objeto no heap
alocaObjeto :: Objeto -> Heap -> (Endereco, Heap)
alocaObjeto obj heap =
  let novoEnd = if null heap then 0 else fst (last heap) + 1
  in (novoEnd, heap ++ [(novoEnd, obj)])

-- Busca classe no ambiente de classes
buscaClasse :: Id -> AmbienteClasse -> Maybe ClasseDef
buscaClasse _ [] = Nothing
buscaClasse nome ((idClasse, def):resto)
  | nome == idClasse = Just def
  | otherwise        = buscaClasse nome resto

-- Verifica se uma classe herda outra (direta ou indiretamente)
herdaDe :: Id -> Id -> AmbienteClasse -> Bool
herdaDe sub sup ac
  | sub == sup = True
  | otherwise = case buscaClasse sub ac of
      Nothing -> False
      Just (Classe _ supers _ _) -> any (\c -> herdaDe c sup ac) supers

-- Interpretador de programas
intPrograma :: Estado -> Heap -> AmbienteClasse -> [Termo] -> (Valor, Estado, Heap, AmbienteClasse)
intPrograma est heap ac [] = (VNum 0, est, heap, ac)
intPrograma est heap ac [t] = intTermo est heap ac t
intPrograma est heap ac (t:ts) = case t of
  Class nome sup atrs metodos ->
    let novaClasse = Classe nome sup atrs metodos
        ac' = (nome, novaClasse) : ac
    in intPrograma est heap ac' ts

  Fun nome args corpo ->
    let f = VFun (\vals est' heap' ac' ->
                    let est'' = zip args vals ++ est'
                        (v, est3, heap3, ac'') = intTermo est'' heap' ac' corpo
                    in (v, est3, heap3, ac'')
                )
        est' = escreve (nome, f) est
    in intPrograma est' heap ac ts

  _ ->
    let (v, est', heap', ac') = intTermo est heap ac t
    in intPrograma est' heap' ac' ts

-- Interpretador de termos
intTermo :: Estado -> Heap -> AmbienteClasse -> Termo -> (Valor, Estado, Heap, AmbienteClasse)
intTermo est heap ac termo = case termo of

  Identifier id -> 
    case busca id est of
      Erro msg -> case busca "this" est of
        VAddr addr -> case buscaObjeto addr heap of
          Just obj -> case lookup id (atributosObj obj) of
            Just val -> (val, est, heap, ac)
            Nothing -> (Erro ("Atributo não encontrado: " ++ id), est, heap, ac)
          Nothing -> (Erro ("Objeto não encontrado no heap no endereço: " ++ show addr), est, heap, ac)
        _ -> (Erro ("Identificador não encontrado: " ++ id), est, heap, ac)
      val -> (val, est, heap, ac)

  LiteralNum n -> (VNum n, est, heap, ac)
  LiteralBool b -> (VBool b, est, heap, ac)

  This -> (busca "this" est, est, heap, ac)

  If cond tThen tElse ->
    let (vCond, est1, heap1, ac1) = intTermo est heap ac cond
    in case vCond of
      VBool True -> intTermo est1 heap1 ac1 tThen
      VBool False -> intTermo est1 heap1 ac1 tElse
      Erro msg -> (Erro ("Erro na condição do if: " ++ msg), est1, heap1, ac1)
      _ -> (Erro "Condição do if não é booleano", est1, heap1, ac1)

  New nome args -> 
    case buscaClasse nome ac of
      Nothing -> (Erro ("Classe não encontrada: " ++ nome), est, heap, ac)
      Just classeDef ->
        let (valsArgs, est1, heap1, ac1) = avaliaLista est heap ac args
            (atribVals, est2, heap2, ac2) = inicializaAtributos est1 heap1 ac1 (atributosClasse classeDef)
            obj = Objeto nome atribVals
            (end, heap3) = alocaObjeto obj heap2
            est3 = escreve ("this", VAddr end) est2
        in (VAddr end, est3, heap3, ac2)

  Call objTerm nomeMetodo args ->
    let (vObj, est1, heap1, ac1) = intTermo est heap ac objTerm
    in case vObj of
      VAddr addr -> case buscaObjeto addr heap1 of
        Nothing -> (Erro ("Objeto não encontrado no endereço: " ++ show addr), est1, heap1, ac1)
        Just obj -> case buscaMetodoClasse nomeMetodo (classeObj obj) ac1 of
          Nothing -> (Erro ("Método não encontrado: " ++ nomeMetodo), est1, heap1, ac1)
          Just (paramNames, corpo) ->
            let (valsArgs, est2, heap2, ac2) = avaliaLista est1 heap1 ac1 args
                est3 = escreve ("this", VAddr addr) est2
                est4 = zip paramNames valsArgs ++ est3
                (vRet, est5, heap5, ac3) = intTermo est4 heap2 ac2 corpo
            in (vRet, est5, heap5, ac3)
      Erro msg -> (Erro ("Erro no objeto da chamada: " ++ msg), est1, heap1, ac1)
      _ -> (Erro "Tentativa de chamar método em um valor que não é objeto", est1, heap1, ac1)

  While cond corpo ->
    let loop est' heap' ac' =
          let (vCond, est1, heap1, ac1) = intTermo est' heap' ac' cond
          in case vCond of
            VBool True -> 
              let (_, est2, heap2, ac2) = intTermo est1 heap1 ac1 corpo
              in loop est2 heap2 ac2
            VBool False -> (VNum 0, est1, heap1, ac1)
            Erro msg -> (Erro ("Erro na condição do while: " ++ msg), est1, heap1, ac1)
            _ -> (Erro "Condição do while não é booleano", est1, heap1, ac1)
    in loop est heap ac

  For id init cond incr corpo ->
    let (vInit, est1, heap1, ac1) = intTermo est heap ac init
        est2 = escreve (id, vInit) est1
        loop est' heap' ac' =
          let (vCond, estc, heapc, acc) = intTermo est' heap' ac' cond
          in case vCond of
            VBool True ->
              let (_, estb, heapo, acb) = intTermo estc heapc acc corpo
                  (_, esti, heapi, aci) = intTermo estb heapo acb incr
              in loop esti heapi aci
            VBool False -> (VNum 0, estc, heapc, acc)
            Erro msg -> (Erro ("Erro na condição do for: " ++ msg), estc, heapc, acc)
            _ -> (Erro "Condição do for não é booleano", estc, heapc, acc)
    in loop est2 heap1 ac1

  InstanceOf objTerm nomeClasse ->
    let (vObj, est1, heap1, ac1) = intTermo est heap ac objTerm
    in case vObj of
      VAddr addr -> case buscaObjeto addr heap1 of
        Nothing -> (Erro ("Objeto não encontrado no endereço: " ++ show addr), est1, heap1, ac1)
        Just obj -> 
          if herdaDe (classeObj obj) nomeClasse ac1
          then (VBool True, est1, heap1, ac1)
          else (VBool False, est1, heap1, ac1)
      Erro msg -> (Erro ("Erro na verificação instanceof: " ++ msg), est1, heap1, ac1)
      _ -> (Erro "Operando de instanceof não é um objeto", est1, heap1, ac1)

  Fun nome args corpo ->
    let f = VFun (\vals est' heap' ac' ->
                    let est'' = zip args vals ++ est'
                        (v, est3, heap3, ac'') = intTermo est'' heap' ac' corpo
                    in (v, est3, heap3, ac'')
                )
        est' = escreve (nome, f) est
    in (f, est', heap, ac)

  Assign (Identifier id) termo2 ->
    let (v, est1, heap1, ac1) = intTermo est heap ac termo2
        est2 = escreve (id, v) est1
    in (v, est2, heap1, ac1)

  Add t1 t2 ->
    let (v1, est1, heap1, ac1) = intTermo est heap ac t1
        (v2, est2, heap2, ac2) = intTermo est1 heap1 ac1 t2
    in case (v1, v2) of
      (VNum n1, VNum n2) -> (VNum (n1 + n2), est2, heap2, ac2)
      (Erro msg, _)      -> (Erro ("Erro na soma, primeiro operando: " ++ msg), est2, heap2, ac2)
      (_, Erro msg)      -> (Erro ("Erro na soma, segundo operando: " ++ msg), est2, heap2, ac2)
      _                  -> (Erro "Tipos inválidos para soma", est2, heap2, ac2)

  Mul t1 t2 ->
    let (v1, est1, heap1, ac1) = intTermo est heap ac t1
        (v2, est2, heap2, ac2) = intTermo est1 heap1 ac1 t2
    in case (v1, v2) of
      (VNum n1, VNum n2) -> (VNum (n1 * n2), est2, heap2, ac2)
      (Erro msg, _)      -> (Erro ("Erro na multiplicação, primeiro operando: " ++ msg), est2, heap2, ac2)
      (_, Erro msg)      -> (Erro ("Erro na multiplicação, segundo operando: " ++ msg), est2, heap2, ac2)
      _                  -> (Erro "Tipos inválidos para multiplicação", est2, heap2, ac2)

  LessThan t1 t2 ->
    let (v1, est1, heap1, ac1) = intTermo est heap ac t1
        (v2, est2, heap2, ac2) = intTermo est1 heap1 ac1 t2
    in case (v1, v2) of
      (VNum n1, VNum n2) -> (VBool (n1 < n2), est2, heap2, ac2)
      (Erro msg, _) -> (Erro ("Erro na comparação menor que, primeiro operando: " ++ msg), est2, heap2, ac2)
      (_, Erro msg) -> (Erro ("Erro na comparação menor que, segundo operando: " ++ msg), est2, heap2, ac2)
      _ -> (Erro "Tipos inválidos para comparação menor que", est2, heap2, ac2)

  GreaterThan t1 t2 ->
    let (v1, est1, heap1, ac1) = intTermo est heap ac t1
        (v2, est2, heap2, ac2) = intTermo est1 heap1 ac1 t2
    in case (v1, v2) of
      (VNum n1, VNum n2) -> (VBool (n1 > n2), est2, heap2, ac2)
      (Erro msg, _) -> (Erro ("Erro na comparação maior que, primeiro operando: " ++ msg), est2, heap2, ac2)
      (_, Erro msg) -> (Erro ("Erro na comparação maior que, segundo operando: " ++ msg), est2, heap2, ac2)
      _ -> (Erro "Tipos inválidos para comparação maior que", est2, heap2, ac2)

  Equal t1 t2 ->
    let (v1, est1, heap1, ac1) = intTermo est heap ac t1
        (v2, est2, heap2, ac2) = intTermo est1 heap1 ac1 t2
    in case (v1, v2) of
      (VNum n1, VNum n2) -> (VBool (n1 == n2), est2, heap2, ac2)
      (Erro msg, _) -> (Erro ("Erro na comparação igual que, primeiro operando: " ++ msg), est2, heap2, ac2)
      (_, Erro msg) -> (Erro ("Erro na comparação igual que, segundo operando: " ++ msg), est2, heap2, ac2)
      _ -> (Erro "Tipos inválidos para comparação igual que", est2, heap2, ac2)  

  Seq t1 t2 ->
    let (_, est1, heap1, ac1) = intTermo est heap ac t1
        (v2, est2, heap2, ac2) = intTermo est1 heap1 ac1 t2
    in (v2, est2, heap2, ac2)

  _ -> (Erro "Expressão inválida", est, heap, ac)



-- Avalia lista de termos em sequência
avaliaLista :: Estado -> Heap -> AmbienteClasse -> [Termo] -> ([Valor], Estado, Heap, AmbienteClasse)
avaliaLista est heap ac [] = ([], est, heap, ac)
avaliaLista est heap ac (t:ts) =
  let (v, est1, heap1, ac1) = intTermo est heap ac t
      (vs, est2, heap2, ac2) = avaliaLista est1 heap1 ac1 ts
  in (v:vs, est2, heap2, ac2)

-- Inicializa os atributos de um objeto
inicializaAtributos :: Estado -> Heap -> AmbienteClasse -> [(Id, Termo)] -> ([(Id, Valor)], Estado, Heap, AmbienteClasse)
inicializaAtributos est heap ac [] = ([], est, heap, ac)
inicializaAtributos est heap ac ((id, termo):resto) =
  let (v, est1, heap1, ac1) = intTermo est heap ac termo
      (vs, est2, heap2, ac2) = inicializaAtributos est1 heap1 ac1 resto
  in ((id, v) : vs, est2, heap2, ac2)

-- Busca método em classe, considerando herança
buscaMetodoClasse :: Id -> Id -> AmbienteClasse -> Maybe ([Id], Termo)
buscaMetodoClasse nomeMetodo nomeClasse ac =
  case buscaClasse nomeClasse ac of
    Nothing -> Nothing
    Just (Classe _ supers _ metodos) ->
      case lookup nomeMetodo (map (\(n, args, corpo) -> (n, (args, corpo))) metodos) of
        Just res -> Just res
        Nothing -> buscaEmSuper supers
      where
        buscaEmSuper [] = Nothing
        buscaEmSuper (c:s) = case buscaMetodoClasse nomeMetodo c ac of
                              Just res -> Just res
                              Nothing -> buscaEmSuper s



-- Programa 1: Criar objeto Ponto e chamar getX
programaTeste1 :: [Termo]
programaTeste1 =
  [ Class "Ponto" [] 
      [("x", LiteralNum 10), ("y", LiteralNum 20)]  
      [("getX", [], Identifier "x")]
  , New "Ponto" []
  , Call (New "Ponto" []) "getX" []
  ]

-- Programa 2: Atribuir e somar
programaTeste2 :: [Termo]
programaTeste2 =
  [ Assign (Identifier "a") (LiteralNum 5)
  , Assign (Identifier "b") (LiteralNum 10)
  , Add (Identifier "a") (Identifier "b")
  ]

-- Programa 3: If simples
programaTeste3 :: [Termo]
programaTeste3 =
  [ If (LiteralBool True) (LiteralNum 1) (LiteralNum 0)
  , If (LiteralBool False) (LiteralNum 1) (LiteralNum 0)
  ]

-- Programa 4: While que soma números até 5
programaTeste4 :: [Termo]
programaTeste4 =
  [ Assign (Identifier "i") (LiteralNum 0)
  , Assign (Identifier "soma") (LiteralNum 4)
  , While
      (LessThan (Identifier "i") (LiteralNum 5))
      (Seq
        (Assign (Identifier "soma") (Mul (Identifier "soma") (LiteralNum 10)))
        (Assign (Identifier "i") (Add (Identifier "i") (LiteralNum 1)))
      )
  ]


-- Programa 5: For loop que incrementa variável
programaTeste5 :: [Termo]
programaTeste5 =
  [ 
    Assign (Identifier "soma") (LiteralNum 4),
    For "i" 
      (LiteralNum 0)                         -- inicialização i = 0
      (LessThan (Identifier "i") (LiteralNum 3))                     -- condição sempre true para exemplo (seria melhor usar i < 3)
      (Assign (Identifier "i") (Add (Identifier "i") (LiteralNum 1))) -- atualização: i = i + 1
      (Assign (Identifier "soma") (Add (Identifier "soma") (LiteralNum 1))) -- corpo: soma = soma + 1
  ]

-- Programa 6: Função que soma dois números e usa ela
programaTeste6 :: [Termo]
programaTeste6 =
  [ Fun "soma" ["x", "y"] (Add (Identifier "x") (Identifier "y"))
  , Call (Identifier "soma") [] [LiteralNum 3, LiteralNum 4]
  ]

-- Programa 7: Usando instanceof
programaTeste7 :: [Termo]
programaTeste7 =
  [ Class "Animal" [] [] []
  , Class "Cachorro" ["Animal"] [] []
  , New "Cachorro" []
  , InstanceOf (New "Cachorro" []) "Gato" -- Deve retornar False (classe não existe)
  , InstanceOf (New "Cachorro" []) "Animal" -- Deve retornar True
  , InstanceOf (New "Cachorro" []) "Cachorro" -- Deve retornar True

  ]






-- Observação: para atribuir a atributo (ex: p.x = 100) talvez precise adaptar o interpretador para isso.


-- Programa exemplo usando assign para testar
programaAssign :: [Termo]
programaAssign = [
    Assign (Identifier "a") (LiteralNum 42), -- a = 42
    Identifier "a"                            -- retorna o valor de "a"
  ]

-- Estado, heap e ambiente de classes vazios inicialmente
estadoInicial :: Estado
estadoInicial = []

heapInicial :: Heap
heapInicial = []

ambienteClasseInicial :: AmbienteClasse
ambienteClasseInicial = []

-- Rodando o interpretador
main :: IO ()
main = do
  let (valor, est, hp, ac) = intPrograma estadoInicial heapInicial ambienteClasseInicial programaTeste5
  putStrLn $ "Valor final: " ++ show valor
  putStrLn $ "Estado final: " ++ show est
  putStrLn $ "Heap final: " ++ show hp
  putStrLn $ "Ambiente de classes final: " ++ show ac
