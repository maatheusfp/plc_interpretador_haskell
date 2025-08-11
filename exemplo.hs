module InterpretacaoEAnaliseEstaticaDelinguagens where


type Id = String
type Numero = Double
data TermoLinFun = Identifier Id
                 | Literal Numero
                 | Lambda Id TermoLinFun
                 | Aplicacao TermoLinFun TermoLinFun
data Definicao = Def Id TermoLinFun
type Programa = [Definicao]




def1 = Def "inc" (Lambda "x" (Aplicacao (Aplicacao (Identifier "+") (Identifier "x")) (Literal 1)))
def2 = Def "v" (Aplicacao (Aplicacao (Identifier "+") (Literal 3)) (Literal 2))
def3 = Def "resultado" (Aplicacao (Identifier "inc") (Identifier "v"))
prog1 = [def1,def2,def3]

defX = Def "x" (Literal 10)
defY = Def "y" (Literal 20)
defSoma = Def "soma" (Aplicacao (Aplicacao (Identifier "+") (Identifier "x")) (Identifier "y"))

defZ = Def "z" (Literal 13)
prog2 = [defX, defY,  defSoma, defZ]


data ValorFun = Numero Double
              | Funcao (ValorFun -> ValorFun)
              | Excecao

instance Show ValorFun where
    show (Numero n) = show n
    show (Funcao f) = "Function definition cannot be printed!"
    show Excecao = "Excecao durante a execucao do interpretador"



type Ambiente = [(Id,ValorFun)]



ambientesimples = [("+",Funcao (\x -> (Funcao (\y -> somaValorFun x y))))]

somaValorFun (Numero x) (Numero y) = Numero (x+y)
somaValorFun _ _ = Excecao




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


-- data Termo = Var Id
--            | Lit Numero
--            | Som Termo Termo
--            | Lam Id Termo
--            | Apl Termo Termo
--            | Atr Id Termo
--            | Seq Termo Termo


-- termo1 = (Apl (Lam "x" (Som (Var "x") (Lit 2))) (Lit 3))

-- termo2 = (Apl (Lam "x" (Som (Var "x") (Var "y"))) (Lit 3))

-- termo3 = (Seq (Atr "y" termo2) termo2)

-- sq1 = (Seq (Atr "y" (Lit 3)) termo2)


-- sq2 = (Seq (Atr "y" (Lit 3)) termo3)

-- sq3 = (Seq (Atr "y" (Som (Atr "z" (Lit 5)) (Var "z"))) termo3)


-- data Valor = Num Double
--            | Fun (Valor -> Estado -> (Valor,Estado))
--            | Erro

-- type Estado = [(Id,Valor)]




-- int a (Var x) e = (search x (a ++ e), e)

-- int a (Lit n) e = (Num n, e)

-- int a (Som t u) e = (somaVal v1 v2, e2)
--                     where (v1,e1) = int a t e
--                           (v2,e2) = int a u e1

-- int a (Lam x t) e = (Fun (\v -> int ((x,v):a) t), e)

-- int a (Apl t u) e = app v1 v2 e2
--                     where (v1,e1) = int a t e
--                           (v2,e2) = int a u e1

-- int a (Atr x t) e = (v1, wr (x,v1) e1)
--                     where (v1,e1) = int a t e

-- int a (Seq t u) e = int a u e1
--                     where (_,e1) = int a t e


-- search i [] = Erro
-- search i ((j,v):l) = if i == j then v else search i l


-- somaVal (Num x) (Num y) = Num (x+y)
-- somaVal _ _ = Erro



-- app (Fun f) v e = f v e
-- app _ _ e = (Erro, e)



-- wr (i,v) [] = [(i,v)]
-- wr (i,v) ((j,u):l) = if (i == j) then (j,v):l else [(j,u)] ++ (wr (i,v) l)


-- at t = int [] t []


-- instance Show Valor where
--    show (Num x) = show x
--    show Erro = "Erro"
--    show (Fun f) = "Função"
