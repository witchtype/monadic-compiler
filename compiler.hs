-- Imperative language:

data Prog = Assign Name Expr
          | If Expr Prog Prog
          | While Expr Prog
          | Seq [Prog]
            deriving Show
data Expr = Val Int | Var Name | App Op Expr Expr
            deriving Show
type Name = Char
data Op   = Add | Sub | Mul | Div
            deriving Show

-- Factorial example:

fac :: Int -> Prog
fac n = Seq [Assign 'A' (Val 1),
             Assign 'B' (Val n),
             While (Var 'B') (Seq
                [Assign 'A' (App Mul (Var 'A') (Var 'B')),
                 Assign 'B' (App Sub (Var 'B') (Val 1))])]

-- Virtual machine:

type Stack = [Int]
type Mem   = [(Name,Int)]
type Code  = [Inst]
data Inst  = PUSH Int
           | PUSHV Name
           | POP Name
           | DO Op
           | JUMP Label
           | JUMPZ Label
           | LABEL Label
             deriving Show

type Label = Int

-- State monad:

type State = Label
newtype ST a = S (State -> (a, State))
app :: ST a -> State -> (a,State)
app (S st) x  =  st x

instance Functor ST where
   -- fmap :: (a -> b) -> ST a -> ST b
   fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
   -- pure :: a -> ST a
   pure x = S (\s -> (x,s))
   -- (<*>) :: ST (a -> b) -> ST a -> ST b
   stf <*> stx = S (\s ->
      let (f,s')  = app stf s
          (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
   -- return :: a -> ST a
   return = pure
   -- (>>=) :: ST a -> (a -> ST b) -> ST b
   st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')
------------------------------------------------------------------

-- function that takes in expression and compiles it into code
compexpr :: Expr -> Code
compexpr (Val n) = [PUSH n]
compexpr (Var x) = [PUSHV x]
compexpr (App o x y) = compexpr x ++ compexpr y ++ [DO o]

fresh :: ST Label
fresh = S (\n -> (n, n+1))

compprog :: Prog -> ST Code
compprog (Assign x e) = return (compexpr e ++ [POP x])

compprog (If e p1 p2) = do l1 <- fresh
                           l2 <- fresh
                           c1 <- compprog p1
                           c2 <- compprog p2
                           return (compexpr e ++ [JUMPZ l1] ++ c1 ++ [JUMP l2, LABEL l1] ++ c2 ++ [LABEL l2])

compprog (While e p) = do l1 <- fresh
                          l2 <- fresh
                          c <- compprog p
                          return ([LABEL l1] ++ compexpr e ++ [JUMPZ l2] ++ c ++ [JUMP l1, LABEL l2])


compprog (Seq ps) = do codes <- mapM compprog ps
                       return (concat codes)

-- top level function
comp :: Prog -> Code
comp p = fst(app(compprog p)0)

-- helper function to lookup variable in memory
lookupvar :: Name -> Mem -> Maybe Int
lookupvar = lookup

-- helper function to update variable in memory
updatevar :: Name -> Int -> Mem -> Mem
updatevar x v [] = [(x,v)]
updatevar x v ((y,w):ys)
    | x == y = (x,v):ys
    | otherwise = (y,w) : updatevar x v ys

-- helper function to evaluate Ops 
eval :: Op -> Int -> Int -> Int
eval Add x y = x + y
eval Sub x y = x - y
eval Mul x y = x * y
eval Div x y = div x y

-- helper function to return code given a label
findlabel :: Label -> Code -> Code
findlabel _ [] = []
findlabel l (c:cs) = 
    case c of
        -- found label
        LABEL k | k == l -> c : cs
        -- any other code
        _                -> findlabel l cs

-- helper function that takes current code, full code, stack and memory and returns new memory after executing all instructions
go :: Code -> Code -> Stack -> Mem -> Mem
go [] _ _ m = m
go (c:cs) fc s m = 
    case c of
        PUSH n -> go cs fc (n:s) m
        PUSHV x -> go cs fc (maybe 0 id (lookupvar x m) : s) m
        POP x -> case s of
                    (v:s') -> go cs fc s' (updatevar x v m)
                    [] -> []
        DO op -> case s of
                    (y:x:s') -> go cs fc (eval op x y : s') m
                    _        -> []
        JUMP l -> go (findlabel l fc) fc s m
        JUMPZ l -> case s of
                     (v:s') -> if v == 0
                               then go (findlabel l fc) fc s' m
                               else go cs fc s' m
                     []     -> []
        LABEL _ -> go cs fc s m


-- final exec function
exec :: Code -> Mem
exec code = go code code [] []



