import           Control.Monad  (ap, liftM)
import           Data.Semigroup
import           Data.Tree
import           Prelude        hiding ((/))

infixl 7 :*:
infixl 6 :+:
infixr 8 ^.
(^.) :: (Expr a) -> (Expr a) -> (Expr a)
x ^. y = Power(x,y)
a / b = (N a) :*: ((N b) ^. (N (-1)))

data Expr a =
  (Expr a) :+: (Expr a)
  | (Expr a) :*: (Expr a)
  | Power (Expr a, Expr a)
  | N Integer
  | Exp (Expr a)
  | Log (Expr a)
  | Sin (Expr a)
  | Cos (Expr a)
  | ArcTan (Expr a)
  | X(a)
  | DX(a)
            deriving (Eq, Show)

d :: Expr a -> Expr a
d expr = case expr of
        (x :+: y) -> ( d x :+: d y)
        Power (x, y) -> (Log x) :*: (Power (x,y) ) :*: (d y) :+: Power(x, y :+: N (-1)) :*: (d x)
        --Naive power rule: No variables in exponents!
        --      Power (x, y) -> Power(x, y :+: N (-1)) :*: (d x)
        (x :*: y) -> (d x) :*: y :+: x :*: (d y)
        N _ -> N 0
        Exp x -> (Exp x) :*: (d x)
        Log x -> x ^. (N (-1)) :*: (d x)
        Sin x -> (Cos x) :*: (d x)
        Cos x -> (N (-1)) :*: (Sin x) :*: (d x)
        ArcTan x -> ((N 1) :+: x ^. (N 2))^.(N(-1)):*:(d x)
        X x -> DX x

nestWhile :: (t -> t) -> t -> (t -> t -> Bool) -> t
nestWhile f x cond =
  let fx = f x in
    if not(cond x fx) then x
    else nestWhile f fx cond

--Some naive simplification via finite-state machine
simplify :: Eq a => Expr a -> Expr a
simplify expr = nestWhile go expr (\x y -> x /= y)
  where go u =
          case u of
               (N x) :+: (N y) -> N (x+y)
               (N x :*: N y)   -> N (x*y)
               (N 0 :*: _)     -> N 0
               (_ :*: N 0)     -> N 0
               (N 1 :*: x)     -> x
               (x :*: N 1)     -> x
               (x :+: N 0)     -> x
               (N 0 :+: x)     -> x
               Power (x, N 0)  -> N 1
               Power (N 0, x)  -> N 0
               Power (N 1, x)  -> N 1
               Power (x, N 1)  -> x
               (x :+: y)       -> simplify x :+: simplify y
               Power (x, y)    -> Power (simplify x, simplify y)
               (x :*: y)       -> (simplify x :*: simplify y)
               Exp (N 0)       -> N 1
               Log (N 1)       -> N 0
               Cos (N 0)       -> N 1
               Sin (N 0)       -> N 0
               ArcTan (N 0)    -> N 0
               Exp x           -> (Exp (simplify x))
               Log x           -> (Log (simplify x))
               Cos x           -> (Cos (simplify x))
               Sin x           -> (Sin (simplify x))
               ArcTan x        -> (ArcTan (simplify x))
               X x             -> X x
               N x             -> N x
               DX x            -> DX x


instance Monad Expr where
  (X x) >>= f = f x
  (N x) >>= f = N x
  (x :*: y) >>= f = (x>>=f) :*: (y>>=f)
  (x :+: y) >>= f = (x>>=f) :+: (y>>=f)
  (Power(x,y)) >>= f = Power(x>>=f,y>>=f)
  (Exp x) >>= f = Exp(x>>=f)
  (Log x) >>= f = Log(x>>=f)
  (Cos x) >>= f = Cos(x>>=f)
  (Sin x) >>= f = Sin(x>>=f)
  (ArcTan x) >>= f = ArcTan(x>>=f)
  (DX x) >>= f = d(f x)
  return a = X(a)

instance Functor Expr where
    fmap = liftM
instance Applicative Expr where
    pure = return
    (<*>) = ap

--Composition semigroup: in *one* variable only (we do not check!)
instance Semigroup (Expr a) where
  f <> g = f >>= (\x -> g)

-- Conversion to a tree
toTree :: Show a => (Expr a) -> Tree [Char]
toTree expr = case expr of
    (x :+: y)    -> Node "Plus" [(toTree x), (toTree y)]
    Power (x, y) -> Node "Power" [(toTree x), (toTree y)]
    (x :*: y)    -> Node "Times" [(toTree x), (toTree y)]
    N x          -> Node (show x) []
    Exp x        -> Node "Exp" [toTree x]
    Log x        -> Node "Log" [toTree x]
    Sin x        -> Node "Sin" [toTree x]
    Cos x        -> Node "Cos" [toTree x]
    ArcTan x     -> Node "ArcTan" [toTree x]
    X x          -> Node ("X" ++ (show x)) []
    DX x         -> Node ("DX" ++ (show x)) []


-- Conversion to Mathematica input
mma :: Show a => Expr a -> [Char]
mma expr = case expr of
    (x :+: y)    -> "Plus[" ++ (mma x) ++ "," ++(mma y)++"]"
    Power (x, y) -> "Power[" ++ (mma x) ++ "," ++(mma y)++"]"
    (x :*: y)    -> "Times[" ++ (mma x) ++ "," ++(mma y)++"]"
    N x          -> (show x)
    Exp x        -> "Exp["++(mma x)++"]"
    Log x        -> "Log["++(mma x)++"]"
    Sin x        -> "Sin["++(mma x)++"]"
    Cos x        -> "Cos["++(mma x)++"]"
    ArcTan x     -> "ArcTan["++(mma x)++"]"
    X x          -> "x"++(show x)
    DX x         -> "dx"++(show x)

-- Draw the expression:
drawExp :: Show a => Expr a -> IO ()
drawExp x = putStr $ drawTree $ toTree x
