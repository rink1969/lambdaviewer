import Data.Char

data Var = V String
data Term = Var Var | Lam Var Term | App Term Term

test1 = App (App (Var (V "g")) (Var (V "h")))
			(App (Var (V "i")) (Var (V "j")))

instance Show Var where
	show (V s) = s

instance Show Term where
 	show (Var v) = show v
 	show (Lam v t) = "("++"\\" ++ show v ++ "." ++ show t ++ ")"
 	show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"


test2 = (Lam (V "x") (App (Var (V "x")) (Var (V "x"))))
test3 = (App (Lam (V "x") (Var (V "x"))) (Var (V "x")))


schar :: (Char->Bool)->ReadS Char
schar f (c:s) | f c = [(c,s)]
schar f _ = []

variable s = map f (schar isAlpha s)
	where f (c,s1) = (V [c],s1)

variable2 s = concat (map f (schar isAlpha s))
	where f (c1,s1) = map (f1 c1) (schar isAlpha s1)
		where f1 c2 (c3,s2) = (V ([c2]++[c3]),s2)

variablenf 0 s sn = [(V sn,s)]
variablenf n s sn = variablenf (n-1) sv (sn++vc)
	where [(V vc,sv)] = variable s
variablen n s = variablenf n s "" 

instance Read Var where
	readsPrec _ = variable


mapP :: (a->b)->ReadS a -> ReadS b
mapP f g = map (\(c,s)->(f c,s)) . g

nil :: ReadS [a]
nil s = [([],s)]

(&&&) :: ReadS a -> ReadS b -> ReadS (a, b)
f &&& g = \s -> [ ((x, y), s2) 
                | (x, s1) <- f s, 
                  (y, s2) <- g s1 ]


--(|||) :: ReadS a -> ReadS b -> ReadS (Either a b)
--f ||| g = \s -> map right (f s) ++ map right (g s)
--  where left  (x, s) = (Left  x, s)
--        right (y, s) = (Right y, s)

f ||| g = \s -> case f s of
                  [] -> map right (g s)
                  xs -> map left xs
  where left  (x, s) = (Left  x, s)
        right (y, s) = (Right y, s)


(<|>) :: ReadS a -> ReadS a -> ReadS a
f <|> g = mapP select (f ||| g)
  where select (Left  x) = x
        select (Right y) = y


many, many1 :: ReadS a -> ReadS [a]
many r  = many1 r <|> nil
many1 r = mapP cons (r &&& many r)
  where cons (x, xs) = x : xs

variables = mapP f (many1 (schar isAlpha))
	where f s = V s

term, atom :: ReadS Term
term = mapP (foldl1 App) (many1 atom)
atom = (trim lam) <|> (trim var) <|> (trim (paren term))
var = mapP f (trim variables)
  where f v = Var v
lam  = mapP f ((trim lbd) &&& (trim variables) &&& (trim (sym '.')) &&& (trim term))
  where f (((_,v),_),e) = Lam v e
app  = mapP f (term &&& space &&& term)
  where f ((f, _), e) = App f e
lbd   = sym '\\' <|> sym 'λ'
space = many1 (sym ' ')
sym c = schar (==c)

paren :: ReadS b -> ReadS b
paren p = mapP f (sym '(' &&& p &&& sym ')')
    where f ((_, x), _) = x

trim :: ReadS b -> ReadS b
trim p = mapP f ((space ||| nil) &&& p &&& (space ||| nil))
    where f ((_, x), _) = x


instance Read Term where
    readsPrec _ = term