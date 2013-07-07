import Data.Char


schar :: (Char->Bool)->ReadS Char
schar f (c:s) | f c = [(c,s)]
schar f _ = []

fromchartoint c = fromEnum c - 48 

variable s = map f (schar isDigit s)
	where f (c,s1) = (fromchartoint c,s1)

variablenf 0 s int0 = [(int0,s)]
variablenf n s int0 = case variable s of
                      [] -> [(nil,s)]
                      [(int1,sv)] -> variablenf (n-1) sv (int0*10+int1)
variables s = variablenf (length s) s 0


mapP :: (a->b)->ReadS a -> ReadS b
mapP f g = map (\(c,s)->(f c,s)) . g


nil :: ReadS [a]
nil s = [([],s)]

(&&&) :: ReadS a -> ReadS b -> ReadS (a, b)
f &&& g = \s -> [ ((x, y), s2) 
                | (x, s1) <- f s, 
                  (y, s2) <- g s1 ]


(|||) :: ReadS a -> ReadS b -> ReadS (Either a b)
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

variablesd = mapP f (many variable)
  where f s = foldl (\x y -> x*10 +y) 0 s



