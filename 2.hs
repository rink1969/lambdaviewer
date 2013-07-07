data Var = V String
data Term = Var Var | Lam Var Term | App Term Term

test1 = App (App (Var (V "g")) (Var (V "h")))
			(App (Var (V "i")) (Var (V "j")))

instance Show Var where
	show (V s) = s

instance Show Term where
 	show (Var v) = show v
 	show (Lam v t) = "\\" ++ show v ++ "." ++"("++ show t ++ ")"
 	show (App t1 t2) = show t1 ++ " " ++ show t2


test2 = (Lam (V "x") (App (Var (V "x")) (Var (V "x"))))
test3 = (App (Lam (V "x") (Var (V "x"))) (Var (V "x")))