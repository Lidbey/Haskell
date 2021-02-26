import Prelude hiding (print)
data Expr 
 = S Char
 | N Expr
 | A Expr Expr
 | C Expr Expr
 | I Expr Expr

print :: Expr -> [Char]
print (S x) = [x]
print (N x) = "~" ++ print x
print (A x y) = "("++ print x ++ " | " ++ print y ++ ")"
print (C x y) = "("++ print x ++ " & " ++ print y ++ ")"
print (I x y) = "("++ print x ++ " => " ++ print y ++ ")"

uniq:: [Char] -> [Char]
uniq a=if not(null a) then if not(elem (head a) (tail a)) then ([(head a)] ++ uniq (tail a)) else (uniq (tail a)) else a

write_variables :: Expr -> [Char]
write_variables (S x) = [x]
write_variables (N x) = write_variables x
write_variables (A x y) = uniq (write_variables x ++ write_variables y)
write_variables (C x y) = uniq (write_variables x ++ write_variables y)
write_variables (I x y) = uniq (write_variables x ++ write_variables y)

putArray :: [Char] -> [Char]
putArray [] = []
putArray n = "," ++ [(head n)] ++ (putArray (tail n))

write_variablesX :: Expr -> IO ()
write_variablesX n = putStrLn ( "[" ++ (tail (putArray (write_variables n))) ++ "]")

check :: Expr -> [(Char, Bool)] -> Bool
check (S x) l = if (x==(fst (head l))) then (snd (head l)) else (check (S x) (tail l))
check (N x) l = not (check x l)
check (A x y) l = (check x l) || (check y l)
check (C x y) l = (check x l) && (check y l)
check (I x y) l = (not (check x l)) || ((check x l) && (check y l))

is_tautology :: Expr -> Bool
is_tautology n = check_variations n (write_variables n) (permutate [] (length (write_variables n)))

permutate :: [Bool] -> Int -> [[Bool]]
permutate a 0 = [a]
permutate a n = (permutate (a ++ [True]) (n-1)) ++ (permutate (a ++ [False]) (n-1))

check_variations :: Expr -> [Char] -> [[Bool]] -> Bool
check_variations n c [] = True
check_variations n c p = if(check n (zip c (head p))) then (check_variations n c (tail p)) else False

-- (I (C (I (S 'A') (S 'B')) (I (S 'B') (S 'C'))) (I (S 'A') (S 'C'))) <- tautology from wikipedia tested