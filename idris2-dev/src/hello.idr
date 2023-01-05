

list_len : List a -> Nat
list_len [] = ?rhs
list_len (_ :: xs) = S (list_len xs)
