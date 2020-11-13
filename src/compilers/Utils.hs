module Utils where

nop, endl :: ShowS
nop = showString ""
endl = showString "\n"

showSify :: [String] -> ShowS
showSify = foldr (\x acc -> showString x . endl . acc) nop
