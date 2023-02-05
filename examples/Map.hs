module List where

map :: (a -> b) -> [a] -> [b]
map f [

filter :: (a->Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = if p x then x:filter p xs
                  else p xs
