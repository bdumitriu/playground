module TT where
f = \x -> g (g x)
g = \x -> f x
