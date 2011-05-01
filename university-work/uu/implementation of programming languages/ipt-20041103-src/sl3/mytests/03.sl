--
-- Just like the previous test, but it adds a new declaration,
-- to show how the environments are handled with more than one
-- declaration in a let expression.
--

let id = \x -> x
 ;  a = 5
 in (\x -> x) id
 ni
