	FirstLiterateScript.lhs
	
	Bogdan Dumitriu, Sep 2004
	
	The purpose of this script is
	- to illustrate some simple definitions over integers (Int)
	- to give a first example of a script.

The value size is an integer (Int), defined to be the sum of 12 and 13

> size :: Int
> size = 12 + 13

The function to square an integer.

> square :: Int -> Int
> square n = n * n

The function to double an integer.

> double :: Int -> Int
> double n = 2 * n

An example using double, square and size.

> example :: Int
> example = double (size - square (2 + 2))

The function doubles its input and squares the result of that.

> doublesq :: Int -> Int
> doublesq n = square (double n)

The function squares its input and doubles the result of that.

> sqdouble :: Int -> Int
> sqdouble n = double (square n)