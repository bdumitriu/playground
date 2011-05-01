module TestData (
  testSigFig,
  testDiffColour,
  testDiffArea,
  testDiffBasicShape,
  testDiffShape
) where

import Utils
import Shape
import OddOneOut

{- Test data for the sigFig function. -}

numbers = [123.456, 123.456, 0.987, 0.432, 34.256, 0.4646, 0.4646]

figures = [2      , 4      , 2    , 2    , 3     , 4     , 1     ]

results = [120.0  , 123.5  , 0.99 , 0.43 , 34.3  , 0.4646, 0.5   ]

{-
        Applies sigFig to corresponding pairs in the numbers and figures lists
        and compares the result to the corresponding one in the results list.
 -}
testSigFig = and (zipWith check (zipWith sigFig numbers figures) results)

check x y
  | x == y      = True
  | otherwise   = error ("Expected: " ++ (show y) ++ "; Got: " ++ (show x) ++ ".")

{- Ellipses and polygons for testing various functions. -}

e0 = object 1 (ellipse (1, 1) (3, 1) 4 3) White
e1 = object 2 (ellipse (1, 1) (3, 1) 4 3) Black
e2 = object 3 (ellipse (1, 1) (3, 1) 4 3) Red
e3 = object 4 (ellipse (1, 1) (3, 1) 4 3) Green
e4 = object 5 (ellipse (1, 1) (3, 1) 4 3) Blue

e10 = object 10 (ellipse (1, 1) (3, 1) 5 1) White
e11 = object 11 (ellipse (1, 1) (3, 1) 5 1) Black
e12 = object 12 (ellipse (1, 1) (3, 1) 5 1) Red
e13 = object 13 (ellipse (1, 1) (3, 1) 5 1) Green
e14 = object 14 (ellipse (1, 1) (3, 1) 5 1) Blue

e20 = object 20 (ellipse (1, 1) (3, 1) 5 3) White
e21 = object 21 (ellipse (1, 1) (3, 1) 5 3) Black
e22 = object 22 (ellipse (1, 1) (3, 1) 5 3) Red
e23 = object 23 (ellipse (1, 1) (3, 1) 5 3) Green
e24 = object 24 (ellipse (1, 1) (3, 1) 5 3) Blue

e30 = object 30 (ellipse (1, 1) (3, 1) 6 2) White
e31 = object 31 (ellipse (1, 1) (3, 1) 6 2) Black
e32 = object 32 (ellipse (1, 1) (3, 1) 6 2) Red
e33 = object 33 (ellipse (1, 1) (3, 1) 6 2) Green
e34 = object 34 (ellipse (1, 1) (3, 1) 6 2) Blue

e40 = object 40 (ellipse (1, 1) (3, 1) 2 6) White
e41 = object 41 (ellipse (1, 1) (3, 1) 2 6) Black
e42 = object 42 (ellipse (1, 1) (3, 1) 2 6) Red
e43 = object 43 (ellipse (1, 1) (3, 1) 2 6) Green
e44 = object 44 (ellipse (1, 1) (3, 1) 2 6) Blue

{- an ellipse whose area is very close to 16 -}
e50 = object 50 (ellipse (1, 1) (3, 1) 2.546479 2) White
e51 = object 51 (ellipse (1, 1) (3, 1) 2.546479 2) Black
e52 = object 52 (ellipse (1, 1) (3, 1) 2.546479 2) Red
e53 = object 53 (ellipse (1, 1) (3, 1) 2.546479 2) Green
e54 = object 54 (ellipse (1, 1) (3, 1) 2.546479 2) Blue

{- hexagon -}
p0 = object 100 (polygon [(1, 0), (2, 0), (3, 1), (2, 2), (1, 2), (0, 1)]) White
p1 = object 101 (polygon [(1, 0), (2, 0), (3, 1), (2, 2), (1, 2), (0, 1)]) Black
p2 = object 102 (polygon [(1, 0), (2, 0), (3, 1), (2, 2), (1, 2), (0, 1)]) Red
p3 = object 103 (polygon [(1, 0), (2, 0), (3, 1), (2, 2), (1, 2), (0, 1)]) Green
p4 = object 104 (polygon [(1, 0), (2, 0), (3, 1), (2, 2), (1, 2), (0, 1)]) Blue

{- square with side of 4 -}
p10 = object 110 (polygon [(0, 0), (4, 0), (4, 4), (0, 4)]) White
p11 = object 111 (polygon [(0, 0), (4, 0), (4, 4), (0, 4)]) Black
p12 = object 112 (polygon [(0, 0), (4, 0), (4, 4), (0, 4)]) Red
p13 = object 113 (polygon [(0, 0), (4, 0), (4, 4), (0, 4)]) Green
p14 = object 114 (polygon [(0, 0), (4, 0), (4, 4), (0, 4)]) Blue

{- rectangle with sides of 2 and 8 -}
p20 = object 120 (polygon [(0, 0), (2, 0), (2, 8), (0, 8)]) White
p21 = object 121 (polygon [(0, 0), (2, 0), (2, 8), (0, 8)]) Black
p22 = object 122 (polygon [(0, 0), (2, 0), (2, 8), (0, 8)]) Red
p23 = object 123 (polygon [(0, 0), (2, 0), (2, 8), (0, 8)]) Green
p24 = object 124 (polygon [(0, 0), (2, 0), (2, 8), (0, 8)]) Blue

{- rectangle with sides of 1 and 4 -}
p30 = object 130 (polygon [(0, 0), (1, 0), (1, 4), (0, 4)]) White
p31 = object 131 (polygon [(0, 0), (1, 0), (1, 4), (0, 4)]) Black
p32 = object 132 (polygon [(0, 0), (1, 0), (1, 4), (0, 4)]) Red
p33 = object 133 (polygon [(0, 0), (1, 0), (1, 4), (0, 4)]) Green
p34 = object 134 (polygon [(0, 0), (1, 0), (1, 4), (0, 4)]) Blue

{- rectangle with sides of 4 and 16, but starting the list with a different vertex -}
p40 = object 140 (polygon [(4, 0), (4, 16), (0, 16), (0, 0)]) White
p41 = object 141 (polygon [(4, 0), (4, 16), (0, 16), (0, 0)]) Black
p42 = object 142 (polygon [(4, 0), (4, 16), (0, 16), (0, 0)]) Red
p43 = object 143 (polygon [(4, 0), (4, 16), (0, 16), (0, 0)]) Green
p44 = object 144 (polygon [(4, 0), (4, 16), (0, 16), (0, 0)]) Blue

{- rectangle with sides of 16 and 2 -}
p50 = object 150 (polygon [(0, 0), (16, 0), (16, 2), (0, 2)]) White
p51 = object 151 (polygon [(0, 0), (16, 0), (16, 2), (0, 2)]) Black
p52 = object 152 (polygon [(0, 0), (16, 0), (16, 2), (0, 2)]) Red
p53 = object 153 (polygon [(0, 0), (16, 0), (16, 2), (0, 2)]) Green
p54 = object 154 (polygon [(0, 0), (16, 0), (16, 2), (0, 2)]) Blue

{- rectangle with sides of 8 and 1 -}
p60 = object 160 (polygon [(0, 0), (8, 0), (8, 1), (0, 1)]) White
p61 = object 161 (polygon [(0, 0), (8, 0), (8, 1), (0, 1)]) Black
p62 = object 162 (polygon [(0, 0), (8, 0), (8, 1), (0, 1)]) Red
p63 = object 163 (polygon [(0, 0), (8, 0), (8, 1), (0, 1)]) Green
p64 = object 164 (polygon [(0, 0), (8, 0), (8, 1), (0, 1)]) Blue

{-
        triangles to test odd one out when vertices are specifified in
        both clockwise and counterclockwise manner
-}
p70 = object 170 (polygon [(0, 0), (3, 0), (0, 4)]) White
p71 = object 171 (polygon [(0, 0), (6, 0), (0, 8)]) White
p72 = object 172 (polygon [(0, 0), (0, 4), (3, 0)]) White
p73 = object 173 (polygon [(0, 0), (0, 8), (6, 0)]) White
p74 = object 174 (polygon [(0, 0), (0, 9), (6, 0)]) White

{- odd one out has different colour -}
diffColour = [
                [p12, p12, p12, p14],
                [p12, p12, p14, p12],
                [p14, p12, p12, p12],
                [e4, e14, p24, p30, p44],
                (take 1000 . repeat $ p12) ++ [p34],
                p34 : (take 1000 . repeat $ p12)
             ]

testDiffColour = testOddOneOut diffColour

{- odd one out has different area -}
diffArea = [
                [e0, e1, e2, e3, e10, e4],
                [e11, e12, e13, e22],
                [e50, p11, p22, p50, p24, e54],
                [p30, p20, p22, p24, p12],
                [p50, p51, p52, p34],
                (take 1000 . repeat $ e14) ++ [e30, e12],
                e12 : e30 : (take 1000 . repeat $ e14)
           ]

testDiffArea = testOddOneOut diffArea

{- odd one out has different basic shape -}
diffBasicShape = [
                        [e50, e50, p20],
                        [e50, e50, p20, e50],
                        [e0, e11, e22, p14],
                        (take 500 . repeat $ e50) ++ [p20] ++ (take 500 . repeat $ e50)
                 ]

testDiffBasicShape = testOddOneOut diffBasicShape

{- odd one out has different shape -}
diffShape = [
                [p10, p11, p12, p20],
                [p20, p31, p42, p50, p22, p34],
                [p14, p50, p51, p52, p64],
                [p70, p71, p72, p73, p74],
                (take 500 . repeat $ p40) ++ [p60] ++ (take 500 . repeat $ p20)
            ]

testDiffShape = testOddOneOut diffShape

testOddOneOut = map oddOneOut