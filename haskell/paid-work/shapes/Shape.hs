module Shape (
  Shape, Object, ID, SizeType,
  Colour(Black, White, Red, Green, Blue),
  ellipse, polygon, object,
  idO, areaO, colourO, sidesO, shapeO, sideListO
) where

import Utils

{- Solution to part 2. -}

{- Some type definitions. -}
type Vertex   = (Double, Double)
type Radius   = Double
type SizeType = Double

type ID = Int

{- Data type Shape. -}
data Shape = Ellipse Vertex Vertex Radius Radius
           | Polygon [Vertex]
           deriving (Eq, Show)

{- Data type Colour. -}
data Colour = Black | White | Red | Green | Blue
            deriving (Eq, Show)

{- Data type Object. -}
data Object = Obj ID Shape Colour
            deriving (Eq, Show)

{- Constructor for polygons. -}
polygon :: [Vertex] -> Shape
polygon = Polygon

{- Constructor for ellipses. -}
ellipse :: Vertex -> Vertex -> Radius -> Radius -> Shape
ellipse = Ellipse

{- Constructor for objects. -}
object :: ID -> Shape -> Colour -> Object
object = Obj

{- Returns the id of an object. -}
idO :: Object -> ID
idO (Obj i _ _) = i

{- Computes and returns the area of an object. -}
areaO :: Object -> SizeType
areaO (Obj _ (Polygon vs       ) _) = areaPoly vs
areaO (Obj _ (Ellipse _ _ r1 r2) _) = pi * r1 * r2

{- Returns the colour of an object. -}
colourO :: Object -> Colour
colourO (Obj _ _ c) = c

{- Returns the number of sides of an object. -}
sidesO :: Object -> Int
sidesO (Obj _ (Polygon vs     ) _) = length vs
sidesO (Obj _ (Ellipse _ _ _ _) _) = 1

{- Returns the shape of an object. -}
shapeO :: Object -> Shape
shapeO (Obj _ s _) = s

{- Returns the list of side lengths for a polygon and the two radii for an ellipse. -}
sideListO :: Object -> [SizeType]
sideListO (Obj _ (Polygon (v:vs)   ) _) = zipWith side (v:vs) (vs ++ [v])
sideListO (Obj _ (Ellipse _ _ r1 r2) _) = [r1, r2]

{-
        Computes and returns the area of a polygon by summing the area
        of the triangles it is composed of.
-}
areaPoly :: [Vertex] -> SizeType
areaPoly (v:vs) = let
                    triangles = zipWith3 (\x y z -> (x, y, z)) (repeat v) (init vs) (tail vs)
                  in
                    sum . map areaTriangle $ triangles

{- Computes and returns the area of a triangle. -}
areaTriangle :: (Vertex, Vertex, Vertex) -> SizeType
areaTriangle (v1, v2, v3) = sqrt (s * (s - a) * (s - b) * (s - c))
  where a = side v1 v2
        b = side v1 v3
        c = side v2 v3
        s = (a + b + c) / 2

{- Computes and returns the distance between two vertices. -}
side :: Vertex -> Vertex -> SizeType
side (v1x, v1y) (v2x, v2y) = sqrt ((v2x-v1x)^^2 + (v2y-v1y)^^2)
