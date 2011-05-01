module Shape where

{-
    We start with some type definitions. With type definitions
    we simply introduce new names for basic (or user-defined) types.
-}
type Length = Double           -- we define the lengths of our shapes to be doubles
type Point  = (Double, Double) -- a point its defined by its x and y coordinates

type Id = Int -- we will use id's to identify various objects;
              -- each shape will have its own id

{-
    We continue with some data type definitions. A data type
    definition introduces a new data type (somewhat like a class
    in Java/C++) and one or more constructors for it. Constructors
    are used to create instances of that data type.
-}

-- The data type Shape has constructors for each type of shape that we need
-- to represent. Each constructor takes a combination of Point's and Length's,
-- depending on the particular shape it represents.
data Shape = Line Point Point      -- a line is defined by its two extremities
           | Square Point Length   -- a square is defined by its (say) top left
                                   -- corner and the length of its side
           | Rectangle Point Point -- a rectangle is defined by its (say) top left
                                   -- and bottom rigth corners
           | Circle Point Length   -- a circle is defined by its center and its radius

           deriving (Eq, Show)     -- this "deriving" thing is a Haskell mechanism
                                   -- for allowing comparison (the Eq part) and
                                   -- output to console (the Show part) of the
                                   -- instances of the data type

-- The data type Color predefines a number of colors that our shapes can have.
-- Naturally, in a real scenario, we'd be using RBG (i.e., three integer values)
-- encoding instead. Black, White, Red, Green and Blue are all paramterless
-- constructors for the Color data type.
data Color = Black | White | Red | Green | Blue
           deriving (Eq, Show)

-- The data type ShapeObj brings together a shape, a color and an id for the
-- shape object. We could, for example, use two colors instead of one if we
-- wanted to encode background as well as foreground colors. In this encoding
-- we assume all shapes are empty (i.e., no background color) and that the
-- color is the foreground color.
data ShapeObj = ShapeObj Id Shape Color
              deriving (Eq, Show)

{-
    Now we define constructors for each of our four shapes. These are, in essence,
    function equivalents of the data type constructors. They are not really
    necessary (one can use the data type constructors directly), but they introduce
    an extra level an abstraction, which can be useful in some cases.
-}

-- Constructor for lines. Takes the two extremities of
-- the line as arguments.
line :: Point -> Point -> Shape
line = Line

-- Constructor for squares. First argument is top left corner,
-- second one is length of its side.
square :: Point -> Length -> Shape
square = Square

-- Constructor for rectangles. First argument is top left corner,
-- second one is bottom right corner.
rectangle :: Point -> Point -> Shape
rectangle = Rectangle

-- Constructor for circles. First argument is center,
-- second one is radius.
circle :: Point -> Length -> Shape
circle = Circle

-- Constructor for shape objects. First argument is an integer
-- id, second one is a shape and third one is a color.
makeShape :: Id -> Shape -> Color -> ShapeObj
makeShape = ShapeObj

{-
    Next we define some getters for each component of a shape object. These
    allow easier access to the id, shape and color of a shape object.
-}

-- Returns the id of a shape object.
idShape :: ShapeObj -> Id
idShape (ShapeObj i _ _) = i

-- Returns the color of a shape object.
colorShape :: ShapeObj -> Color
colorShape (ShapeObj _ _ c) = c

-- Returns the shape of a shape object.
shape :: ShapeObj -> Shape
shape (ShapeObj _ s _) = s

{-
    Finally, we define two sample operations for a shape object.

    Changing the properties of an object is different in functional
    programming than in OO programming due to the fact that functional
    variables are immutable. So we can't simply change one property or
    another at will as we can in OO, we have to create new objects
    instead that copy the unaltered properties of the old ones and
    use different values for the altered properties. So the general
    schema is:

      (modifierOperation oldObject) will return a newObject, which
      is a copy of oldObject, but with the desired properties changed.
-}

-- Shifts a shape object with dx on the x-axis and with dy
-- on the y-axis.
--
-- Notice that we are creating a new shape object (using the makeShape
-- constructor) which copies the id and color of the old one, but
-- uses a new shape whose coordinates place it at the shifted position.
shift :: ShapeObj -> Length -> Length -> ShapeObj
shift (ShapeObj i s c) dx dy = makeShape i (shiftShape s dx dy) c

-- This is the method that does the shifting proper. It takes a shape
-- and two deltas (dx and dy) and returns a new, shifted shape. Since
-- each particular shape we support (line, square, rectangle, circle)
-- is affected differently, we need to provide implementation for each
-- of the four shapes. In each case, we use the proper constructor
-- to create the new, shifted shape.
shiftShape :: Shape -> Length -> Length -> Shape
shiftShape (Line (x1,y1) (x2,y2)) dx dy      = line (x1+dx,y1+dy) (x2+dx,y2+dy)
shiftShape (Square (x,y) l) dx dy            = square (x+dx,y+dy) l
shiftShape (Rectangle (x1,y1) (x2,y2)) dx dy = rectangle (x1+dx,y1+dy) (x2+dx,y2+dy)
shiftShape (Circle (x,y) r) dx dy            = circle (x+dx,y+dy) r

-- Changes the color of a shape object to the specified color.
--
-- Again, we are creating a new shape object (using the makeShape
-- constructor) which copies the id and the shape of the old shape
-- object, but changes its color to the specified one. The old color
-- is ignored by using _ for matching its position.
changeColor :: ShapeObj -> Color -> ShapeObj
changeColor (ShapeObj i s _) c = makeShape i s c

{-
    The code above represents the design. What follows is some test code
    that creates a few objects, shifts them all with the same deltas and
    then changes all their colors to red.
-}

-- define a few objects
myShapeObjects = [ makeShape 1 (line      (1,1) (10,10)) Black
                 , makeShape 2 (line      (4,5) (-2,5) ) Red
                 , makeShape 3 (square    (4,19) 5     ) Blue
                 , makeShape 4 (square    (-3,-4) 10   ) Blue
                 , makeShape 5 (rectangle (1,4) (5,2)  ) White
                 , makeShape 6 (rectangle (2,10) (7,4) ) Black
                 , makeShape 7 (circle    (5,5) 4      ) Red
                 , makeShape 8 (circle    (6,-8) 3     ) Green
                 ]

-- shift all the shape objects in a list with 10 on the x-axis and 10 on the y-axis
shiftAll = map (\x -> shift x 10 10)

-- change the color of all the shape objects in a list to red
changeColorAll = map (\x -> changeColor x Red)

-- get the id's of all the shape objects in a list
getAllIds = map idShape

-- get the colors of all the shape objects in a list
getAllColors = map colorShape

-- get the shapes of all the shape objects in a list
getAllShapes = map shape

