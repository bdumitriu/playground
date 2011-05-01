module Shape where

{- Data type definitions. -}

data Shape = Line (Double,Double) (Double,Double) -- a line is defined by its two extremities
           | Circle (Double,Double) Double   -- a circle is defined by its center and its radius
           deriving (Eq, Show)

data Color = Black | White | Red | Green | Blue
           deriving (Eq, Show)

data ShapeObj = ShapeObj Shape Color
              deriving (Eq, Show)

{- Some sample getters. -}

-- Returns the color of a shape object.
colorOf :: ShapeObj -> Color
colorOf (ShapeObj _ c) = c

-- Returns the shape of a shape object.
shapeOf :: ShapeObj -> Shape
shapeOf (ShapeObj s _) = s

{- Some sample setters. -}

-- Shifts a shape object with dx on the x-axis and with dy
-- on the y-axis.
shift :: ShapeObj -> Double -> Double -> ShapeObj
shift (ShapeObj s c) dx dy = ShapeObj (shiftShape s dx dy) c

shiftShape :: Shape -> Double -> Double -> Shape
shiftShape (Line (x1,y1) (x2,y2)) dx dy = Line (x1+dx,y1+dy) (x2+dx,y2+dy)
shiftShape (Circle (x,y) r) dx dy       = Circle (x+dx,y+dy) r

-- Changes the color of a shape object to the specified color.
changeColor :: ShapeObj -> Color -> ShapeObj
changeColor (ShapeObj s _) c = ShapeObj s c

{- Some test functions. -}

-- define a few objects
myShapeObjects = [ ShapeObj (Line      (1,1) (10,10)) Black
                 , ShapeObj (Line      (4,5) (-2,5) ) Red
                 , ShapeObj (Circle    (5,5) 4      ) Red
                 , ShapeObj (Circle    (6,-8) 3     ) Green
                 ]

-- get the colors of all the shape objects in a list
getAllColors = map colorOf

-- get the shapes of all the shape objects in a list
getAllShapes = map shapeOf

-- shift all the shape objects in a list with 10 on the x-axis and 10 on the y-axis
shiftAll = map (\x -> shift x 10 10)

-- change the color of all the shape objects in a list to red
changeColorAll = map (\x -> changeColor x Red)

-- main function
main :: IO()
main = do putStrLn ""
          putStrLn "Testing shapes..."
          putStrLn ""
          putStrLn "List of predefined shapes:"
          putStrLn (show myShapeObjects)
          putStrLn ""
          putStrLn "The list of colors of predefined shapes:"
          putStrLn (show (getAllColors myShapeObjects))
          putStrLn ""
          putStrLn "The list of shapes of predefined shapes:"
          putStrLn (show (getAllShapes myShapeObjects))
          putStrLn ""
          putStrLn "The list of shifted (with (5,5)) predefined shapes:"
          putStrLn (show (shiftAll myShapeObjects))
          putStrLn ""
          putStrLn "The list of predefined shapes with colors changed to red:"
          putStrLn (show (changeColorAll myShapeObjects))
          putStrLn ""
          putStrLn "The end."

