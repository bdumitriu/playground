module OddOneOut (
  oddOneOut
) where

import Utils
import Shape

{-
        Solution to part 3. 

        Since it was not specified, in computing side length rations, we have
        assumed that the vertices of polygons are always specified either all
        in clockwise order or all in counterclockwise order.

        Here is how the algorithm works: for each property that is tracked, a data
        structure represented by a tuple of (Group a)'s is maintained. We start with
        the value (None, None), which indicates that we haven't seen anything yet.
        The first time we see a particular value of a property, we turn the first
        available None in the tuple into a One, saving the value of the property we
        have just seen and the id of the current object (the one having that value
        for the property) in the arguments of the One. If we already have a One in
        the tuple with that value for the property saved, we turn that into a Many  
        which stores the same value for that property, dropping the id of the object,
        since it is no longer relevant. If we have a Many in the tuple with that value
        for the property saved, we advance to the next object without making any changes
        to the tuple. If none of the above is the case, and we have no more None's
        either, then it means there is no odd one out with that property (either
        there have appeared 3 or more different values for the property, or just 2,
        but with each appearing at 2 or more objects). In this case, we turn the
        tuple into a (Bust, Bust), indicating that we have no odd one out for that
        property. We also have no odd one out if we end up with a (Many _, None) tuple
        in the end, since this means all objects have the same single value for the
        property.

        A generic implementation of the algorithm is done in the function mark,
        which parameterizes over the function computing the property for an object and
        over the function comparing two values of the property. The mark function
        is specialized for each property by the markColour, markArea, markBasicShape
        and markShape functions respectively, each specializing the two functions which
        are parameters to mark.

        To track all the 4 properties in one traversal of the list, we actually
        start with a tuple of 4 tuples (one for each property) and fold the function
        splitMark over the list, which applies the proper mark function to each
        of the 4 elements of the tuple. This is done in the oddOneOut_ function.

        After the algorithm is run, the interpret function is used to interpret the
        results in the four tuples and turn them into the expected (ID, String) result.
-}

{- Data type for tracking the values of properties. -}
data Group a = None | One a ID | Many a | Bust
             deriving (Eq, Show)

{- Type synonyms for the tuples that are used. -}
type GC  = (Group Colour, Group Colour)
type GA  = (Group SizeType, Group SizeType)
type GBS = (Group Int, Group Int)
type GS  = (Group [SizeType], Group [SizeType])

{- Computes and returns the odd one out according to specifications. -}
oddOneOut :: [Object] -> (ID, String)
oddOneOut os
  | length os < 3       = (-1, "Need at least 3 objects for odd one out to make sense")
  | otherwise           = interpret (oddOneOut_ os)

{-
        Interprets the results of oddOneOut_ according to the description of
        the algorithm given above.
-}
interpret :: (GC, GA, GBS, GS) -> (ID, String)
interpret (x, y, z, t) = case check x of
                           Just i  -> (i, "its colour is different")
                           Nothing -> case check y of
                                        Just i  -> (i, "its area is different")
                                        Nothing -> case check z of
                                                     Just i  -> (i, "its basic shape is different")
                                                     Nothing -> case check t of
                                                                  Just i  -> (i, "its shape is different")
                                                                  Nothing -> (-1, "no odd one out found")

{-
        Checks a single tuple for a odd one out case. Returns Just id with
        the id of the odd one out object in there is such an object or Nothing
        otherwise.
-}
check :: (Group a, Group a) -> Maybe ID
check (One _ i, Many _) = Just i
check (Many _, One _ i) = Just i
check (_, _)            = Nothing

{-
        This is the actual implementation of odd one out, while oddOneOut is simply
        a wrapper around it. The algorithm applied here traverses the list only once,
        using a foldl and when the traversal is done, it returns the encoded result.
        The principle of the algorithm is described above.
-}
oddOneOut_ :: [Object] -> (GC, GA, GBS, GS)
oddOneOut_ = foldl splitMark ((None, None), (None, None), (None, None), (None, None))

{- Applies the proper mark function to each element in the tuple. -}
splitMark :: (GC, GA, GBS, GS) -> Object -> (GC, GA, GBS, GS)
splitMark (x, y, z, t) o = (markColour x o, markArea y o, markBasicShape z o, markShape t o)

{- Specialization of mark for the colour property. -}
markColour :: GC -> Object -> GC
markColour = mark colourO (==)

{- Specialization of mark for the area property. -}
markArea :: GA -> Object -> GA
markArea = mark areaO eq4SF

{- Compares x1 and x2 at 4 significant figures. -}
eq4SF x1 x2 = sigFig (abs (x2 - x1)) 4 == 0.0

{- Specialization of mark for the basic shape property. -}
markBasicShape :: GBS -> Object -> GBS
markBasicShape = mark sidesO (==)

{- Specialization of mark for the shape property. -}
markShape :: GS -> Object -> GS
markShape = mark sideRatio eqShape

{-
        Divides all the elements of a list by element which is the minimum in the
        list. This way we compute a normlaized ratio of all of the sides of a polygon
        (or of the radii of an ellipse). These normalized ratios we then compare
        using the eqShape function.
-}
sideRatio :: Object -> [SizeType]
sideRatio o = map (/denom) sl
  where sl    = sideListO o
        denom = minimum sl

{-
        Equality of shapes. Two shapes are considered equal if:
         (1) they have the same number of sides;
         (2) when comparing side ratios (which is what we do here)
             we can find a circular shift of the second list which
             is equal to the unshifted first list (to accomodate for
             the fact that polygons can be specified starting at any
             one of their vertices. Also, to accomodate for the case
             where one polygon is specified in a clockwise manner, while
             another in a counterclockwise manner, we also try the
             equality between the unshifted first list and any of
             the circular shifts of the _reversed_ second list.

        Property (1) is tested in eqShape, while property (2) is tested
        in eqShape_.

        Both input lists should contain side ratio computed by the
        sideRatio function.

        In the implementation, we take advantage of Haskell's lazy
        evaluation (in computing the circular shifts) and of the way
        the "and" and "or" functions work. Specifically, the "and"
        that is used in the comparison of two lists makes sure that
        as soon as a pair of elements differ (i.e., their equality
        yields False), the comparison stops. Also, the "or" that is
        used to check for a match stops the comparison as soon as a
        match succeeds (i.e., the result of comparing two lists is
        True). The same holds for the "or" operator (||), which only
        evaluates the second argument if the first one is False.

        An implicit assumption is made that all polygons have at
        least 3 sides. In this way, whenever we have two lists with
        two elements each to compare, we can safely assume we are dealing
        with ellipses (for which we actually consider the 2 radii for
        comparison; see function sideListO for this).
-}
eqShape :: [SizeType] -> [SizeType] -> Bool
eqShape xs ys = (length xs == length ys) && eqShape_ xs ys

eqShape_ :: [SizeType] -> [SizeType] -> Bool
eqShape_ xs ys = or (zipWith (\x y -> and (zipWith eq4SF x y)) (repeat xs) (circularShift ys))
              || or (zipWith (\x y -> and (zipWith eq4SF x y)) (repeat xs) (circularShift (reverse ys)))

{-
        Returns a list with all the lists which represent circular shifts
        of the input list (including the original list). For example,

                circularShift [1,2,3] = [[1,2,3],[2,3,1],[3,1,2]]
-}
circularShift :: [a] -> [[a]]
circularShift xs = take l (circShift (cycle xs) l)
  where l                  = length xs
        circShift (x:xs) n = take n (x:xs) : circShift xs n

{- The function that implements the algorithm described above in a generic way. -}
mark :: (Object -> a) -> (a -> a -> Bool) -> (Group a, Group a) -> Object -> (Group a, Group a)
mark prop eq (None, None) o
                        = (One (prop o) (idO o), None)
mark prop eq (One c id1, None) o
  | co `eq` c           = (Many c, None)
  | otherwise           = (One c id1, One co (idO o))
  where co = prop o
mark prop eq (Many c, None) o
  | co `eq` c           = (Many c, None)
  | otherwise           = (Many c, One co (idO o))
  where co = prop o
mark prop eq (One c1 id1, One c2 id2) o
  | c1 `eq` co          = (Many c1, One c2 id2)
  | c2 `eq` co          = (One c1 id1, Many c2)
  | otherwise           = (Bust, Bust)
  where co = prop o
mark prop eq (Many c1, One c2 id2) o
  | c1 `eq` co          = (Many c1, One c2 id2)
  | otherwise           = (Bust, Bust)
  where co = prop o
mark prop eq (One c1 id1, Many c2) o
  | c2 `eq` co          = (One c1 id1, Many c2)
  | otherwise           = (Bust, Bust)
  where co = prop o
mark _ _ (Bust, Bust) o = (Bust, Bust)
mark _ _ (_, _) _       = error "Something is wrong with the odd one out algorithm!"
