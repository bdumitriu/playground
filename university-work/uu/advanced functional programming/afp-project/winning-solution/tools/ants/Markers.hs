module Markers(makeMarker,rewriteMarker,readMarker,readMarkerSomeWhere,hasMarker,hasMarkerSomeWhere,MarkerType(..)) where

import Prelude hiding (Left,Right)
import Library

import Marks

data MarkerType = HomeMarker | FoodMarker

makeMarker :: MarkerType -- which mark to set
	   -> Int       -- value to write (raw value, *not* direction)
	   -> Fragment
makeMarker marker val 
    = let (zero,one,two) = intMarkToBoolMark val
          base = markerTypeToDirectionBase marker
      in
         (if two then cmdMark (base+2) else cmdUnmark (base+2)) .*.
         (if one then cmdMark (base+1) else cmdUnmark (base+1)) .*.
         (if zero then cmdMark base else cmdUnmark base)

rewriteMarker :: MarkerType
	      -> Int   -- old marker
	      -> Int   -- new marker
	      -> Fragment

rewriteMarker marker oldval newval
    = let (zeroold,oneold,twoold) = intMarkToBoolMark oldval
	  (zeronew,onenew,twonew) = intMarkToBoolMark newval
          base = markerTypeToDirectionBase marker
      in remark (base+2) twoold twonew .*.
	 remark (base+1) oneold onenew .*.
	 remark base     zeroold zeronew
             where remark pos old new =
                      if old==new then skip else 
                         if new then cmdMark pos else cmdUnmark pos

readMarkerSomeWhere
           :: MarkerType                -- which mark to read
           -> SenseDir                  -- where to read it
	   -> (Int -> Fragment)  -- fragment to run afterwards, based
                                 -- on raw value (*not* direction) in field
	   -> Fragment

readMarkerSomeWhere marker sensedir binis
 = fHR2
   where fHR2   = cmdSense sensedir (Marker (base+2)) (fHR1 4) .*. (fHR1 0)
         fHR1 n = cmdSense sensedir (Marker (base+1)) (fHR0 (n+2)) .*. (fHR0 n)
         fHR0 n = cmdSense sensedir (Marker (base+0)) (binis (n+1)) .*. (binis n)
	 base = markerTypeToDirectionBase marker

-- special case
readMarker marker = readMarkerSomeWhere marker Here


hasMarkerSomeWhere ::
         MarkerType
      -> SenseDir                  -- where to read it
	  -> Fragment    -- run this if there is a marker
	  -> Fragment

hasMarkerSomeWhere marker sensedir isthere
 = let base = markerTypeToDirectionBase marker
   in 
      cmdSense sensedir (Marker (base+1)) isthere .*.
      cmdSense sensedir (Marker (base+2)) isthere .*.
      cmdSense sensedir (Marker base)     isthere .*.
      skip

hasMarker :: MarkerType
	  -> Fragment    -- run this if there is a marker
	  -> Fragment

hasMarker marker isthere = hasMarkerSomeWhere marker Here isthere

-- base (0 or 3 normally)
markerTypeToDirectionBase :: MarkerType -> Int
markerTypeToDirectionBase HomeMarker = 0
markerTypeToDirectionBase FoodMarker = 3
