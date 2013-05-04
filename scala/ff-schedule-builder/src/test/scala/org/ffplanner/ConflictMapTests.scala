package org.ffplanner

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.ffplanner.TestFestivalProgrammes._

/** Unit tests for [[org.ffplanner.ConflictMap ConflictMap]].
  *
  *  @author Bogdan Dumitriu
  */
@RunWith(classOf[JUnitRunner])
class ConflictMapTests extends FunSuite with ShouldMatchers {

  test("no conflicts") {
    val conflictDetector: ConflictMap = new ConflictMap(festivalProgramme1)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(15, Set(), Set())
    showingIds should be ('empty)
  }

  test("no conflict with the same showing of the same movie") {
    val conflictDetector: ConflictMap = new ConflictMap(festivalProgramme1)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(15, Set(10), Set(15))
    showingIds should be ('empty)
  }

  test("no conflict with different showing of the same movie") {
    val conflictDetector: ConflictMap = new ConflictMap(festivalProgramme4)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(15, Set(10), Set(25))
    showingIds should be ('empty)
  }

  test("no conflicts - none in movie scope, showing scope empty") {
    val conflictDetector: ConflictMap = new ConflictMap(festivalProgramme2)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(1002, ((1L to 7) ++ (9L to 12)).toSet, Set())
    showingIds should be ('empty)
  }

  test("no conflicts - movie scope empty, none in showing scope") {
    val conflictDetector: ConflictMap = new ConflictMap(festivalProgramme2)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(
      1002, Set(), ((1001L to 1007) ++ (1009L to 1012)).toSet)
    showingIds should be ('empty)
  }

  test("one conflict - movie scope") {
    val conflictDetector: ConflictMap = new ConflictMap(festivalProgramme2)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(1002, (1L to 12).toSet, Set())
    showingIds should have size (1)
    showingIds should contain (1008L)
  }

  test("one conflict - showing scope") {
    val conflictDetector: ConflictMap = new ConflictMap(festivalProgramme2)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(1002, Set(), (1001L to 1012).toSet)
    showingIds should have size (1)
    showingIds should contain (1008L)
  }

  test("multiple conflicts - movie scope") {
    val conflictDetector: ConflictMap = new ConflictMap(tiff2012Programme)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(15, Set((1L to 198): _*), Set())
    showingIds should be (Set(2L, 3, 9, 19, 23, 30))
  }

  test("multiple conflicts - showing scope") {
    val conflictDetector: ConflictMap = new ConflictMap(tiff2012Programme)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(71, Set(), Set((1L to 392): _*))
    showingIds should be (Set(41L, 44, 50, 56, 58, 59, 63, 66, 69, 70, 72, 73, 75))
  }

  test("multiple conflicts - both scopes") {
    val conflictDetector: ConflictMap = new ConflictMap(tiff2012Programme)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(55, Set((1L to 198): _*), Set((1L to 392): _*))
    showingIds should be (Set(39L, 40, 43, 48, 49, 57, 58, 62, 65, 68, 69, 74))
  }

  test("showing 126 starts exactly at the same time as showings 117 and 137 - conflict expected") {
    val conflictDetector: ConflictMap = new ConflictMap(tiff2012Programme)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(126, Set(), Set(117, 137))
    showingIds should be (Set(117L, 137))
  }

  test("showing 42 starts exactly when showing 53 ends - no conflict expected") {
    val conflictDetector: ConflictMap = new ConflictMap(tiff2012Programme)
    val showingIds: Set[Long] = conflictDetector.conflictingShowingIds(53, Set((1L to 198): _*), Set())
    showingIds should be (Set(37L, 38, 47, 60, 67))
  }

  test("correct conflicts between showings 275, 281, 286, 287 and 295 (bug)") {
    val conflictDetector: ConflictMap = new ConflictMap(tiff2012Programme)
    conflictDetector.conflictingShowingIds(
      275, Set(), Set(275L, 281L, 286L, 287L, 295L)) should be (Set(281L, 286, 287, 295))
    conflictDetector.conflictingShowingIds(
      281, Set(), Set(275L, 281L, 286L, 287L, 295L)) should be (Set(275L, 286, 287, 295))
    conflictDetector.conflictingShowingIds(286, Set(), Set(275L, 281L, 286L, 287L, 295L)) should be (Set(275L, 281))
    conflictDetector.conflictingShowingIds(
      287, Set(), Set(275L, 281L, 286L, 287L, 295L)) should be (Set(275L, 281, 295))
    conflictDetector.conflictingShowingIds(
      295, Set(), Set(275L, 281L, 286L, 287L, 295L)) should be (Set(275L, 281, 287))
  }
}
