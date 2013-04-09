package org.ffplanner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.ffplanner.ConflictGraph.Node

/** Unit tests for [[org.ffplanner.ConflictGraph SchedulingGraph]].
  *
  * @author Bogdan Dumitriu
  */
@RunWith(classOf[JUnitRunner])
class ConflictGraphTests extends FunSuite with ShouldMatchers with TestFestivalProgrammes with TestScheduleConstraints {

  trait TestFixture1 {
    val graph: ConflictGraph = new ConflictGraph(tiff2012Programme)
    graph.initializeWith(new ScheduleConstraints(Set((1L to 198).map(new MovieConstraint(_, 2)): _*), Set.empty))
  }

  trait TestFixture2 {
    val graph: ConflictGraph = new ConflictGraph(tiff2012Programme)
    graph.initializeWith(scheduleConstraints1)
  }

  test("graph construction") {
    new TestFixture1 {
      graph.neighboursOf(1000) should be ('empty)
      graph.neighboursOf(36) should be (buildNodeSet(45, 46, 52))
      graph.neighboursOf(46) should be (buildNodeSet(36, 37))
      graph.neighboursOf(47) should be (buildNodeSet(38, 53, 60, 67, 42))
    }
  }

  test("update on showing scheduled") {
    new TestFixture1 {
      List(47, 38, 53, 60, 67, 42, 227, 199, 209, 219, 223, 231, 202, 203, 238).foreach { showingId =>
        assert(graph.hasShowing(showingId), "Showing "+showingId+" should be in the conflict graph.")
      }
      graph.neighboursOf(37) should be (buildNodeSet(46, 53, 60))
      graph.neighboursOf(224) should contain (new Node(203))
      graph.neighboursOf(230) should contain (new Node(202))

      graph.updateWith(tiff2012Programme.getShowing(47))

      List(47, 38, 53, 60, 67, 42, 227, 199, 209, 219, 223, 231, 202, 203, 238).foreach { showingId =>
        assert(!graph.hasShowing(showingId), "Showing "+showingId+" should no longer be in the conflict graph.")
      }
      graph.neighboursOf(37) should be (buildNodeSet(46))
      graph.neighboursOf(224) should not contain (new Node(203))
      graph.neighboursOf(230) should not contain (new Node(202))
    }
  }

  test("restricted graph construction") {
    new TestFixture2 {
      graph.neighboursOf(1) should be (buildNodeSet(14))
      graph.neighboursOf(5) should be ('empty)
      graph.neighboursOf(8) should be (buildNodeSet(14))
      graph.neighboursOf(14) should be (buildNodeSet(1, 8))
      graph.neighboursOf(16) should be ('empty)
      graph.neighboursOf(137) should be ('empty)
      graph.neighboursOf(181) should be ('empty)
      graph.neighboursOf(231) should be ('empty)
      graph.neighboursOf(286) should be ('empty)
      graph.neighboursOf(344) should be ('empty)
    }
  }

  test("isolated nodes") {
    new TestFixture2 {
      graph.getIsolatedNodes should be (buildNodeSet(5, 16, 137, 181, 231, 286, 344))
      graph.updateWith(1)
      graph.getIsolatedNodes should be (buildNodeSet(5, 8, 16, 137, 181, 231, 286, 344))
    }
  }

  def buildNodeSet(showingIds: Long*): Set[Node] = {
    Set(showingIds: _*).map(new Node(_))
  }
}
