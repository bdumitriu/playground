package org.ffplanner

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.ffplanner.ConflictGraph.Node
import org.ffplanner.TestFestivalProgrammes.tiff2012Programme
import org.ffplanner.TestScheduleConstraints._

/** Unit tests for [[org.ffplanner.ConflictGraph SchedulingGraph]].
  *
  * @author Bogdan Dumitriu
  */
@RunWith(classOf[JUnitRunner])
class ConflictGraphTests extends FunSuite with ShouldMatchers {

  trait TestFixture1 {
    val graph: ConflictGraph = new ConflictGraph(tiff2012Programme)
    graph.initializeWith(tiff2012AllMovieConstraints)
  }

  trait TestFixture2 {
    val graph: ConflictGraph = new ConflictGraph(tiff2012Programme)
    graph.initializeWith(scheduleConstraints1)
  }

  trait TestFixture3 {
    val graph: ConflictGraph = new ConflictGraph(tiff2012Programme)
    graph.initializeWith(scheduleConstraints2)
  }

  test("graph construction") {
    new TestFixture1 {
      graph.neighboursOf(1000) should be ('empty)
      graph.neighboursOf(36) should be (nodeSetOf(45, 46, 52))
      graph.neighboursOf(46) should be (nodeSetOf(36, 37))
      graph.neighboursOf(47) should be (nodeSetOf(38, 53, 60, 67, 42))
    }
  }

  test("graph update 1") {
    new TestFixture1 {
      List(47, 38, 53, 60, 67, 42, 227, 199, 209, 219, 223, 231, 202, 203, 238).foreach { showingId =>
        assert(graph.hasShowing(showingId), "Showing "+showingId+" should be in the conflict graph.")
      }
      graph.neighboursOf(37) should be (nodeSetOf(46, 53, 60))
      List(199, 209, 219, 223, 231, 202, 203, 238).foreach { showingId =>
        assert(
          graph.neighboursOf(showingId).contains(nodeOf(227)), "227 should be among the neighbours of "+showingId+".")
      }
      graph.neighboursOf(224) should contain (nodeOf(203))
      graph.neighboursOf(230) should contain (nodeOf(202))

      graph.updateWith(tiff2012Programme.getShowing(47))

      graph.neighboursOf(37) should be (nodeSetOf(46))
      List(47, 38, 53, 60, 67, 42, 227).foreach { showingId =>
        assert(!graph.hasShowing(showingId), "Showing "+showingId+" should no longer be in the conflict graph.")
      }
      List(199, 209, 219, 223, 231, 202, 203, 238).foreach { showingId =>
        assert(graph.hasShowing(showingId), "Showing "+showingId+" should still be in the conflict graph.")
        assert(
          !graph.neighboursOf(showingId).contains(nodeOf(227)), "227 should no longer be a neighbour of "+showingId+".")
      }
    }
  }

  test("graph update 2") {
    new TestFixture3 {
      graph.hasShowing(23) should be (true)
      graph.neighboursOf(23) should be (nodeSetOf(19, 2))
      graph.neighboursOf(2) should be (nodeSetOf(8, 23))
      graph.neighboursOf(19) should be (nodeSetOf(23))

      graph.hasShowing(248) should be (true)
      graph.neighboursOf(248) should be (nodeSetOf(244, 262))
      graph.neighboursOf(244) should be (nodeSetOf(248, 262))
      graph.neighboursOf(262) should be (nodeSetOf(244, 248))

      graph.updateWith(248)

      graph.hasShowing(23) should be (false)
      graph.neighboursOf(2) should be (nodeSetOf(8))
      graph.neighboursOf(19) should be ('empty)

      graph.hasShowing(248) should be (false)
      graph.hasShowing(244) should be (false)
      graph.hasShowing(262) should be (false)
    }
  }

  test("restricted graph construction") {
    new TestFixture2 {
      graph.neighboursOf(1) should be (nodeSetOf(14))
      graph.neighboursOf(5) should be ('empty)
      graph.neighboursOf(8) should be (nodeSetOf(14))
      graph.neighboursOf(14) should be (nodeSetOf(1, 8))
      graph.neighboursOf(16) should be ('empty)
      graph.hasShowing(96) should be (false)
      graph.hasShowing(100) should be (false)
      graph.neighboursOf(137) should be ('empty)
      graph.neighboursOf(181) should be ('empty)
      graph.neighboursOf(231) should be ('empty)
      graph.neighboursOf(286) should be ('empty)
      graph.neighboursOf(344) should be ('empty)
    }
  }

  test("isolated nodes") {
    new TestFixture2 {
      graph.getIsolatedNodes should be (nodeSetOf(5, 16, 137, 181, 231, 286, 344))
      graph.updateWith(1)
      graph.getIsolatedNodes should be (nodeSetOf(5, 8, 16, 137, 181, 231, 286, 344))
    }
  }

  test("order of isolated nodes") {
    new TestFixture2 {
      graph.getIsolatedNodes.toList should be (nodeListOf(16, 5, 137, 181, 231, 286, 344))
    }
  }

  test("movie priorities are not lost") {
    def testPriority1(graph: ConflictGraph, neighbourId: Long, priority1Id: Long) {
      graph.neighboursOf(neighbourId).filter(_.priority == 1) should have size (1)
      val priority1Node: Option[Node] = graph.neighboursOf(neighbourId).find(_.showingId == priority1Id)
      assert(priority1Node.isDefined)
      priority1Node.get.priority should be (1)
    }

    val graph: ConflictGraph = new ConflictGraph(tiff2012Programme)
    val priority2Constraint: MovieConstraint = tiff2012AllMovieConstraints.movieConstraints.find(_.movie.id == 19).get
    val priority1Constraint: MovieConstraint = new MovieConstraint(priority2Constraint.movie, 1: Short)
    val scheduleConstraints = new ScheduleConstraints(
      tiff2012AllMovieConstraints.movieConstraints - priority2Constraint + priority1Constraint,
      tiff2012AllMovieConstraints.showingConstraints)
    graph.initializeWith(scheduleConstraints)
    testPriority1(graph, 15, 19)
    testPriority1(graph, 145, 138)
    testPriority1(graph, 362, 357)
  }

  def nodeSetOf(showingIds: Long*): Set[Node] = Set(showingIds: _*).map(nodeOf)

  def nodeListOf(showingIds: Long*): List[Node] = List(showingIds: _*).map(nodeOf)

  def nodeOf(showingId: Long): Node =
    new Node(new ShowingConstraint(tiff2012Programme.getShowing(showingId), Utils.DefaultPriority))
}
