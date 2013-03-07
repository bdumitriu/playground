import java.util
import org.ffplanner.`def`.ConstraintDefinition.WatchType
import org.ffplanner.`def`.{ConstraintDefinition, ShowingDefinition, ScheduleDefinition, FestivalProgrammeDefinition}
import org.ffplanner.{Schedule, Movie, Showing, ScheduleBuilder}
import org.joda.time.DateTime
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 *
 *
 * @author Bogdan Dumitriu
 */
@RunWith(classOf[JUnitRunner])
class ScheduleTests extends FunSuite {

  trait TestScheduleDefinitions {
    val festivalProgramme: FestivalProgrammeDefinition = new FestivalProgrammeDefinition {
      def getShowings: util.List[ShowingDefinition] = {
        val showings: util.LinkedList[ShowingDefinition] = new util.LinkedList[ShowingDefinition]()
        showings.add(new Showing(15, DateTime.now, new Movie(10)))
        showings.add(new Showing(25, DateTime.now, new Movie(10)))
        showings
      }
    }
    val scheduleDefinition1: ScheduleDefinition = new TestScheduleDefinition(Map.empty)
    val scheduleDefinition2: ScheduleDefinition = new TestScheduleDefinition(Map[java.lang.Long, ConstraintDefinition](
      long2Long(25) -> new TestConstraint(WatchType.MOVIE, 2)
    ))
  }

  test("no movies") {
    new TestScheduleDefinitions {
      assert(new ScheduleBuilder(festivalProgramme).getPossibleSchedules(scheduleDefinition1)(0).showings.size === 0)
    }
  }

  test("one definitely movie") {
    new TestScheduleDefinitions {
      val schedules: List[Schedule] = new ScheduleBuilder(festivalProgramme).getPossibleSchedules(scheduleDefinition2)
      assert(schedules.size === 1)
      assert(schedules(0).showings(0) === 15)
    }
  }
}
