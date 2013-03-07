import java.lang.Long
import org.ffplanner.`def`.{ConstraintDefinition, ScheduleDefinition}
import scala.collection.JavaConversions.asJavaCollection

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class TestScheduleDefinition(val showings: Map[Long, ConstraintDefinition]) extends ScheduleDefinition {

  def getShowingIds: java.util.Collection[Long] = showings.keys

  def getConstraint(showingId: Long): ConstraintDefinition = showings(showingId)
}
