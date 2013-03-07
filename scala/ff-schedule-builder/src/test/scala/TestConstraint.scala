import org.ffplanner.`def`.ConstraintDefinition
import org.ffplanner.`def`.ConstraintDefinition.WatchType

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class TestConstraint(val watchType: WatchType, val priority: Short) extends ConstraintDefinition {

  def getWatchType: WatchType = watchType

  def getPriority: java.lang.Short = priority
}
