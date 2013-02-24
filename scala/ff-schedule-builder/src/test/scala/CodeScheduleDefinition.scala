import java.lang.Long
import org.ffplanner.`def`.ScheduleDefinition
import ScheduleDefinition.WatchType
import scala.collection.JavaConversions.seqAsJavaList

/**
 *
 *
 * @author Bogdan Dumitriu
 */
class CodeScheduleDefinition(val showings: List[Long], val movies: List[Long], val maybeMovies: List[Long])
  extends ScheduleDefinition {

  def getShowingIds(watchType: WatchType): java.util.List[Long] = watchType match {
    case WatchType.SHOWING => showings
    case WatchType.MOVIE => movies
    case WatchType.MAYBE_MOVIE => maybeMovies
  }
}
