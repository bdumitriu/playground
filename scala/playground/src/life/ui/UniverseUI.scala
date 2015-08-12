package life.ui

import swing._
import java.awt.Color
import life.Universe

/**
 *
 * @author Bogdan Dumitriu
 */

class UniverseUI(universe: Universe) extends Component {

  protected override def paintComponent(g: Graphics2D) {
    g.setColor(Color.GREEN)
    for (i <- 0 until universe.cells.size)
      for (j <- 0 until universe.cells(i).size)
        if (universe.cells(i)(j))
          g.drawLine(i, j, i, j)
  }
}
