package life

import swing.{MainFrame, SimpleSwingApplication}
import ui.UniverseUI

/**
 *
 * @author Bogdan Dumitriu
 */

object GameOfLife extends SimpleSwingApplication {

  def top = new MainFrame {
    title = "Game of Life"
    contents = new UniverseUI(new Universe)
    size = new swing.Dimension(1024, 768)
    maximize()
    visible = true
  }
}
