import streams._
import scala.collection.script.Start

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

object foo {

  val x = new Level1() {
    def getN() = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet
  }                                               //> x  : java.lang.Object with Level1{def getN(): scala.collection.immutable.Se
                                                  //| t[(this.Block, List[this.Move])]} = foo$$anonfun$main$1$$anon$1@9c0287

  x.pathsFromStart.take(1)                        //> res0: scala.collection.immutable.Stream[(foo.x.Block, List[foo.x.Move])] = 
                                                  //| Stream((Block(Pos(1,1),Pos(1,1)),List()), ?)
}