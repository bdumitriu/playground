package life

/**
 *
 * @author Bogdan Dumitriu
 */
class Universe {

  val WIDTH = 1024
  val HEIGHT = 768

  val cells: Array[Array[Boolean]] = Array.tabulate(WIDTH, HEIGHT) ((i, j) => (i + j) % 2 == 0)
}
