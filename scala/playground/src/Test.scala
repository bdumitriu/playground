/**
 * 
 * @author Bogdan Dumitriu
 */

object Test extends App {

  val gs = new Array[String](3)
  gs(0) = "foo"
  gs(1) = "bar"
  gs(2) = "baz"
  for (i <- 0 to 2)
    print(gs(i)+" ")
}
