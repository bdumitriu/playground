package week3

import scala.util.{Try, Success, Failure}

trait AdventureGame {

  trait Adventure {

    def collectCoins(): Try[List[Coin]]
    def buyTreasure(coins: List[Coin]): Try[Treasure]
  }

  object Adventure extends Adventure {

    override def collectCoins(): Try[List[Coin]] = ???

    override def buyTreasure(coins: List[Coin]): Try[Treasure] = ???

    def apply() = this
  }

  class Coin(value: Int)

  class Treasure(treasureCost: Int)

  val adventure = Adventure()

  for {
    coins <- adventure.collectCoins()
    treasure <- adventure.buyTreasure(coins)
  } yield treasure
}
