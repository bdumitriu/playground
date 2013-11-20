package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val prevalenceRate: Double = 0.01
    val transmissibilityRate: Double = 0.4
    val maxDaysBeforeMove: Int = 5
    val deathRate: Double = 0.25
    val airTrafficRate: Double = 0.01
    val reducedMobilityActInForce: Boolean = false
    val chosenFewActInForce: Boolean = false
    val vaccinatedRate: Double = 0.05
  }

  import SimConfig._

  private var idsInfectedPeople = Set[Int]()

  private var idsVaccinatedPeople = Set[Int]()

  while (idsInfectedPeople.size != (prevalenceRate * population).toInt) {
    idsInfectedPeople += (randomBelow(population) + 1)
  }

  while (idsVaccinatedPeople.size != (vaccinatedRate * population).toInt) {
    val proposedId = randomBelow(population) + 1
    if (!idsInfectedPeople.contains(proposedId)) {
      idsVaccinatedPeople += proposedId
    }
  }

  val persons: List[Person] = List.tabulate(population) { idx =>
    val person = new Person(idx + 1)
    if (idsInfectedPeople contains person.id) {
      person.infected = true
      afterInfectionActions(person)
    }
    if (chosenFewActInForce && (idsVaccinatedPeople contains person.id)) {
      person.immune = true
    }
    person
  }

  persons foreach moveAction

  def moveAction(person: Person) {
    afterDelay(randomBelow(getMaxDaysBeforeMove(person)) + 1) {
      if (!person.dead) {
        if (random <= airTrafficRate) {
          person.changeRoomByAirTraffic()
        } else {
          person.changeRoomRandomly()
        }
        if (infectedPeopleIn(person.getRoom)) {
          if (person.maybeInfect()) {
            afterInfectionActions(person)
          }
        }
        moveAction(person)
      }
    }
  }

  def getMaxDaysBeforeMove(person: Person) = {
    if (reducedMobilityActInForce) {
      if (person.isVisiblySick) {
        maxDaysBeforeMove * 4
      } else {
        maxDaysBeforeMove * 2
      }
    } else {
      maxDaysBeforeMove
    }
  }

  def afterInfectionActions(person: Person) {
    sickAction(person)
    dieAction(person)
    immuneAction(person)
    healthyAction(person)
  }

  def sickAction(person: Person) {
    afterDelay(6) {
      person.sick = true
    }
  }

  def dieAction(person: Person) {
    afterDelay(14) {
      person.dead = random <= deathRate
    }
  }

  def immuneAction(person: Person) {
    afterDelay(16) {
      if (!person.dead) {
        person.sick = false
        person.immune = true
      }
    }
  }

  def healthyAction(person: Person) {
    afterDelay(18) {
      if (!person.dead) {
        person.infected = false
        person.immune = false
      }
    }
  }

  def infectedPeopleIn(room: Room) = {
    persons exists { person => person.isIn(room) && person.infected }
  }

  def noVisiblySickPeopleIn(room: Room) = {
    !(persons exists { person => person.isIn(room) && person.isVisiblySick })
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def changeRoomRandomly() = {
      val candidateRooms = getCandidateRooms
      if (candidateRooms.length > 0) {
        changeRoomTo(candidateRooms(randomBelow(candidateRooms.length)))
      }
    }

    def changeRoomByAirTraffic() = {
      changeRoomTo(getRandomRoom)
    }

    private def changeRoomTo(room: Room) = {
      assert(room.row >= 0 && row < roomRows, s"assertion failed: unexpected row: ${room.row}")
      assert(room.col >= 0 && room.col < roomColumns, s"assertion failed: unexpected column: ${room.col}")
      row = room.row
      col = room.col
    }

    private def getCandidateRooms = {
      val neighbouringRooms = getNeighbouringRooms
      neighbouringRooms filter noVisiblySickPeopleIn
    }

    private def getNeighbouringRooms = {
      Array(
        new Room((row + 1) % roomRows, col),
        new Room(if (row == 0) roomRows - 1 else row - 1, col),
        new Room(row, (col + 1) % roomColumns),
        new Room(row, if (col == 0) roomColumns - 1 else col - 1)
      )
    }

    private def getRandomRoom = {
      def generateRandomRoom = {
        new Room(randomBelow(roomRows), randomBelow(roomColumns))
      }

      def randomRooms: Stream[Room] = generateRandomRoom #:: randomRooms
      val randomRoom = randomRooms find { room => room.row != row || room.col != col }
      randomRoom.get
    }


    def isIn(room: Room) = {
      row == room.row && col == room.col
    }

    def getRoom = new Room(row, col)

    def isVisiblySick = {
      sick || dead
    }

    def maybeInfect() = {
      if (infected || immune) {
        false
      } else {
        infected = random <= transmissibilityRate
        infected
      }
    }
  }

  class Room(val row: Int, val col: Int) {

    def canEqual(other: Any): Boolean = other.isInstanceOf[Room]

    override def equals(other: Any) = other match {
      case that: Room => that.canEqual(this) && this.row == that.row && this.col == that.col
      case _ => false
    }

    override def hashCode = 41 * (41 + this.row) + this.col
  }
}
