import Main.{HEIGHT, PARTICULE_SIZE, WIDTH}
import Direction.*
import scalafx.scene.paint.Color.{Green, Red}
import scalafx.scene.shape.Circle

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class State(canva : ArrayBuffer[ArrayBuffer[Boolean]], allParticules: List[Particule]) {
  val numberOfDirection = Direction.values.length

  def newState = {
    val newAllParticule = allParticules.map(p =>
      val neighbours = neighboursOf(p.coordinate.x, p.coordinate.y, PARTICULE_SIZE)
      if (neighbours.exists(c => canva(c.x)(c.y))) {
        p.copy(direction = Direction.values(Random.nextInt(numberOfDirection)))
      } else {
        p.copy(getPositions(p.direction, p.coordinate.x, p.coordinate.y))
      }
    )

    for (i <- 0 until WIDTH; j <- 0 until HEIGHT) {
      canva(i)(j) = false
    }

    fillCanva(newAllParticule)
    State(canva, newAllParticule)
  }

  def draw(): List[Circle] = {
    val particuleCircle: List[Circle] = {
      allParticules.map { cell =>
        cell.draw
      }
    }
    particuleCircle
  }

  def fillCanva(newAllParticule : List[Particule]): Unit = {
    newAllParticule.foreach(p =>
      canva(p.coordinate.x)(p.coordinate.y) = true
    )
  }

  def getPositions(direction: Direction, x: Int, y: Int) = {
    val (xMin, xMax, yMin, yMax) = (0, WIDTH - 1, 0, HEIGHT - 1)
    (direction, x, y) match {
      case (NORTH, _, `yMin`) => Coordinate(x, yMax) // tout en haut => tout en bas
      case (SOUTH, _, `yMax`) => Coordinate(x, yMin) // tout en bas => tout en haut
      case (EAST, `xMax`, _) => Coordinate(xMin, y) // tout à droite => tout à gauche
      case (WEST, `xMin`, _) => Coordinate(xMax, y) // tout à gauche => tout à droite
      case (NORTH_EAST, `xMax`, `yMin`) => Coordinate(xMin, yMax) // tout en haut à droite => tout en bas à gauche
      case (NORTH_WEST, `xMin`, `yMin`) => Coordinate(xMax, yMax) // tout en haut à gauche => tout en bas à droite
      case (SOUTH_EAST, `xMax`, `yMax`) => Coordinate(xMin, yMin) // tout en bas à droite => tout en haut à gauche
      case (SOUTH_WEST, `xMin`, `yMax`) => Coordinate(xMax, yMin) // tout en bas à gauche => tout en haut à droite

      case (NORTH_EAST, `xMax`, _) => Coordinate(xMin, y)
      case (NORTH_EAST, _, `yMin`) => Coordinate(x, yMax)

      case (NORTH_WEST, `xMin`, _) => Coordinate(xMax, y)
      case (NORTH_WEST, _, `yMin`) => Coordinate(x, yMax)

      case (SOUTH_EAST, `xMax`, _) => Coordinate(xMin, y)
      case (SOUTH_EAST, _, `yMax`) => Coordinate(x, yMin)

      case (SOUTH_WEST, `xMin`, _) => Coordinate(xMax, y)
      case (SOUTH_WEST, _, `yMax`) => Coordinate(x, yMin)

      case (NORTH, _, _) => Coordinate(x, y - 1)
      case (SOUTH, _, _) => Coordinate(x, y + 1)
      case (EAST, _, _) => Coordinate(x + 1, y)
      case (WEST, _, _) => Coordinate(x - 1, y)
      case (NORTH_EAST, _, _) => Coordinate(x + 1, y - 1)
      case (NORTH_WEST, _, _) => Coordinate(x - 1, y - 1)
      case (SOUTH_EAST, _, _) => Coordinate(x + 1, y + 1)
      case (SOUTH_WEST, _, _) => Coordinate(x - 1, y + 1)
    }
  }

  def neighboursOf(x: Int, y: Int, distance: Int): Seq[Coordinate] =
    for {
      i <- -distance to distance
      j <- -distance to distance if (i, j) != (0, 0)
    } yield Coordinate(x + i, y + j)
}