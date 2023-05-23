import scalafx.scene.shape.Circle
import scalafx.scene.paint.Color
import Main.PARTICULE_SIZE
import Direction.*

case class Particule(coordinate: Coordinate, direction: Direction, color: Color) {

  def draw: Circle = new Circle {
    centerX = coordinate.x
    centerY = coordinate.y
    radius = PARTICULE_SIZE
    fill = color
  }


}