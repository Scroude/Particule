import javafx.scene.input.KeyCode
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.Circle

import java.awt.Toolkit
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends JFXApp3 {

  val PARTICULE_SIZE = 1
  val NUMBER_OF_PARTICULE = 500
  val WIDTH = 800
  val HEIGHT = 800

  val allParticules: List[Particule] = generateParticules(NUMBER_OF_PARTICULE)
  val canva: ArrayBuffer[ArrayBuffer[Boolean]] = ArrayBuffer.fill(WIDTH, HEIGHT)(false)


  def generateParticules(n: Int): List[Particule] = List.fill(n)(0).map(_ =>
    val x = Random.nextInt(WIDTH)
    val y = Random.nextInt(HEIGHT)
    val numberOfDirection = Direction.values.length
    val direction = Direction.values(Random.nextInt(numberOfDirection))
    val color = Color.rgb(Random.nextInt(256), Random.nextInt(256), Random.nextInt(256), 1)

    Particule(Coordinate(x, y), direction, color)
  )

  def gameLoop(update: () => Unit): Unit =
    def tick = Future {update(); Thread.sleep(50)}
    def loopAgain = Future(gameLoop(update))
    for {
      _ <- tick
      _ <- loopAgain
    } yield ()

  override def start(): Unit = {
    val state = ObjectProperty(State(canva, allParticules))
    val frame = IntegerProperty(0) //boucle

    frame.onChange {
      state.update(state.value.newState)
    }

    stage = new PrimaryStage {
      title = "Particules"
      width = WIDTH
      height = HEIGHT
      scene = new Scene {
        fill = Black
        content = allParticules.map(p =>
          p.draw
        )

        frame.onChange(Platform.runLater {
          content = state.value.draw()
        })
      }
    }
    gameLoop(() => frame.update(frame.value + 1))
  }
}
