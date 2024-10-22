package Hex
import Hex.projeto.{hasContiguousLine}
import Hex.projetoUtilisTUI.endTui
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

import java.time.LocalTime
import projetoUtilis.makeEmptyBoard

import scala.util.control.Breaks.break

class Main extends App {
  def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Hex Game")
    val fxmlLoader=
        new FXMLLoader(getClass.getResource("Hex/Controller.fxmller.fxml"))
        val mainViewRoot: Parent = fxmlLoader.load()
        val scene = new Scene(mainViewRoot)
        primaryStage.setScene(scene)
        primaryStage.show()
  }
}


object FxApp {

  val size = 5
  val rand: MyRandom = MyRandom(LocalTime.now().toSecondOfDay())

  var game = projeto(makeEmptyBoard(5), rand, size, false, Cells.Red)
  var gameAux = projeto(makeEmptyBoard(5), rand, size, false, Cells.Red)

  def main(args: Array[String]): Unit = {

    // Application.launch(classOf[Main], args: _*)

      while(true){

      game = game.alterar(game.gui, game.player)

      }


  }
}

