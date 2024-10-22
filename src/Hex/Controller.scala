package Hex
import javafx.fxml.FXML
import javafx.scene.control.{Button, TextField}

class Controller {

  @FXML
  private var button00: Button = _
  @FXML
  private var textField1: TextField = _


  def button00onClick(): Unit = {

   // FxApp.game=projeto(play(FxApp.game.board),

    button00.setStyle("-fx-background-color: red;")




  }



}