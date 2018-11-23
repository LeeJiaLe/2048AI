package controller

import javafx.stage.Stage
import scalafxml.core.macros.sfxml


@sfxml
class DialogBox extends DialogBoxTrait{

  //function call when user click on ok or close button
  var setResult:Boolean => Unit = _
  var stage:Stage = _


  def okAction(): Unit ={
    setResult(true)
    stage.close()
  }

  def cancelAction(): Unit ={
    setResult(false)
    stage.close()
  }
}


trait DialogBoxTrait{
  var setResult:Boolean => Unit
  var stage:Stage
}
