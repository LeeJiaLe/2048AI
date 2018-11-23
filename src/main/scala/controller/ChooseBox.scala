package controller


import javafx.event.{ActionEvent, EventHandler}
import scalafx.geometry.Pos
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color._
import scalafx.scene.text.Font
import scalafx.scene.text.TextAlignment._

// generic class accept any kind of data
class ChooseBox[+T](title:String,selectList:List[ChooseBoxData[T]]) extends VBox {

  //current selected index
  var index:Int=0

  //max index of this choose box
  val maxIndex:Int=selectList.length - 1

  //label at center, which show the text (eg: 10x10)
  val centerLabel:Label = new Label{
    text = selectList(index).text
    textAlignment = Center
    alignment= Pos.Center
    maxHeight=40
    minHeight=40
    maxWidth=300
    minWidth=300
    style = "-fx-border-color: #683a01;"+
            "-fx-border-width: 3;"
    font = new Font(25)

  }

  //two button which change the index
  val btnLeft:Button = new ChooseBoxButtonLeft(this)
  val btnRight:Button = new ChooseBoxButtonRight(this)

  //horizontal box which keep the both button and the label
  val selection:HBox=new HBox(){
    children = List(
      btnLeft,
      centerLabel,
      btnRight
    )
    alignment = Pos.Center
    spacing = 3
  }

  alignment = Pos.Center
  spacing = 3
  children = List(
    new Label(title){
      alignment = Pos.Center
      font = new Font(20)
    },
    selection
  )

  def changeLabelText():Unit={
    centerLabel.text = selectList(index).text
  }

  //return result base on the type provided
  def result:T = selectList(index).value
}


abstract class ChooseBoxButton(chooseBox: ChooseBox[Any])extends Button{
  maxHeight=40
  minHeight=40
  maxWidth=40
  minWidth=40
  textFill = White
  style = "-fx-background-color:#683a01;"
  textAlignment = Center
  font = new Font(21)
}

class ChooseBoxButtonRight(chooseBox: ChooseBox[Any]) extends ChooseBoxButton(chooseBox){
  val btnAction: EventHandler[ActionEvent] = _ =>{
    if(chooseBox.index==chooseBox.maxIndex){
      chooseBox.index=0
    }else {
      chooseBox.index += 1
    }
    chooseBox.changeLabelText()
  }

  onAction = btnAction
  text = "▶"
}

class ChooseBoxButtonLeft(chooseBox: ChooseBox[Any]) extends ChooseBoxButton(chooseBox){
  val btnAction: EventHandler[ActionEvent] = _ =>{
    if(chooseBox.index==0){
      chooseBox.index=chooseBox.maxIndex
    }else {
      chooseBox.index -= 1
    }
    chooseBox.changeLabelText()
  }

  onAction = btnAction

  text = "◀"
}

//each data inside the choose box
case class ChooseBoxData[T](value:T, text:String)


