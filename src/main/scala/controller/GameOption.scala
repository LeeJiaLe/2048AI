package controller

import scalafx.geometry.Insets
import scalafx.scene.control.Button
import scalafx.scene.layout.VBox
import scalafxml.core.macros.sfxml

@sfxml
class GameOption(
                val optionList1:VBox,
                val optionList2:VBox,
                val btnStartGame:Button,
                val btnBackToMenu:Button
                )
{
  val sizeSelection:ChooseBox[Int] = new ChooseBox[Int](
    "Size:",
    List(
      ChooseBoxData[Int](4,"4x4"),
      ChooseBoxData[Int](5,"5x5"),
      ChooseBoxData[Int](6,"6x6"),
      ChooseBoxData[Int](7,"7x7"),
      ChooseBoxData[Int](8,"8x8"),
      ChooseBoxData[Int](9,"9x9"),
      ChooseBoxData[Int](10,"10x10"),
      ChooseBoxData[Int](11,"11x11"),
      ChooseBoxData[Int](12,"12x12"),
      ChooseBoxData[Int](13,"13x13"),
      ChooseBoxData[Int](14,"14x14"),
      ChooseBoxData[Int](15,"15x15"),
      ChooseBoxData[Int](3,"3x3")
    )
  )

  val gameRestrict:ChooseBox[String] = new ChooseBox[String](
    "Restrict:",
    List(
      ChooseBoxData[String]("off","Off"),
      ChooseBoxData[String]("noUp","No Up Command"),
      ChooseBoxData[String]("noUpLeft","No Up, Left Command"),
    )
  )

  val numberDisplay:ChooseBox[String] = new ChooseBox[String](
    "Number Display:",
    List(
      ChooseBoxData[String]("classic","Classic"),
      ChooseBoxData[String]("simple","Simple"),
      ChooseBoxData[String]("none","None"),
      ChooseBoxData[String]("blind","Blind"),
      ChooseBoxData[String]("ninja","Ninja"),
    )
  )

  val undoSetting:ChooseBox[Boolean] = new ChooseBox[Boolean](
    "Undo:",
    List(
      ChooseBoxData[Boolean](true,"On"),
      ChooseBoxData[Boolean](false,"Off")
    )
  )

  val initRandomNumber:ChooseBox[Double] = new ChooseBox[Double](
    "Initial Random Number:",
    List(
      ChooseBoxData[Double](0,"off"),
      ChooseBoxData[Double](0.1,"10%"),
      ChooseBoxData[Double](0.2,"20%"),
      ChooseBoxData[Double](0.3,"30%"),
      ChooseBoxData[Double](0.5,"50%")
    )
  )

  val randomMaxSelection:ChooseBox[Int] = new ChooseBox[Int](
    "Maximum Random Number:",
    List(
      ChooseBoxData[Int](1,"2"),
      ChooseBoxData[Int](2,"4"),
      ChooseBoxData[Int](3,"8"),
      ChooseBoxData[Int](4,"16"),
      ChooseBoxData[Int](5,"32")
    )
  )
  optionList1.spacing = 20
  optionList1.padding = Insets(20,0,0,0)
  optionList2.spacing = 20
  optionList2.padding = Insets(20,0,0,0)
  optionList1.children=List(
    sizeSelection,
    undoSetting,
    numberDisplay
  )

  optionList2.children=List(
    gameRestrict,
    initRandomNumber,
    randomMaxSelection
  )
  def startGame():Unit={
    val result:GameOptionData = GameOptionData(
      sizeSelection.result,
      undoSetting.result,
      numberDisplay.result,
      gameRestrict.result,
      initRandomNumber.result,
      randomMaxSelection.result
    )

    MainApp.getGameScene(result)
  }

  def backToMenu(): Unit ={
    MainApp.setScene("MainPage","2^n")
  }
}
