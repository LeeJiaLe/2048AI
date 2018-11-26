package controller

import scalafx.scene.control.Label
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.BorderPane
import scalafx.geometry.Pos
import scalafxml.core.macros.sfxml

@sfxml
class Game(
          val mainLayout:BorderPane,
          val lblScore:Label,
          val lblMove:Label
          ) extends GameTrait {
  var gameArea:GameArea = _

  var keyValue:String = ""

  //init the game area
  def setGameArea(option: GameOptionData): Unit = {
    gameArea = new GameArea(option,1){


      //set the label reference to the object
      lblMoveCount = lblMove
      lblScoreValue = lblScore
    }
    mainLayout.center = gameArea
  }

  // handle the key pressed event
  def handleKeyPressed(key:KeyEvent): Unit ={
    if(keyValue!=key.code.toString()){
      keyValue=key.code.toString()
      if(keyValue=="M"){
        gameArea.startAI
      }else if(keyValue=="K"){
        gameArea.stopAI
      }else if(keyValue=="L"){
        gameArea.stepAI
      }else if(key.text.matches("^\\d+$")){
        gameArea.stepAIAtDepth(key.text.toInt)
      }else{
        gameArea.handleTranslate(key.code.toString())
      }
    }

  }

  //undo the game
  def gameUndo(): Unit ={
    gameArea.undoMove()
  }

  //action when user click on back button
  def backToOption(): Unit ={
    gameArea.onBackClick()
  }

  //handle the key release event
  def handleKeyRelease(key:KeyEvent): Unit ={
    keyValue = ""
  }
}

trait GameTrait{
  def handleKeyPressed(key:KeyEvent)
  def handleKeyRelease(key:KeyEvent)
  def setGameArea(option:GameOptionData)
}
