package controller

import scalafx.scene.control.Button
import scalafxml.core.macros.sfxml

@sfxml
class MainPage(
                val btnStart: Button,
                val btnHowToPlay: Button,
                val btnLeaderBoard: Button,
                val btnSetting: Button,
              ) {


  //Switch scene

  def startGame(): Unit ={
    MainApp.setScene("GameOption","Option")
  }

  def checkLeaderBoard(): Unit ={
    MainApp.setScene("LeaderboardPage","Leader Board")
  }

  def exit(): Unit ={
    MainApp.stage.close()
  }

  def howToPlayGame(): Unit ={
    MainApp.setScene("HowtoPlayPage","How To Play")
  }
}
