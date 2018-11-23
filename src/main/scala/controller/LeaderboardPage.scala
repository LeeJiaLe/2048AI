package controller
import scalafx.scene.control.{TableColumn, TableView}
import scalafxml.core.macros.sfxml



@sfxml
class LeaderboardPage(val playerTable: TableView[Player],
                      val firstDateColumn : TableColumn[Player, String],
                      val secondScoresColumn : TableColumn[Player, String]) {
  def backToMenu(): Unit ={
    MainApp.setScene("MainPage","2^n")
    playerTable.getItems.clear()
  }

  HighScores.display()
  playerTable.items = HighScores.playerData
  firstDateColumn.cellValueFactory = {x => x.value.dates}
  secondScoresColumn.cellValueFactory  = {_.value.scores}
}
