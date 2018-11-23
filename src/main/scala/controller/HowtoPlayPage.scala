package controller
import scalafxml.core.macros.sfxml
@sfxml
class HowtoPlayPage {
  def backToMenu(): Unit ={
    MainApp.setScene("MainPage","2^n")
  }
}
