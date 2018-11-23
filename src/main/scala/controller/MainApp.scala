package controller
import java.io.IOException

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafxml.core.{FXMLLoader, FXMLView, NoDependencyResolver}
import scalafx.Includes._
import javafx.{scene=>jfxs}

object MainApp extends JFXApp{
  val MainScene:Scene = getScene("GameOption")

  stage = new PrimaryStage() {
    title = "2^n"
    scene = MainScene
  }

  //getGameScene()
  //get the fxml scene by it location
  def getScene(name:String):Scene = {
    val source = getClass.getResource("/view/" + name + ".fxml")
    if (source == null) {
      throw new IOException("Cannot load resource: /view/"+name+".fxml")
    }

    val root =FXMLView(source, NoDependencyResolver)
    new Scene(root){

    }
  }

  //speacial method for getting game scene
  def getGameScene(option:GameOptionData): Unit = {
    val loader = new FXMLLoader(getClass.getResource("/view/Game.fxml"),NoDependencyResolver)
    loader.load()
    val root = loader.getRoot[jfxs.Parent]
    val controller = loader.getController[GameTrait]
    controller.setGameArea(option)
    stage.scene=new Scene(root){

      onKeyPressed = k=>{
        controller.handleKeyPressed(k)
      }

      onKeyReleased = k=>{
        controller.handleKeyRelease(k)
      }
    }
  }

  def setScene(name:String,title:String){
    val scene = this.getScene(name)
    stage.scene = scene
    stage.title = title
  }
}
