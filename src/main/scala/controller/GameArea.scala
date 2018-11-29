package controller

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId}
import java.util.Random

import javafx.scene.Scene
import javafx.stage.{Modality, Stage}
import javafx.{scene => jfxs}
import scalafx.animation.FadeTransition
import scalafx.geometry.Pos
import scalafx.scene.Group
import scalafx.scene.control.Label
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Font
import scalafx.util.Duration
import scalafxml.core.{FXMLLoader, NoDependencyResolver};

class GameArea(option:GameOptionData, ratio:Double) extends Group {
  //random function which help generate random number
  val rand = new Random()

  //size of the game area (eg: 10x10, 11x11)
  val size:Int = option.size

  //indicate how the number will display in game (eg:none, ninja)
  val numType:String = option.numDisplay

  //maximum number which is able to generate in game (eg: 2, 4, 8....)
  val maxRandom:Int = option.maxRandom

  //store the index of all the blank tile without number inside,
  //so later can random choose one of the index
  var blankIndex:List[Array[Int]] = List()

  //data in each tile
  var gameData:Array[Array[Long]] =Array.ofDim[Long](size,size)

  //size of each square
  val squareSize:Double= 600/size

  //gap between square
  val gap:Double = squareSize/32

  //all active square in game area
  var activeNum:List[GameSquare] = List()

  //indicate how many square finish its animation
  var finishNum:Int = 0

  //indicate the key should be active or not,
  //false, to prevent user press any key when the animation still go
  var activeKey:Boolean = true

  //indicate the up, down, left and right command should be enabled or not
  var upMove:Boolean = true
  var downMove:Boolean = true
  var leftMove:Boolean = true
  var rightMove:Boolean = true

  //calculate the move
  var moveCount:Int = 0

  //init the game over transition
  var gameOverTransition:FadeTransition = new FadeTransition(){
    duration =Duration(750)
    fromValue = 0
    toValue = 1
  }

  var aiFun:String = "D"
  var depth:Int = 9

  //score and move label
  var lblScoreValue:Label = _
  var lblMoveCount:Label = _

  //keep the history off all the map which done by player
  // so that the game able to undo
  var moveHistory:List[GameHistory] = List()

  //indicate the restriction of the game
  var noUpAllow:Boolean = (option.restrict=="noUp")||(option.restrict=="noUpLeft")
  var noLeftAllow:Boolean = option.restrict=="noUpLeft"

  //init the game area background
  val gameBackground:Group = new Group(){

    //the big square in the background
    val back:Rectangle = new Rectangle(){
      height = 600
      width = 600
      x = 0
      y = 0
      fill = web("rgba(186,174,160,1.0)")
    }

    children.add(back)

    //all small square which indicate the position at the background
    for(yp <-0 until size;xp <- 0 until size){
      gameData(yp)(xp)=0

      val square:Rectangle = new Rectangle(){
        //calculate the size and position base on the game data provided
        height = squareSize-gap*2
        width = squareSize-gap*2
        x = xp*squareSize+gap
        y = yp*squareSize+gap
        fill = web("rgba(214,205,196,1.0)")

      }

      children.add(square)
    }
  }

  //store the active square
  val gameElement:Group = new Group()

  //store the game over text
  val textContainer:VBox = new VBox{
    alignment = Pos.Center
    prefHeight = 600
    prefWidth = 600
  }

  //store the element which to notify game over
  val gameOverNotification:Group = new Group(){
    opacity = 0
    val background:Rectangle = new Rectangle(){
      height = 600
      width = 600
      x = 0
      y = 0
      fill = web("#FFFC9A",0.7)
    }

    val gameOverText:Label = new Label("Game Over"){
      font = new Font("System Bold",55)
    }

    val gameOverTextSmall:Label = new Label("No More Move"){
      font = new Font("System Bold",25)
    }

    textContainer.children.add(gameOverText)
    textContainer.children.add(gameOverTextSmall)

    children = List(
      background,
      textContainer
    )
  }


  alignmentInParent = Pos.TopCenter

  //add all the layout
  children.add(gameBackground)
  children.add(gameElement)
  children.add(gameOverNotification)

  //specify the game over transition control which element
  gameOverTransition.node = gameOverNotification

  //set up the companion object of the game square
  setUpGameSquareCompanion()

  //add random number at random position
  addNumAtRandomPos(rand.nextInt(maxRandom)+1)

  //if user setup the game to init some number,
  //this will run
  if(option.initRandom!=0){
    val length:Int = Math.ceil(size*size*option.initRandom).toInt
    for(x<-1 to length){
      addNumAtRandomPos(x)
    }
  }

  //check all the available move
  checkAvailableMove()

  //add this map to history
  addToHistory()

  //handle all translation of the square
  def handleTranslate(event:String): Unit ={

    //check if the key is active,
    //key only active when all the previous animation is finish
    if(activeKey){

      //check if current pressed key is allowed to click
      if(
        (event=="UP"&&upMove)||
        (event=="DOWN"&&downMove)||
        (event=="LEFT"&&leftMove)||
        (event=="RIGHT"&&rightMove)
        ){

        //indicate animation is start, cannot press
        //any key until animation finish
        activeKey=false

        //loop through all the active square
        for(num<-activeNum){
          val translateInfo:TranslateInfo=num.getTranslateInfo(event)
          if(translateInfo.increase){
            num.setNum()
          }
          num.startTranslate(translateInfo)
        }
      }

    }
  }

  def checkFinish(): Unit ={
    //when a square finish it animation, this function will call
    //finish num will plus 1 to indicate one more animation is finish
    finishNum+=1

    //check if all animation is finish
    if(finishNum>=activeNum.length){

      //loop through all the number to check if this number need to remove from game area
      for(gs<-activeNum){
        if(gs.removeNum){
          removeActive(gs)
        }
      }

      //reset the game data to zero
      for(yp <-0 until size;xp <- 0 until size){
        gameData(yp)(xp)=0
      }

      //set all the new number to game data
      for(num<-activeNum){
        gameData(num.position(0))(num.position(1))=num.num
      }

      //reset the finish number to zero, add random number at random blank tile
      //and check if there's any available move
      finishNum=0
      addNumAtRandomPos(rand.nextInt(maxRandom)+1)
      checkAvailableMove()

      //if no more move, play the game over transition to notify user no more move
      if(!(upMove||downMove||leftMove||rightMove)){
        gameOverTransition.play()
      }

      //after all transition finish,
      //user able to click any key, move count will add one,
      //set the score and add current map to history
      activeKey=true
      moveCount+=1
      setScoreLabel()
      addToHistory()

      if(aiFun=="M"){
        if(!(upMove||downMove||leftMove||rightMove)){
          aiFun=="D"
        }else{
          val allMapAndScore: AllMapAndScore = new AllMapAndScore
          handleTranslate(AIFunction.checkCommand(Long.MinValue,Long.MaxValue,allMapAndScore,gameData,depth).command)
        }
      }
    }
  }

  def onBackClick(): Unit ={

    //set up the dialog box
    val loader = new FXMLLoader(getClass.getResource("/view/DialogBox.fxml"),NoDependencyResolver)
    loader.load()
    val root = loader.getRoot[jfxs.Parent]
    val controller = loader.getController[DialogBoxTrait]
    var backToMenu:Boolean = false
    controller.setResult = x=>{
      backToMenu = x
    }
    val scene:Scene = new Scene(root)
    val stage:Stage = new Stage()
    controller.stage = stage
    stage.setScene(scene)
    stage.initModality(Modality.APPLICATION_MODAL)
    stage.showAndWait()

    if(backToMenu){
      val dtf:DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
      val now:LocalDateTime = LocalDateTime.now.atZone(ZoneId.of("Asia/Kuala_Lumpur")).toLocalDateTime;
      HighScores.save(dtf.format(now),(moveCount*100).toString)
      MainApp.setScene("GameOption","Option")
    }
  }

  //set the move and score
  def setScoreLabel(): Unit ={
    lblMoveCount.text = "Move: "+moveCount.toString
    lblScoreValue.text = "Score: "+(moveCount*100).toString
  }


  def checkAvailableMove(): Unit ={
    upMove =false
    downMove = false
    leftMove = false
    rightMove = false
    val commandList:List[String]=List(
      "RIGHT",
      "UP",
      "LEFT",
      "DOWN"
    )

    //loop through the up, down, left and right command
    for(command<-commandList){

      //loop through all active num to check is there any move
      for(num<-activeNum){
        val translateInfo:TranslateInfo=num.getTranslateInfo(command)
        val allowMove:Boolean= (translateInfo.translateValueY!=0)||(translateInfo.translateValueX!=0)

        //enable command if there's a move on any square
        if(command=="UP"){
          if(!upMove){
            upMove=allowMove
          }
        }else if(command=="DOWN"){
          if(!downMove){
            downMove=allowMove
          }
        }else if(command=="LEFT"){
          if(!leftMove){
            leftMove=allowMove
          }
        }else if(command=="RIGHT"){
          if(!rightMove){
            rightMove=allowMove
          }
        }
      }
    }

    //if not allow the move, then disable it
    if(noUpAllow){
      upMove=false
    }
    if(noLeftAllow){
      leftMove=false
    }
  }

  def removeActive(gs:GameSquare): Unit ={

    //remove the selected square
    gameElement.children.remove(gs)
    var found:Boolean = false
    var x=0


    while(!found){
      //when found in list, start remove
      found=activeNum(x) == gs
      if(found){
        activeNum = activeNum.take(x) ++ activeNum.drop(x+1)
      }
      x+=1
    }
    activeNum.filterNot(p=>p==gs)

  }

  def removeAllActive(): Unit ={
    for(gs<- activeNum){
      gameElement.children.remove(gs)
    }
    activeNum=List()
  }


  def undoMove(): Unit ={

    //check if undo is allow
    if(option.undo){
      //if player move more than one step
      if(moveCount>1){

        //always set the opacity to 0
        gameOverNotification.opacity = 0
        moveCount-=1

        //setup new game data by fetching previous map in history
        val newGameData:Array[Array[Long]] = moveHistory(1).board.map(_.clone())

        //check whether command is allowed
        upMove=moveHistory(1).upMove
        downMove=moveHistory(1).downMove
        leftMove=moveHistory(1).leftMove
        rightMove=moveHistory(1).rightMove

        //remove current map
        moveHistory=moveHistory.drop(1)

        //remove all active number in game
        removeAllActive()

        //set all the game data and the tile
        for(yp <-0 until size;xp <- 0 until size){
          gameData(yp)(xp)=0
          if(newGameData(yp)(xp)!=0){
            addNumAtFixedPos(yp,xp,newGameData(yp)(xp).toInt)
          }
        }
        setScoreLabel()
      }

    }

  }

  def addToHistory(): Unit ={
    //copy all element inside 2d array
    moveHistory +:= GameHistory(
      gameData.map(_.clone()),
      moveCount*100,
      moveCount,
      upMove,
      downMove,
      leftMove,
      rightMove
    )
    if(moveHistory.length > 100){
      moveHistory=moveHistory.dropRight(1)
    }
  }

  def addNumAtRandomPos(_num:Int): Unit ={
    val blankIndex:Array[Int] = getRandomBlankIndex
    val newNum:GameSquare = new GameSquare(_num,blankIndex)
    activeNum :+= newNum
    gameData(blankIndex(0))(blankIndex(1)) = newNum.num
    gameElement.children.add(newNum)
  }

  def addNumAtFixedPos(y:Int,x:Int,_num:Int): Unit ={
    val blankIndex:Array[Int] = Array(y,x)
    val newNum:GameSquare = new GameSquare(_num,blankIndex,true)
    activeNum :+= newNum
    gameData(blankIndex(0))(blankIndex(1)) = newNum.num
    gameElement.children.add(newNum)
  }

  def setUpGameSquareCompanion(): Unit ={
    GameSquare.parent = this
    GameSquare.size = option.size
    GameSquare.squareSize = squareSize
    GameSquare.gap = gap
    GameSquare.numType = option.numDisplay
  }

  def getRandomBlankIndex:Array[Int]={
    for(yp <-0 until size;xp <- 0 until size){
      if(gameData(yp)(xp)==0){
        blankIndex :+= Array(yp,xp)
      }
    }

    if(blankIndex.nonEmpty){
      val selectedIndex:Array[Int] = blankIndex(rand.nextInt(blankIndex.length))
      blankIndex = List()
      selectedIndex
    }else{
      Array(-1,-1)
    }

  }

  def startAI(): Unit ={
    aiFun = "M"
    val allMapAndScore: AllMapAndScore = new AllMapAndScore
    handleTranslate(AIFunction.checkCommand(Long.MinValue,Long.MaxValue,allMapAndScore,gameData,depth).command)
  }

  def stepAI(): Unit ={
    aiFun = "D"
    val allMapAndScore: AllMapAndScore = new AllMapAndScore
    handleTranslate(AIFunction.checkCommand(Long.MinValue,Long.MaxValue,allMapAndScore,gameData,depth).command)
  }

  def stepAIAtDepth(depth:Int): Unit ={
    aiFun = "D"
    val allMapAndScore: AllMapAndScore = new AllMapAndScore
    if(depth!=0){
      handleTranslate(AIFunction.checkCommand(Long.MinValue,Long.MaxValue,allMapAndScore,gameData,depth).command)
    }else{
      undoMove()
    }

  }

  def stopAI():Unit={
    aiFun="D"
  }
}


case class GameOptionData(size:Int, undo:Boolean,numDisplay:String,restrict:String,initRandom:Double,maxRandom:Int)
case class GameHistory(
                       board:Array[Array[Long]],
                       score:Long,
                       move:Int,
                       upMove:Boolean,
                       downMove:Boolean,
                       leftMove:Boolean,
                       rightMove:Boolean
                      )