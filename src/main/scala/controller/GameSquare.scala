package controller

import scalafx.geometry.Pos
import scalafx.scene.control.Label
import scalafx.scene.layout.Pane
import scalafx.scene.shape.{Rectangle, StrokeType}
import scalafx.scene.paint.Color._
import scalafx.scene.text.Font
import scalafx.animation.TranslateTransition
import scalafx.util.Duration

//Companion object for GameSquare
object GameSquare{
  var parent:GameArea = _
  var size:Int = 4
  var squareSize:Double = 10
  var gap:Double = 10
  var ratio:Double = 1
  var numType:String = "classic"
  val numStrokeWidth:Int = 50
  
}


class GameSquare(initNumber:Int,pos:Array[Int],undo:Boolean=false) extends Pane {
  minWidth = GameSquare.squareSize
  maxWidth = GameSquare.squareSize
  minHeight = GameSquare.squareSize
  maxHeight = GameSquare.squareSize
  var position:Array[Int]=pos
  var removeNum:Boolean = false
  var finishInitColor:Boolean = undo
  var firstFinish:Boolean = true
  layoutX = position(1)*GameSquare.squareSize
  layoutY = position(0)*GameSquare.squareSize
  private var _num:Int = initNumber
  var displayNum:String = ""

  def num:Int = _num
  def num_= (newNum:Int): Unit = _num = newNum
  val gs:GameSquare = this
  val tt:TranslateTransition = new TranslateTransition(){
    onFinished = _ =>{
      this.stop()
      finishEvent()
    }

    node = gs

  }

  val square:Rectangle = new Rectangle(){
    x = GameSquare.gap
    y = GameSquare.gap
    height = GameSquare.squareSize-GameSquare.gap*2
    width = GameSquare.squareSize-GameSquare.gap*2
    fill = LightBlue
    stroke = Black
    strokeWidth = GameSquare.gap*2
    strokeType = StrokeType.Inside
  }

  val numView:Label = new Label(
    if(undo&&(GameSquare.numType=="blind"||GameSquare.numType=="ninja"||GameSquare.numType=="none")){
      ""
    }else if(GameSquare.numType=="classic"){
      Math.pow(2,this.num).toInt.toString
    }else{
      this.num.toString
    }
  ){
    minWidth = GameSquare.squareSize-GameSquare.gap*4
    maxWidth = GameSquare.squareSize-GameSquare.gap*4
    minHeight = GameSquare.squareSize-GameSquare.gap*4
    maxHeight = GameSquare.squareSize-GameSquare.gap*4
    layoutX = GameSquare.gap*2
    layoutY = GameSquare.gap*2
    alignment = Pos.Center
    textFill = Black
    wrapText = true
    font = new Font(25)
  }


  setColor(this.num)
  children = List(square,numView)



  def finishEvent(): Unit ={
    if(firstFinish){
      if(GameSquare.numType=="none"){
        numView.text.value = ""
      }else if(GameSquare.numType=="blind"){
        numView.text.value = ""
        square.stroke = Black
        square.fill = Black
      }else if(GameSquare.numType=="ninja"){
        numView.text.value = ""
        square.stroke = web("#FFFFFF",0)
        square.fill =web("#FFFFFF",0)
      }
      firstFinish=false
    }

    GameSquare.parent.checkFinish()
  }


  def getTranslateInfo(event:String): TranslateInfo ={
    //indicate the move position of checking the tile,
    //exp: if movX=0, movY=-1, the checking process is from up to down
    var movX:Int = 0
    var movY:Int = 0

    //indicate the starting position
    var startX:Int=position(1)
    var startY:Int=position(0)

    //indicate the new position, it might not move at all so default value is current position
    var newPosX:Int=position(1)
    var newPosY:Int=position(0)

    //indicate whether the value should increase or not,
    //and indicate this square should remove or not
    var valueIncrease:Boolean=false
    var remove:Boolean = false

    //assign start position and moving path by the event
    if(event == "RIGHT"){
      movX= -1
      startX=GameSquare.size-1
    }else if(event == "LEFT"){
      movX= 1
      startX = 0
    }else if(event == "UP"){
      movY= 1
      startY=0
    }else if(event == "DOWN"){
      movY = -1
      startY = GameSquare.size-1
    }

    //indicate should the checking process stop or not
    var stop:Boolean = false

    //indicate whether checking position is reach this square position
    //exp: if currently is checking position (0,0) and this square position is (0,1),
    //this should be false, otherwise it should be true
    var reachPlace:Boolean = false


    var matchValue:Long = 0
    var testPre:Boolean = true
    while(!stop){
      val currentPosNum:Long=GameSquare.parent.gameData(startY)(startX)

      if(!reachPlace){
        reachPlace = position(0) == startY && position(1) == startX
        if(currentPosNum==0){
          val PosX:Int=newPosX + movX* -1
          val PosY:Int=newPosY + movY* -1
          if(!((PosX<0)||(PosX>=GameSquare.size)||(PosY<0)||(PosY>=GameSquare.size))){
            newPosX =PosX
            newPosY = PosY
          }
        }else{
          if (matchValue == 0){
            matchValue = currentPosNum
          }else if(matchValue != currentPosNum){
            matchValue = currentPosNum
          }else if(matchValue == currentPosNum){
            matchValue = 0
            val PosX:Int=newPosX + movX* -1
            val PosY:Int=newPosY + movY* -1
            if(!((PosX<0)||(PosX>=GameSquare.size)||(PosY<0)||(PosY>=GameSquare.size))){
              newPosX = PosX
              newPosY = PosY
            }
            if(reachPlace){
              remove=true
            }
          }
        }

      }

      startX+=movX
      startY+=movY

      stop = (startX<0)||(startX>=GameSquare.size)||(startY<0)||(startY>=GameSquare.size)
      if(reachPlace && (!stop) && testPre && !remove){
        if(GameSquare.parent.gameData(startY)(startX)!=0){
          testPre=false
        }
        if(GameSquare.parent.gameData(startY)(startX)==this.num){
          valueIncrease=true
        }
      }
    }

    TranslateInfo(newPosX,newPosY,-(position(1)-newPosX),-(position(0)-newPosY),valueIncrease,remove)
  }

  def setNum(): Unit ={
    this.num +=1
    if(GameSquare.numType=="simple"){
      numView.text.value = this.num.toString
    }else if(GameSquare.numType=="none"||GameSquare.numType=="blind"||GameSquare.numType=="ninja"){
      numView.text.value = ""
    }else{
      numView.text.value = Math.pow(2,this.num).toInt.toString
    }
    setColor(this.num)
  }

  def setColor(num:Int):Unit={
    val colorArea:Int = Math.ceil(num /9).toInt
    val colorIndex:Int = Math.ceil((num %9)*28).toInt
    val colorOffSet:Int= colorArea%5
    var r:Int=255
    var g:Int=255
    var b:Int=255
    if(GameSquare.numType=="blind"&&finishInitColor){
      square.stroke = Black
      square.fill = Black
    }else if(GameSquare.numType=="ninja"&&finishInitColor){
      square.stroke = web("#FFFFFF",0)
      square.fill =web("#FFFFFF",0)
    }else{
      if(colorOffSet==0){
        b=0
        g=colorIndex
      }else if(colorOffSet==1){
        r=255-colorIndex
        b=0
      }else if(colorOffSet==2){
        r=0
        b=colorIndex
      }else if(colorOffSet==3){
        r=0
        g=255-colorIndex
      }else if(colorOffSet==4){
        r=colorIndex
        g=0
      }

      square.fill = web("rgb("+r+","+g+","+b+")")
      if(num<45){
        square.stroke = web("rgb("+r+","+g+","+b+")")
      }else if(num<90){
        square.stroke = Black
      }else if(num<135){
        square.stroke = Brown
      }else if(num<180){
        square.stroke = Silver
      }else if(num<225){
        square.stroke = Gold
      }
    }


    finishInitColor=true
  }

  def startTranslate(translateInfo:TranslateInfo): Unit ={
    removeNum = translateInfo.remove
    translateTo(translateInfo.translateValueX,translateInfo.translateValueY)
    position(1)=translateInfo.newPosX
    position(0)=translateInfo.newPosY
  }

  def translateTo(x:Int,y:Int): Unit ={
    tt.byX=x*GameSquare.squareSize
    tt.byY=y*GameSquare.squareSize
    tt.duration = Duration.apply(100)
    tt.play()
  }

}

case class TranslateInfo(newPosX:Int,newPosY:Int,translateValueX:Int,translateValueY:Int,increase:Boolean,remove:Boolean)
