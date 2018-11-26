package controller

object AIFunction {


  def checkCommand(gameData:Array[Array[Long]],depth:Int): CommandData ={
    val AIGSList:List[AIGameSquare] = createGameSquares(gameData)
    val availableMove:List[String] = checkAvailableMove(AIGSList)
    var bestCommand:String=""
    var bestScore:Long=Long.MaxValue
    val newDepth:Int = depth -1

    for(move:String<-availableMove){
      //println(move)
      val newGameData:Array[Array[Long]] = moveSquare(gameData,move)
      val score:Long=if(newDepth==0) evaluateMap(newGameData) else checkRandPos(newGameData,newDepth)
      //println("depth:"+newDepth+" score:"+score)
      if(score<bestScore){
        bestScore=score
        bestCommand=move
      }
    }
//    println("depth:"+newDepth+" best:"+bestScore)
//    println("")
    CommandData(bestScore,bestCommand)
  }

  def checkRandPos(gameData:Array[Array[Long]],depth:Int): Long ={
    var bestScore:Long=Long.MinValue
    val size:Int = gameData.length
    val newDepth = depth-1
    var count = 0
    for(y <-0 until size; x <-0 until size){
      if(gameData(y)(x)==0){
        count+=1
        val newGameData:Array[Array[Long]] = addSquare(gameData,y,x)
        val score:Long=if(newDepth==0) evaluateMap(newGameData) else checkCommand(newGameData,newDepth).score
//        println("depth:"+newDepth+" score:"+score)
        if(score>bestScore){
          bestScore=score
        }
      }
    }
//    println("depth:"+newDepth+" best:"+bestScore)
//    println("")
    if(count==0){
      bestScore=Long.MaxValue
    }
    bestScore
  }

  def checkAvailableMove(activeNum:List[AIGameSquare]): List[String] ={
    var upMove =false
    var downMove = false
    var leftMove = false
    var rightMove = false
    val commandList:List[String]=List(
      "LEFT",
      "DOWN",
      "RIGHT",
      "UP"
    )

    var availableCommand:List[String] = List()

    //loop through the up, down, left and right command
    for(command<-commandList){
      //loop through all active num to check is there any move
      for(num<-activeNum){
        val translateInfo:TranslateInfo=num.getTranslateInfo(command)
        val allowMove:Boolean = (translateInfo.translateValueY!=0)||(translateInfo.translateValueX!=0)

        if(allowMove){
          if(command=="UP"){
            if(!upMove){
              upMove=allowMove
              availableCommand :+= "UP"
            }
          }else if(command=="DOWN"){
            if(!downMove){
              downMove=allowMove
              availableCommand :+= "DOWN"
            }
          }else if(command=="LEFT"){
            if(!leftMove){
              leftMove=allowMove
              availableCommand :+= "LEFT"
            }
          }else if(command=="RIGHT"){
            if(!rightMove){
              rightMove=allowMove
              availableCommand :+= "RIGHT"
            }
          }
        }
        //enable command if there's a move on any square
      }
    }
    availableCommand
  }

  def createGameSquares(gameData:Array[Array[Long]]):List[AIGameSquare]={
    val size:Int = gameData.length
    var AIGSList:List[AIGameSquare] = List()
    var i = 0
    while(i<size){
      var j = 0
      while(j<size){
        if(gameData(i)(j)!=0){
          AIGSList :+= new AIGameSquare(gameData(i)(j).toInt,Array(i,j))
        }
        j+=1
      }
      i+=1
    }
    AIGSList
  }

  def addSquare(gameData:Array[Array[Long]],y:Int,x:Int): Array[Array[Long]] ={
    val size = gameData.length
    val newGameData:Array[Array[Long]]=Array.ofDim[Long](size,size)
    var i = 0
    while(i<size){
      var j = 0
      while(j<size){
        newGameData(i)(j)=if(i==y && j==x) 1 else gameData(i)(j)
       // print(newGameData(i)(j))
        j+=1
      }
//      println("")
      i+=1
    }
//    println("")

    newGameData
  }

  def moveSquare(gameData:Array[Array[Long]],command:String):Array[Array[Long]] ={
    val size = gameData.length
    val newGameData:Array[Array[Long]]=Array.ofDim[Long](size,size)
    var i = 0
    val AIGSList:List[AIGameSquare] = createGameSquares(gameData)
    while(i<size){
      var j = 0
      while(j<size){
        newGameData(i)(j)=0
        j+=1
      }
      i+=1
    }

    for(gs:AIGameSquare <- AIGSList){
      val tf:TranslateInfo=gs.getTranslateInfo(command)
      if(!tf.remove){
        newGameData(tf.newPosY)(tf.newPosX)=if(tf.increase) gs.num+1 else gs.num
      }
    }
    newGameData
  }

  def evaluateMap(gameData:Array[Array[Long]]): Long ={
    var score:Long = 0
    val size:Int = gameData.length
    var count:Int= 1
    var k = 0
    while(k<size){
      var l = 0
      while(l<size){
        if(gameData(k)(l)==0){
          count+=1
        }
        if((k+1)!=size){
          score+=Math.abs(gameData(k)(l)-gameData(k+1)(l))
        }
        if((l+1)!=size){
          score+=Math.abs(gameData(k)(l)-gameData(k)(l+1))
        }
        l+=1
      }
      k+=1
    }
    score/count
  }
}

class AIGameSquare(initNumber:Int,pos:Array[Int],undo:Boolean=false){
  var position:Array[Int]=pos
  var removeNum:Boolean = false
  var finishInitColor:Boolean = undo
  var firstFinish:Boolean = true
  private var _num:Int = initNumber
  var displayNum:String = ""
  def num:Int = _num
  def num_= (newNum:Int): Unit = _num = newNum
  val gs:AIGameSquare = this

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
  }
}


case class CommandData(score:Long,command:String)