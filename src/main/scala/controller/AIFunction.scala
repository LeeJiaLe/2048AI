package controller

object AIFunction {


  def checkCommand(alpha:Long, beta:Long,allMapAndScore: AllMapAndScore, gameData:Array[Array[Long]],depth:Int): CommandData ={
    val AIGSList:List[AIGameSquare] = createGameSquares(gameData)
    val availableMove:List[String] = checkAvailableMove(AIGSList)
    var bestCommand:String=if(availableMove.nonEmpty) availableMove.head else ""
    val newDepth:Int = depth -1
    val newAlpha:Long = alpha
    var newBeta:Long = beta
    var i = 0
    var loop:Boolean = i<availableMove.length
    var break:Boolean = false
    while(loop){
      //println(move)
      val move:String = availableMove(i)
      val newGameData:Array[Array[Long]] = moveSquare(gameData,move)
      val initScore:Long=allMapAndScore.checkExist(move,newGameData)
      val score:Long=if(initScore!= -1) initScore else if(newDepth==0) evaluateMap(newGameData) else checkRandPos(newAlpha, newBeta,newGameData,newDepth)
      //println("depth:"+newDepth+" score:"+score)
      if(initScore == -1){
        allMapAndScore.add(move,MapAndScore(newGameData,score))
      }
      if(score<newBeta){
        newBeta=score
        bestCommand=move
      }

      break = newAlpha>=newBeta

      i+=1
      loop = i<availableMove.length

      if(break){
        //println("hi")
        loop=false
      }
    }
//    println("depth:"+newDepth+" best:"+bestScore)
//    println("")
    CommandData(newBeta,bestCommand)
  }

  def checkRandPos(alpha:Long, beta:Long,gameData:Array[Array[Long]],depth:Int): Long ={
    val size:Int = gameData.length
    val newDepth = depth-1
    var newAlpha:Long = alpha
    val newBeta:Long = beta
    var count = 0
    var y = 0
    var loop:Boolean = y<size
    var break:Boolean = false
    val allMapAndScore:AllMapAndScore=new AllMapAndScore
    while(loop){
      var x = 0
      var loop2:Boolean = x<size
      while(loop2){
        var loop3:Boolean = true
        var z=1
        while(loop3){
          if(gameData(y)(x)==0){
            count+=1
            val newGameData:Array[Array[Long]] = addSquare(gameData,y,x,z)
            val score:Long=if(newDepth==0) evaluateMap(newGameData) else checkCommand(newAlpha, newBeta,allMapAndScore,newGameData,newDepth).score
            //        println("depth:"+newDepth+" score:"+score)
            if(score>newAlpha){
              newAlpha=score
            }
            break = newAlpha>=newBeta
          }
          z+=1
          loop3 = z<3
          if(break){
            loop3=false
          }
        }

        x+=1
        loop2 = x<size
        if(break){
          loop2=false
        }
      }
      y+=1
      loop = y<size
      if(break){
        loop=false
      }
    }
//    println("depth:"+newDepth+" best:"+bestScore)
//    println("")
    newAlpha
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
          AIGSList :+= new AIGameSquare(gameData(i)(j).toInt,Array(i,j),gameData)
        }
        j+=1
      }
      i+=1
    }
    AIGSList
  }

  def addSquare(gameData:Array[Array[Long]],y:Int,x:Int,z:Int): Array[Array[Long]] ={
    val size = gameData.length
    val newGameData:Array[Array[Long]]=Array.ofDim[Long](size,size)
    var i = 0
    while(i<size){
      var j = 0
      while(j<size){
        newGameData(i)(j)=if(i==y && j==x) z else gameData(i)(j)
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
    val newGameData:Array[Array[Long]]=Array.fill(size)(Array.fill(size)(0))
    val AIGSList:List[AIGameSquare] = createGameSquares(gameData)

//    AIGSList.par.foreach {
//      gs=>{
//        val tf:TranslateInfo=gs.getTranslateInfo(command)
//        if(!tf.remove){
//          newGameData(tf.newPosY)(tf.newPosX)=if(tf.increase) gs.num+1 else gs.num
//        }
//      }
//    }

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




class AIGameSquare(initNumber:Int,pos:Array[Int],gameData:Array[Array[Long]],undo:Boolean=false){
  var position:Array[Int]=pos
  var removeNum:Boolean = false
  private var _num:Int = initNumber
  def num:Int = _num
  def num_= (newNum:Int): Unit = _num = newNum

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
      val currentPosNum:Long=gameData(startY)(startX)
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
        if(gameData(startY)(startX)!=0){
          testPre=false
        }
        if(gameData(startY)(startX)==this.num){
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

class AllMapAndScore(){
  var mapAndScoreUp:List[MapAndScore] = List()
  var mapAndScoreDown:List[MapAndScore] = List()
  var mapAndScoreLeft:List[MapAndScore] = List()
  var mapAndScoreRight:List[MapAndScore] = List()

  def checkExist(s:String,map:Array[Array[Long]]): Long ={
    val size = mapAndScore(s).length

    var loop:Boolean=true
    var break:Boolean =false
    var score:Long = -1
    var i:Int =0
    loop = i<size
    while(loop){
      val currentMapAndScore:MapAndScore= mapAndScore(s)(i)
      if(checkIdentical(map,currentMapAndScore.map)){
        score = currentMapAndScore.score
        break = true
      }
      i+=1
      loop = i<size
      if(break){
        loop = false
      }
    }
    score
  }

  def add(s:String,map:MapAndScore): Unit ={
    if(s=="UP")
      mapAndScoreUp :+= map
    else
    if(s=="RIGHT")
      mapAndScoreRight :+= map
    else
    if(s=="LEFT")
      mapAndScoreLeft :+= map
    else
      mapAndScoreDown :+= map
  }

  def mapAndScore(s:String):List[MapAndScore]={
    if(s=="UP")
      mapAndScoreUp
    else
    if(s=="RIGHT")
      mapAndScoreRight
    else
    if(s=="LEFT")
      mapAndScoreLeft
    else
      mapAndScoreDown
  }

  def checkIdentical(a:Array[Array[Long]],b:Array[Array[Long]]): Boolean ={
    val size=a.length
    var i = 0
    var loop1:Boolean=true
    var break:Boolean =false
    var identical:Boolean = true
    while(loop1){
      var j = 0
      var loop2:Boolean=true
      while(loop2){
        identical = a(i)(j)==b(i)(j)
        break = !identical
        j+=1
        loop2 = j<size

        if(break){
          loop2 = false
        }
      }
      i+=1
      loop1 = i<size
      if(break){
        loop1 = false
      }
    }

    identical
  }
}
case class MapAndScore(map:Array[Array[Long]],score:Long)