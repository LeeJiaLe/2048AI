package controller

import java.sql._


import scalafx.collections.ObservableBuffer

object HighScores {
  val url = "jdbc:sqlite:2048.db"
  val driver = "org.sqlite.JDBC"
  val playerData = new ObservableBuffer[Player]()
  Class.forName(driver)
  // create a connection to the database
  val connection: Connection = DriverManager.getConnection(url)


  def save(playdate: String, score: String) {
    val stat: Statement = connection.createStatement()

    val op: String = "INSERT INTO HighScore (PlayDate, Score) VALUES ('" + playdate + "', '" + score + "');"
    stat.executeUpdate(op) //Here is the problem
    stat.close()


  }

  def display(): Unit ={
    val op:String = "SELECT PlayDate, Score FROM HighScore ORDER BY Score Desc"
    val stmt: Statement = connection.createStatement()
    val execute = stmt.executeQuery(op)
    while (execute.next()){

      val date: String = execute.getString("PlayDate")

      val score: String  = execute.getString("Score")
      playerData.add(new Player(date,score))
    }

  }
}



