package controller

import scalafx.beans.property.{StringProperty}
class Player (date : String, score: String){
  var dates = StringProperty(date)
  var scores   = new StringProperty(score)
}
