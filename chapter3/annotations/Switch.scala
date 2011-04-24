import annotation.switch

object WeekDay extends Enumeration {
  type WeekDay = Value
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  val Mon_ID = Mon.id;
  val Tue_ID = Tue.id;
  val Wed_ID = Wed.id;
  val Thu_ID = Thu.id;
  val Fri_ID = Fri.id;
  val Sat_ID = Sat.id;
  val Sun_ID = Sun.id;
}

import WeekDay._

class Test {
  


  def unannotated(x : Int) = x match {
    case 1 => "One"
    case 2 => "Two!"
    case z => z + "?"
  }
  def annotated(x : Int) = (x : @switch) match {
    case 1 => "One"
    case 2 => "Two!"
    case z => z + "?"
  }
  def notOptimized(x : Any) = x match {
    case 1 => "One"
    case 2 => "Two!"
    case _ => x + "?"
  }
  def notOptimised2(x : Int) = x match {
    case 1 => "One"
    case 2 => "Two!"    
    case i if i % 2 == 0 => "Even"
    case _ => "Odd"
  }
  def notOptimised3(x : Int) = x match {
    case 1 => "One"
    case 2 => "Two!"    
    case i : Int => "Other"
  }
  def notOptimisedEnum(x : WeekDay) = x.id match {
    case Mon_ID => "Case of the Mondays"
    case Tue_ID => "Ruby Tuesdays?"
    case Fri_ID => "Thank God!"
    case _ => "Not an interesting day"
  }

  def mightBeOptimised(x : WeekDay) = x match {
    case Mon => "Case of the Mondays"
    case Tue => "Ruby Tuesdays?"
    case Fri => "Thank God!"
    case _ => "Not an interesting day"
  }
}
