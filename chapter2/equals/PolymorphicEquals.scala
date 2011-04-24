
trait InstantaneousTime {
  val repr : Int
  override def equals(other : Any) : Boolean = other match {
    case that : InstantaneousTime =>
      if(this eq that) {
        true
      } else {
        (that.## == this.##) &&
        (repr == that.repr)
      }
    case _ => false
  }
  override def hashCode() : Int = repr.hashCode
}

trait Event extends InstantaneousTime {
  val name : String
  override def equals(other : Any) : Boolean = other match {
    case that : Event =>
      if(this eq that) { 
        true
      } else {
        (repr == that.repr) &&
        (name == that.name)
      }
    case _ => false
  }
}


trait InstantaneousTime extends Equals {
  val repr : Int
  override def canEqual(other : Any) = other.isInstanceOf[InstantaneousTime]
  override def equals(other : Any) : Boolean = other match {
    case that : InstantaneousTime =>
      if(this eq that) true else {
             (that.## == this.##) &&
             (that canEqual this) &&
             (repr == that.repr)
      }
    case _ => false
  }
  override def hashCode() : Int = repr.hashCode
}

trait Event extends InstantaneousTime {
  val name : String
  override def canEqual(other : Any) = other.isInstanceOf[Event]
  override def equals(other : Any) : Boolean = other match {
    case that : Event =>
      if(this eq that) { 
        true
      } else {
        (that canEqual this) &&
        (repr == that.repr) &&
        (name == that.name)
      }
    case _ => false
  }
}


object InstantaneousTime {
  def apply(secondsGMT : Int) = new InstantaneousTime {
    override val repr = secondsGMT
  }
}

/*
class TimeSeriesRange(start : Time, end : Time) extends Equals {
  def hashCode() : Int = start.## + ((27+end.##) * 13)
  def equals(other : Any) : Boolean = other match {
    case that : TimeSeriesRange =>
      // Check simple equals first
      if(that eq this) {
        true
      } else if ( (that.## != this.##) &&   // If hashCode is quicker than deep check
                                            // , we can check differences for quick failure
                  (that canEqual this) ){   // Make sure equivalence holds true! -> Allows subclasses to "opt-out" of equality!
        // Our implementation
        (start == that.start) && (stop == that.stop)
      } else false
    case _ => false
  }

  def canEqual(other : Any) : Boolean = other.isInstanceOf[TimeSeriesRange]
}

object Day {
  def startOfDay(day : Time) : Time = day
  def endOfDay(day : Time) : Time = day
}

class DayRange(day : Time) extends TimeSeriesRange(Day.startOfDay(day),Day.endOfDay(day)) {
 override def canEqual(other : Any) : Boolean = other.isInstanceof[DayRange]
 // We don't have to override equals, although
}

*/


