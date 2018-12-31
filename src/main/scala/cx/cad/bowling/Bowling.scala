package cx.cad.bowling

/**
  *
  * @param count The number of pins that were knocked down in the roll.
  */
case class Roll(count: Int) extends AnyVal

/**
  *
  * @param points The total number of points for a Game.
  */
case class Score(points: Int) extends AnyVal

/**
  * Each frame has a score but requires different information to calculate that
  * score. This trait provides a generic way to address all frames.
  */
trait Frame {
  val score: Score
}

/**
  * Any frame in which the bowler did not knock down 10 pins.
  *
  * @param first the first roll of the frame
  * @param second the second roll of the frame
  */
case class Open(first: Roll, second: Roll) extends Frame {
  val score: Score = Score(first.count + second.count)
}

/**
  * A frame in which the bowler knocked down 10 pins on the first roll.
  *
  * The bowler gets additional points in this frame based on the next two rolls,
  * if they exist.
  *
  * Hint: They only don't exist in the 10th frame's two extra rolls, both of
  * which this implementation interprets as frames but drops before scoring.
  *
  * @param nextRoll the next roll
  * @param overnextRoll the roll after next
  */
case class Strike(nextRoll: Option[Roll] = None, overnextRoll: Option[Roll] = None) extends Frame {
  val score: Score = Score(
    10
      + nextRoll.fold(0)(_.count)
      + overnextRoll.fold(0)(_.count)
  )
}

/**
  * A frame in which the bowler knocked down all 10 pins by the second roll.
  *
  * The bowler gets additional points in this frame based on the next roll, if
  * it exists.
  *
  * Hint: The next roll only doesn't exist in the 10th frame's bonus rolls.
  *
  * @param first the first roll of the frame
  * @param second the second roll of the frame
  * @param next the next roll after the frame
  */
case class Spare(first: Roll, second: Roll, next: Option[Roll] = None) extends Frame {
  val score: Score = Score(
    10
      + next.fold(0)(_.count))
}


import cx.cad.bowling.Game._

/**
  * Game with a score and frame records
  *
  * Note that the number of rolls is not checked. An incomplete game is still a
  * game but only the first 10 frames are regarded.
  *
  * @param rolls The count of each roll
  */
case class Game(rolls: Seq[Roll]) {
  val frames: List[Frame] = rollsToFrames(rolls)
  val score: Int = frames.foldLeft(0) { (cumulative, frame) => cumulative + frame.score.points}
}

object Game {
  /**
    * Create Game with a score and frame records given an array of roll counts.
    *
    * @param rolls The count of each roll
    * @return a Game
    */
  def from(rolls: Array[Int]): Game = {
    Game(rolls.map(Roll))
  }

  /**
    * Build frames from a sequence of rolls.
    *
    * @param remainingRolls the sequence of rolls
    * @return the list of frames, limited to 10
    */
  def rollsToFrames(remainingRolls: Seq[Roll]): List[Frame] = {
    remainingRolls.toList match {
      case Roll(10) :: cdr =>
        val nextTwoRolls = cdr.take(2).lift
        Strike(nextTwoRolls(0), nextTwoRolls(1)) :: rollsToFrames(cdr)
      case first :: second :: cdr if first.count + second.count == 10 =>
        Spare(first, second, cdr.headOption) :: rollsToFrames(cdr)
      case first :: second :: cdr if first.count + second.count < 10 =>  Open(first, second) :: rollsToFrames(cdr)
      case _ => Nil
    }
  // TODO: this could be configurable. Is there ever a reason for more than
  //       10 frames?
  }.take(10) // Parsing rolls into frames leads to an interesting side effect:
             // bonus rolls earned during the tenth frame look like additional
             // frames. There might be another pattern that could detect this,
             // but the more straightforward solution is to simply truncate the
             // extra frames, relying on the look-ahead of the Strike and Spare
             // branches of the above match to get the data they need before it
             // is dropped.
}
