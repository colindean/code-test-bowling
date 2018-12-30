package cx.cad.bowling


package object bowling {
  val EndOfGame = None
}

case class Roll(count: Int) extends AnyVal

case class Game(rolls: Seq[Roll]) {
  def score: Int = frames.foldLeft(0) { (cumulative, frame) => cumulative + frame.score}
  def frames: Seq[Frame] = {
    rolls.sliding(2)
    ???
  }
}

object Game {
  def from(rolls: Array[Int]): Game = {
    Game(rolls.view.map(Roll))
  }
}

case class Score(points: Int) extends AnyVal
/*trait Frame {
  val first: Int
  val second: Option[Int]
  val third: Option[Int]
}*/
trait Frame {
  val score: Int
  val first: Roll
}
trait LookaheadFrame extends Frame {
  val nextFrame: Option[Frame]
}
case class Open(first: Roll, second: Roll) extends Frame {
  val score: Int = first.count + second.count
}
case class Strike(nextFrame: Option[Frame]) extends LookaheadFrame {
  val first = Roll(10)
  val score: Int = 10 + nextFrame.map(_.score).getOrElse(0)
}
case class Spare(first: Roll, second: Roll, nextFrame: Option[Frame]) extends LookaheadFrame {
  val score = 10 + nextFrame.map(_.first).map(_.count).getOrElse(0)
}
