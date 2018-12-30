package cx.cad.bowling


package object bowling {
  val EndOfGame = None
}

case class Roll(count: Int) extends AnyVal

import cx.cad.bowling.Game._
case class Game(rolls: Seq[Roll]) {
  def score: Int = rollsToFrames(rolls).foldLeft(0) { (cumulative, frame) => cumulative + frame.score}
}

object Game {
  def from(rolls: Array[Int]): Game = {
    Game(arrayToSeq(rolls))
  }

  def arrayToSeq(array: Array[Int]): Seq[Roll] = array.map(Roll)

  def rollsToFrames(remainingRolls: Seq[Roll]): List[Frame] = {
    remainingRolls.toList match {
      case Roll(10) :: cdr =>
        val next = rollsToFrames(cdr)
        Strike(next.headOption) :: next
      case first :: second :: cdr if first.count + second.count == 10 =>
        val next = rollsToFrames(cdr)
        Spare(first, second, next.headOption) :: next
      case first :: second :: cdr if first.count + second.count < 10 =>  Open(first, second) :: rollsToFrames(cdr)
      case _ => Nil
    }
  }
}

case class Score(points: Int) extends AnyVal
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
