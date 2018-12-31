package cx.cad.bowling

import cx.cad.bowling.Games._
import org.scalacheck._
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}

class FrameSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {

  private val examples = Table[(Seq[Roll], List[Frame])]("rolls and frames",
    Seq(1,2).map(Roll) -> List(
      Open(
        Roll(1),
        Roll(2))),
    Seq(10).map(Roll) -> List(Strike()),
    // TODO: still nasty
    Seq(10,10).map(Roll) -> List(
      Strike(Option(Roll(10))),
      Strike()),
    // FIXME: got less nasty, still not fun
    Seq(1, 9, 5, 5, 0, 0).map(Roll) -> List(
      Spare(
        Roll(1),
        Roll(9),
        Some(Roll(5))),
      Spare(
        Roll(5),
        Roll(5),
        Some(Roll(0))),
      Open(
        Roll(0),
        Roll(0)))
  )

  property("Frame generation should generate correct frames") {
    forAll(examples) { case(rollSeq, framesList) =>
      val frames = Game.rollsToFrames(rollSeq)
      frames should contain theSameElementsInOrderAs framesList
    }
  }
}

case class GameResult(score: Int, rolls: Array[Int])

object Games {
  val Perfect = GameResult(300, Array.fill(12)(10))
  val TwoStrikes = GameResult(30, Array.fill(2)(10))
  val ThreeStrikes = GameResult(60, Array.fill(3)(10))
  val FourStrikes = GameResult(90, Array.fill(4)(10))
  val FiveStrikes = GameResult(120, Array.fill(5)(10))
  val Spares = GameResult(40, Array.fill(6)(5))
  val Ones = GameResult(20, Array.fill(20)(1))
  val Zeros = GameResult(0, Array.fill(20)(0))
  val TypicalGame = GameResult(143, Array(
    7,1,2,4,7,3,10,10,8,0,4,1,10,5,5,0,10,10
  ))
  val Botch = GameResult(187, Array(
    9,1,8,2,9,1,9,1,9,1,9,1,9,1,9,1,9,1,10,3,3
  ))
}
class GameSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  private val examples = Table[GameResult]("game results",
    Ones,
    Zeros,
    TwoStrikes,
    ThreeStrikes,
    FourStrikes,
    FiveStrikes,
    Perfect,
    Spares,
    TypicalGame,
    Botch,
  )

  property("Known games should be scored correctly") {
    forAll(examples) { result =>
      info(s"${result.rolls.toList} should have score ${result.score}")
      val game = Game.from(result.rolls)
      game.score should be(result.score)
    }
  }
}


class ScoringSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "A strike" should "have score of 10 without a next frame" in {
    Strike().score.points should be(10)
  }

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(minSize = 10, maxSize = 20, minSuccessful = 5, maxDiscarded = 5000)

  val validCounts: Gen[Int] = Gen.choose(0,10)

  val twoRolls: Gen[(Int, Int)] = for {
    first <- validCounts
    second <- validCounts
  } yield (first, second)

  val openFrames: Gen[(Int, Int)] = twoRolls suchThat { case(f,s) => f+s < 10 }
  val spareFrames: Gen[(Int, Int)] = twoRolls suchThat { case(f,s) => f+s == 10 }

  "An Open frame" should "have a simple score" in {
    forAll(openFrames) { case (first, second) =>
      Open(Roll(first), Roll(second)).score.points == first + second
    }
  }

  "A Strike frame" should "have a score of 10 plus the next frame" in {
    forAll(openFrames) { case (first, second) =>
      Strike(
        Option(Roll(first)),
        Option(Roll(second)))
        .score.points == 10 + first + second
    }
  }

  "A Spare frame" should "have a score of 10 plus the first roll of the next frame" in {
    forAll(openFrames, spareFrames) { case ((openFirst, _), (spareFirst, spareSecond)) =>
      val spare = Spare(
        Roll(spareFirst),
        Roll(spareSecond),
        Option(Roll(openFirst)))
      spare.score.points == 10 + openFirst
    }
  }
}

