package cx.cad.bowling

import cx.cad.bowling.Games._
import cx.cad.bowling.bowling.EndOfGame
import org.scalatest._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}

// Problem statement:
//Write a program that takes as input an array of integers representing a
// complete 10-pin bowling game, where each int represents the number of pins
// knocked down by a single roll, and outputs the score for the game. We write
// code primarily in Scala using the functional programming paradigm, but you
// are welcome to use the language and design strategy of your choice.

//Deliverables:
//Please include the code for the program in a git repository including all
// docs, tests and anything needed to build your code (e.g. a Makefile if you
// write it in C). Also include instructions on how to run it and tell us
// what the dependencies are if they need to be installed manually.
//Things we care about when looking at your code:
//  It solves the problem
//  Correctness / edge case handling
//  Code design / simplicity and clarity
//  Error handling
//  Test quality and expressiveness
//  Documentation
//
class FrameSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {

  val examples = Table[(Seq[Roll], List[Frame])]("rolls and frames",
    Seq(1,2).map(Roll) -> List(
      Open(
        Roll(1),
        Roll(2))),
    Seq(10).map(Roll) -> List(Strike(EndOfGame)),
    // TODO: ew, this could get nasty
    Seq(10,10).map(Roll) -> List(
      Strike(Some(Strike(EndOfGame))),
      Strike(EndOfGame)),
    // FIXME: got nasty
    Seq(1, 9, 5, 5, 0, 0).map(Roll) -> List(
      Spare(
        Roll(1),
        Roll(9),
        Some(Spare(
          Roll(5),
          Roll(5),
          Some(Open(
            Roll(0),
            Roll(0)))))),
      Spare(
        Roll(5),
        Roll(5),
        Some(Open(
          Roll(0),
          Roll(0)))),
      Open(
        Roll(0),
        Roll(0)))
  )

  val f =

  property("Frame generation should generate correct frames") {
    forAll(examples) { case(rollSeq, framesList) =>
      val frames = Game.rollsToFrames(rollSeq)
      frames should contain theSameElementsInOrderAs(framesList)
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
}
class GameSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val examples = Table[GameResult]("game results",
    Ones,
    TwoStrikes,
    ThreeStrikes,
    FourStrikes,
    FiveStrikes,
    Perfect,
    Spares,
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
    Strike(EndOfGame).score should be(10)
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
      Open(Roll(first), Roll(second)).score == first + second
    }
  }

  "A Strike frame" should "have a score of 10 plus the next frame" in {
    forAll(openFrames) { case (first, second) =>
      Strike(Option(Open(Roll(first), Roll(second)))).score == 10 + first + second
    }
  }

  "A Spare frame" should "have a score of 10 plus the first roll of the next frame" in {
    forAll(openFrames, spareFrames) { case ((openFirst, openSecond), (spareFirst, spareSecond)) =>
      val spare = Spare(
        Roll(spareFirst),
        Roll(spareSecond),
        Option(Open(Roll(openFirst), Roll(openSecond))))
      spare.score == 10 + openFirst
    }
  }
}

