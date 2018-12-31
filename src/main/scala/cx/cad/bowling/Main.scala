package cx.cad.bowling

import scala.io.StdIn

object Main extends App {
  println("List the count of the rolls, separated by a space. No error handling for non-integer entries or numbers greater than 10.")
  print("> ")
  val rolls = StdIn.readLine().split(' ').map(_.toInt)
  println(s"Score: ${Game.from(rolls).score}")
}
