import scala.io.Source

@main def main() = {
  // val lines = Source.fromFile("small.txt").mkString.toList
  val lines = Source.fromFile("input.txt").mkString.toList
  val groupSize = 14
  println(lines.sliding(groupSize).indexWhere(_.distinct.length == groupSize) + groupSize) 
}