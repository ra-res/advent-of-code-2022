import scala.io.Source
import scala.io.Source.fromFile
import scala.collection.mutable.ListBuffer

@main def main() = {
  var lines = Source.fromFile("small.txt").mkString.split("\n\n").toList.map(_.split("\n").toList)
  // val lines = Source.fromFile("input.txt").getLines.toList
  println(lines)
}