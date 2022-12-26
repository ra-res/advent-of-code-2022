import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile

@main def main() = {
  // val lines = Source.fromFile("small.txt").getLines.toList.map(_.split(" "))
  val lines = Source.fromFile("input.txt").getLines.toList.map(_.split(" "))

   val cycles = lines.foldLeft(Array((1, 1)))((acc, instruction) => {
      val lastCycle = acc.last
      val next = instruction match {
          case Array("noop") => Array((lastCycle(0), lastCycle(1) + 1))
          case Array("addx", addxval) => {
             Array((lastCycle(0), lastCycle(1) + 1), (lastCycle(0) + addxval.toInt, lastCycle(1) + 2))
          }
      }
      acc ++ next
  })

  val targets = List(20, 60, 100, 140, 180, 220)
  println(cycles.filter((_, cycle) => targets.contains(cycle)).map(_.toList.product).sum)
  println(cycles
      .map((x, cycle) => x)
      .grouped(40)
      .map(r =>
          r.zipWithIndex.map((x, i) => if ((x - 1 to x + 1).contains(i)) "#" else ".").mkString
      )
      .mkString("\n"))
}
