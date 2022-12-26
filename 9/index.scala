import scala.io.Source
import scala.collection.mutable.ListBuffer

def tailPath(length: Int, headPath: List[(Int, Int)]): List[(Int, Int)] =
    (1 until length).foldLeft(headPath)((path, _) => moveTail(path))

def moveTail(headPath: List[(Int, Int)]): List[(Int, Int)] = {
    headPath.scanLeft((0, 0))((tail, head) => {
        val delta = (head(0) - tail(0), head(1) - tail(1))
        val update = delta(0).abs > 1 || delta(1).abs > 1
        if (update) (tail(0) + delta(0).sign, tail(1) + delta(1).sign) else tail
    })
}

@main def main() = {
  // val lines = Source.fromFile("small.txt").getLines.toList
  val lines = Source.fromFile("input.txt").getLines.toList
  val re = """(\w+) (\d+)""".r
  val headPath = lines.flatMap{
     case re(ins, steps) => {
        (0 until steps.toInt).map(_ => ins).toList
     }}.scanLeft((0,0))((head, ins) => {
       ins match {
          case "D" => (head(0) + 1, head(1))
          case "U" => (head(0) - 1, head(1))
          case "R" => (head(0), head(1) + 1)
          case "L" => (head(0), head(1) - 1)
       }
  })
  println(tailPath(2, headPath).toSet.size) // 6339
  println(tailPath(10, headPath).toSet.size) // 2541
}
