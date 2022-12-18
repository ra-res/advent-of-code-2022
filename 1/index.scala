import Array._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.io.Source.fromFile
import scala.util.Using

@main def main() = {
    // val lines = Using(fromFile("small.txt"))(_.mkString).get
    val lines = Using(fromFile("input.txt"))(_.mkString).get
    val calories = lines.split("\n\n").map(_.split("\n").map(_.toInt))
    val top3 = calories.map(_.sum).sorted.reverse.take(3)
    println(top3(0)) // 69795
    println(top3.sum) // 208437
}