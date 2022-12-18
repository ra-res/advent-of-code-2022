import scala.io.Source
import Array._

@main def main() = {
    // val lines: List[String] = Source.fromFile("small.txt").getLines.toList
    val lines: List[String] = Source.fromFile("input.txt").getLines.toList


    def overlap(nums: List[Int]) = {
        val arr1 = range(nums(0), nums(1) + 1).toList
        val arr2 = range(nums(2), nums(3) + 1).toList
        (arr1 intersect arr2).toList.length > 0
    }

    def contains(nums: List[Int]) = {
        val lb = nums(0).min(nums(2))
        val ub = nums(1).max(nums(3))
        (lb == nums(0) && ub == nums(1)) || (lb == nums(2) && ub == nums(3))
    }
    
    val containsCount = lines.map(_.split(",").toList)
                    .map(_.map(_.split("-").map(_.toInt).toList).flatten)
                    .map(_.toList)
                    // .map(contains) // part 1
                    .map(overlap) // part 2
                    .filter(_ == true)
                    .length

    println(containsCount)
}