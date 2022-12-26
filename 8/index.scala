import scala.io.Source

@main def main() = {
  // val lines = Source.fromFile("small.txt").getLines.toList.map(_.toList.map(_.asDigit))
  val lines = Source.fromFile("input.txt").getLines.toList.map(_.toList.map(_.asDigit))
  var trees = (lines.length + lines(0).length - 2)  * 2

  def neighbours(i: Int, j: Int): List[List[Int]] = {
    val up = (0 until i).map(x => lines(x)(j)).toList.reverse
    val down = (i + 1 until lines(0).length).map(x => lines(x)(j)).toList
    val left = (0 until j).map(x => lines(i)(x)).toList.reverse
    val right = (j + 1 until lines.length).map(x => lines(i)(x)).toList
    List(up, down, left, right)
  }

  var scenicScore = 0
  for (i <- 1 until lines.length - 1) {
    for (j <- 1 until lines(0).length - 1) {
      val neighbouringTrees = neighbours(i, j)

      if (!neighbouringTrees
        .map(_.max)
        .filter(_ < lines(i)(j))
        .toList
        .isEmpty) {
           trees = trees + 1 
       }

      scenicScore = scenicScore.max(neighbouringTrees.map(_.indexWhere(_ >= lines(i)(j))).zipWithIndex.map{ 
        case (indexOfTallestTree, index) => {
          if (indexOfTallestTree == -1) {
            neighbouringTrees(index).length
          } else {
            indexOfTallestTree + 1
          }
        }
      }.product)
    }
  }

  println(trees) // 1763
  println(scenicScore) // 671160
}