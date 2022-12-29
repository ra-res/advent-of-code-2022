import scala.io.Source
import scala.io.Source.fromFile
import scala.collection.mutable.{ ListBuffer, Set }

@main def main() = {
  // var lines = Source.fromFile("small.txt").getLines.toList.map(_.replaceAllLiterally(" " ,"")).map(_.split("->").toList.map(_.split(",").map(_.toInt).toList.reverse))
  var lines = Source.fromFile("input.txt").getLines.toList.map(_.replaceAllLiterally(" " ,"")).map(_.split("->").toList.map(_.split(",").map(_.toInt).toList.reverse))
  val columnLength = lines.flatten.map(_(1)).max * 2
  val rowLength = lines.flatten.map(_(0)).max + 2 
  var cave = ListBuffer[ListBuffer[Char]]()
  var gap = (0,500)

  (0 until rowLength).foreach(i => {
    cave.addOne(ListBuffer[Char]())
    (0 until columnLength).foreach(i => {
      cave.last.addOne('.')
    })
  })

  def draw(p1: List[Int], p2: List[Int]): Set[(Int, Int)] = {
    var occupied = Set[(Int, Int)]()
    if (p1(0) == p2(0)) {
      val (min, max)= (math.min(p1(1), p2(1)), math.max(p1(1), p2(1)))
      for (j <- min to max) {
        occupied += ((p1(0), j))
        cave(p1(0))(j) = '#'
      }
    }
    if (p1(1) == p2(1)) {
      val (min, max) = (math.min(p1(0), p2(0)), math.max(p1(0), p2(0)))
      for (i <- min to max) {
        occupied += ((i, p1(1)))
        cave(i)(p1(1)) = '#'
      }
    } else {
      val (min, max) = (math.min(p1(0), p2(0)), math.max(p1(0), p2(0))) 
      for (i <- min to max) {
        val (min2, max2) = (math.min(p1(1), p2(1)), math.max(p1(1), p2(1)))
        for (j <- min2 to max2) {
          occupied += ((i,j))
          cave(i)(j) = '#'
        }
      }
    }
    occupied
  }

  var occupied = Set[(Int, Int)]()
  (0 until lines.length).foreach(i => {
    (0 until lines(i).length - 1).foreach(j => {
      draw(lines(i)(j), lines(i)(j + 1)).map(i => {
        occupied += i
      })
    })
  })
  cave(gap(0))(gap(1)) = '+'

  def options(curr: (Int, Int)): List[(Int, Int)] = {
    val (i, j) = curr
    List(
      (i + 1, j), // down
      (i + 1, j - 1), // down - left
      (i + 1, j + 1), // down - right
    )
  }

  def inBounds(option: (Int, Int)): Boolean = {
    option(0) > -1 && option(0) < cave.length && option(1) > -1 && option(1) < cave(0).length
  }

  def isOverflowing(option: (Int, Int)): Boolean = {
    (option(0) == cave.length) || (option(1) == cave(0).length) || (option(0) == -1) || (option(1) == -1)
  }

  def dropSand(currPos: (Int, Int), round: Int): Unit = {
    val valid: List[(Int, Int)] = options(currPos)
    val unoccupied: List[(Int, Int)] = valid.filter(i => !occupied.contains(i))
    val inbounds: List[(Int, Int)] = unoccupied.filter(inBounds)
    // if (!unoccupied.filter(isOverflowing).isEmpty) { // PART 1
    //   println(round)
    // } else 
    if (currPos == gap && inbounds.isEmpty)  { // PART 2
      println(round)
    } else if (!inbounds.isEmpty) {
      dropSand(unoccupied.head, round)
    } else {
      val (i, j): (Int, Int) = currPos
      cave(i)(j) = 'o'
      occupied += (i, j)
    }
  }

  for (round <- 0 to 23500) {
    dropSand(gap, round)
  }
  // println(cave.map(_.mkString).mkString("\n"))
}