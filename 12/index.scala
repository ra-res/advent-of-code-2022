import scala.io.Source
import scala.io.Source.fromFile
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set

case class DecoratedNode(node: (Int, Int), parent: Option[DecoratedNode], cost: Int) {
    def getNode: (Int, Int) = { node }
    def x: Int = { node(0) }
    def y: Int = { node(1) }
    def c: Int = { cost }
    def getParent: Option[DecoratedNode] = { parent }
}

@main def main() = {
//   var lines = Source.fromFile("small.txt").getLines.toList.map(_.toList)
  val lines = Source.fromFile("input.txt").getLines.toList.map(_.toList)
  var start: (Int, Int) = (0,0)
  var dst: (Int, Int) = (0,0)
  for (i <- 0 until lines.length) {
    for (j <- 0 until lines(0).length) {
        if (lines(i)(j) == 'S') {
            start = (i, j)
        } else if (lines(i)(j) == 'E') {
            dst = (i, j)
        }
    }
  }

  def inBounds(node: (Int, Int)): Boolean = {
    node(0) > -1 && node(0) < lines.length && node(1) > -1 && node(1) < lines(0).length
  }

  def adjancent(node: (Int, Int)): List[(Int, Int)] = {
    List((node(0),node(1) + 1),(node(0),node(1) - 1),(node(0) + 1,node(1)),(node(0) - 1,node(1))).filter(inBounds)
  }

  def signal(node: (Int, Int)): Char = {
    val (i, j) = node
    if (lines(i)(j) == 'S') {
        return 'a'
    } else if (lines(i)(j) == 'E') {
        return 'z'
    }
    lines(i)(j)
  }

  def dijkstra(start: (Int, Int), dst: (Int, Int)): Int = {
    val queue = collection.mutable.PriorityQueue[DecoratedNode]()(Ordering.by[DecoratedNode, Int](_.cost)).reverse
    queue.enqueue(new DecoratedNode(start, None, 0))
    val explored: ListBuffer[(Int, Int)] = ListBuffer[(Int, Int)]()
    var solutionFound = false
    while (queue.nonEmpty && !solutionFound) {
        val decoratedNode = queue.dequeue()
        val (i, j) = decoratedNode.node
        if (lines(i)(j) == 'E'){
            solutionFound = true
            return decoratedNode.cost
        }
        explored.addOne(decoratedNode.node)
        adjancent(decoratedNode.getNode).foreach(node => {
            if (!explored.contains(node) && signal(node) - signal(decoratedNode.getNode) <= 1) {
                queue.enqueue(new DecoratedNode(node, Some(decoratedNode), decoratedNode.cost + 1))
                explored.addOne(node)
            }
        })
    }
    -1
  }


  val starts = lines.zipWithIndex
                .flatMap((r, i) => r.zipWithIndex.map((height, j) => (i, j, height)))
                .filter((_, _, h) => h == 'a')  
                .map((i, j, h) => (i,j))
  println(dijkstra(start, dst)) // 472
  println(starts.map(dijkstra(_, dst)).filter(_ != -1).sorted.take(1)(0)) // 465
}

