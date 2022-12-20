import scala.io.Source

@main def main() = {
//   val lines = Source.fromFile("small.txt").mkString.split("\n\n")
  val lines = Source.fromFile("input.txt").mkString.split("\n\n")
  val lsts = lines(0).split("\n").reverse.toList.tail.map(_.tail.grouped(4).map(_.head).toList)
  val stacks = (0 until lsts(0).length).map(i => lsts.map(_(i)).filter(_ != ' ')).toArray
  // val stacks = (for (i <- 0 until lsts(0).length) yield lsts.map(_(i)).filter(_ != ' ')).toArray
  val re = """move (\d+) from (\d+) to (\d+)""".r
  val ins = lines(1).split("\n").map({ case re(amount, from, to) => (amount.toInt, from.toInt - 1, to.toInt - 1) }).toList

  ins.foreach(in => {
    val amount = in(0)
    val from = in(1)
    val to = in(2)
    stacks(to) = stacks(to) ++ stacks(from).takeRight(amount) // .reverse
    stacks(from) = stacks(from).dropRight(amount)
  })

  println(stacks.map(_.last).mkString)
}