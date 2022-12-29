import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer 
import scala.io.Source.fromFile
import scala.BigInt

case class Monkey(items: ArrayBuffer[BigInt], operation: List[String], tests: List[Int]) {
    var inspectionsCount = 0
    def inspect(lcm: BigInt, divideBy: Int): List[(BigInt, Int)] = {
        if (!items.isEmpty) {
            val moveIns = new ListBuffer[(BigInt, Int)]()
            inspectionsCount = inspectionsCount + this.items.length
            (0 until this.items.length).foreach(_ => {
                val old = items.remove(0)
                val worry = worry(old, divideBy) % lcm
                val dst = test(worry)
                moveIns.addOne((worry, dst))
            })
            return moveIns.toList
        }
        List((-1, -1))
    }

    def test(item: BigInt): Int = {
        if (item % tests(0) == 0) { tests(1) } else { tests(2) }
    }

    def worry(old: BigInt, divideBy: Int): BigInt = {
        operation match {
            case List("*", "old") => (old * old) / divideBy
            case List("*", op) => (old * BigInt(op)) / divideBy
            case List("+", op) => (old + BigInt(op)) / divideBy
        }
    }

    def getPrime: Int = {
        tests(0)
    }

    def getInspectionsCount = {
        this.inspectionsCount
    }

    def addItem(item: BigInt) = {
        this.items.addOne(item)
    }
}

@main def main() = {
//   val lines = Source.fromFile("small.txt").mkString.split("\n\n").toList
  val lines = Source.fromFile("input.txt").mkString.split("\n\n").toList

  def parseItems(ins: String): ArrayBuffer[BigInt] = {
    ArrayBuffer(ins
        .split(" ")
        .drop(2)
        .map(BigInt(_))
        : _*)
  }

  def parseOperation(ins: String): List[String] = {
    ins.split(" ").toList.takeRight(2)
  }

  def parseTests(ins: List[String]): List[Int] = {
    List(ins(0).split(" ").takeRight(1)(0).toInt, // prime
        ins(1).split(" ").takeRight(1)(0).toInt, // case true 
        ins(2).split(" ").takeRight(1)(0).toInt) // case false
  }

  val monkeys: List[Monkey] = lines.map(line => {
    val ins = line.replaceAllLiterally("  ", "")
        .replaceAllLiterally(",", "")
        .split("\n")
        .toList
    val items = parseItems(ins(1))
    val operation = parseOperation(ins(2))
    val tests = parseTests(ins.takeRight(3))
    new Monkey(items, operation, tests);
  }).toList

  val lcm = monkeys.map(_.getPrime).product
  val divideBy = 1
  val rounds = 10000
  (0 until rounds).foreach(_ => {
    monkeys.foreach(monkey => {
        val results = monkey.inspect(lcm, divideBy)
        if (results != List((-1, -1))) {
            results.foreach(result => {
                monkeys(result(1)).addItem(result(0))
            })
        }
    })
  })

  println(monkeys.map(m => m.inspectionsCount).sorted.takeRight(2).map(BigInt(_)).reduce(_ * _)) // 20683044837
}
