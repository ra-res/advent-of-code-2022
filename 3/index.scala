import scala.io.Source

@main def main() = {
    // val lines: List[String] = Source.fromFile("small.txt").getLines.toList
    val lines: List[String] = Source.fromFile("input.txt").getLines.toList

    def getAscii(char: Char): Int = {
        val int = char.toInt
        var d = 65 - 27
        if (int > 96) d = 96
        int - d
    }

    val sumOfDuplicateLetters = lines.map(_.toList.map(getAscii))
            .map(ints => { 
                val pivot = ints.length / 2
                val l = ints.sliding(pivot, pivot).toList
                (l(0) intersect l(1)).toSet.toList
            })
            .flatten
            .sum
    println(sumOfDuplicateLetters)

    var sumOfBadges = lines.sliding(3, 3)
        .toList
        .map(_.map(_.toList.map(getAscii)))
        .map(group => {
           (group(0) intersect group(1) intersect group(2)).toSet.toList
        })
        .flatten
        .sum
   println(sumOfBadges)
}