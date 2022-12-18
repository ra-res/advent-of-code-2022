import scala.io.Source

def calculatescore(a: Int, b: Int): Int = {
    if (a==b) return 3
    if ((a == 1 && b == 3)
        || (a == 3 && b == 2)
        || (a == 2 && b == 1)
    ) return 0
    6
}

def roundscore(a: String, b: String): Int = {
    val ascore =  scorebymove.get(a).get
    var bscore = ascore
    if (b == "X") {
        bscore = scorebymove.get(losemove.get(a).get).get
    } else if (b == "Z") {
        bscore = scorebymove.get(winmove.get(a).get).get
    }
    calculatescore(ascore, bscore) + bscore
}

val winmove = Map( "A" -> "B", "B" -> "C", "C" -> "A")
val losemove = Map( "B" -> "A", "C" -> "B", "A" -> "C")
val scorebymove = Map( "A" -> 1, "B" -> 2, "C" -> 3)

@main def main() = {
    // val lines: Array[String] = Source.fromFile("small.txt").getLines.toArray
    val lines: Array[String] = Source.fromFile("input.txt").getLines.toArray
    
    println(lines.map(line => { 
        val moves = line.split(" ")
        roundscore(moves(0), moves(1))
    }).sum)
}