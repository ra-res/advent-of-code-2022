import scala.io.Source

case class Directory(name: String, parent: Option[Directory]) {
  var files: scala.collection.mutable.ListBuffer[File] = scala.collection.mutable.ListBuffer[File]()
  var subDirectories: scala.collection.mutable.ListBuffer[Directory] = scala.collection.mutable.ListBuffer[Directory]()

  def addFile(file: File): Unit = {
    this.files.addOne(file)
  }

  def addSubdirectory(dir: Directory): Unit = {
    this.subDirectories.addOne(dir)
  }

  def getSize: Long = {
    files.map(_.getSize).sum + subDirectories.map(_.getSize).sum
  }

  def tree: Unit = {
    println(name)
    println(files.map(_.str).toString())
    println(subDirectories.map(_.tree))
  }

  def getSubDirectory(dirName: String): Directory = { 
    val dir: Option[Directory] = subDirectories.find(_.getName == dirName)
    if (dir.isDefined) {dir.get} else {Directory(dirName, Some(this))}
  }

  def getName: String= {
    name 
  }
}

case class File(name: String, size: Long, parent: Directory) {
  def str: String = {
    (name, size, parent.getName).toString()
  }

  def getName: String= {
    name 
  }

  def getSize: Long = {
    size
  }
}

@main def main() = {
  val lines = Source.fromFile("input.txt").mkString.split("\n").toList
  // val lines = Source.fromFile("small.txt").mkString.split("\n").toList
  val dirs: scala.collection.mutable.ListBuffer[Directory] = scala.collection.mutable.ListBuffer[Directory]()
  val root: Directory = Directory("/", None)
  dirs.addOne(root)
  var cwd: Directory = root

  lines.foreach(line => {
    val substrings = line.split(" ").toList
    if (substrings(0) == "$") {
      if (substrings(1) == "cd") { // cd command
        if (substrings(2) == "/") {
          cwd = root
        } else if (substrings(2) == "..") {
           cwd = if (cwd.parent.isDefined) { cwd.parent.get } else { root }
        } else { // cd dir
          val n: Directory = cwd.getSubDirectory(substrings(2))
          dirs.addOne(n)
          cwd = n
        }
      } // ignore ls command
    } else {
      if (substrings(0) == "dir") { 
        cwd.addSubdirectory(Directory(substrings(1), Some(cwd)))
      } else { // is file
        cwd.addFile(File(substrings(1), substrings(0).toLong, cwd))
      }
    }
  })

  val part1 = dirs.map(x => (x.getName, x.getSize)).filter(_._2 < 100000).map(_._2).sum
  val neededSpace = 30000000 - (70000000 - root.getSize)
  val part2 = dirs.filter(_.getSize > neededSpace).sortBy(_.getSize).toList(0).getSize // 24933642
  println(part2)
}