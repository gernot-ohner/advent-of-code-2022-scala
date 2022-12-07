package ohner.dev

import scala.collection.mutable.ListBuffer

object Day7 extends Solution[Int] {
  override def task1(input: List[String]): Int = traverseTreePart1(buildTree(input))

  private def buildTree(input: List[String]): Tree = {
    val dirRegex = """dir (\w+)""".r
    val dirChangeRegex = """\$ cd (\w+)""".r
    val fileRegex = """(\d+) ([\w.]+)""".r
    val root = Directory(ListBuffer(), null)
    var currentNode = root
    input.foreach(line => {
      line match
        case "$ cd /" => currentNode = root
        case "$ ls" => print("")
        case fileRegex(fileSize, _) =>
          val leaf = Leaf(fileSize.toInt, currentNode)
          currentNode.children.addOne(leaf)
        case "$ cd .." => currentNode = currentNode.parent
        case dirChangeRegex(_) =>
          val newNode = Directory(ListBuffer(), currentNode)
          currentNode.children.addOne(newNode)
          currentNode = newNode
        case dirRegex(_) => print("")
    })
    root
  }

  private def traverseTreePart1(tree: Tree): Int = {

    var total = 0

    def rec(tree: Tree): Int = tree match
      case Leaf(size, _) => size
      case Directory(children, _) =>
        val size = children.map(rec).sum
        if (size < 100_000) total = total + size
        size

    rec(tree)
    total
  }

  override def task2(input: List[String]): Int = {
    val tree = buildTree(input)
    val totalUnused = 70_000_000 - findTotalUsedSize(tree)
    val necessarySizeToFree = 30_000_000 - totalUnused
    findSmallestFolderLargerThan(tree, necessarySizeToFree)
  }

  private def findTotalUsedSize(tree: Tree): Int = {
    def rec(tree: Tree): Int = tree match
      case Leaf(size, _) => size
      case Directory(children, _) => children.map(rec).sum

    rec(tree)
  }

  private def findSmallestFolderLargerThan(tree: Tree, minSize: Int) = {
    var smallestDirectorySize = Integer.MAX_VALUE

    def rec(tree: Tree): Int = tree match
      case Leaf(size, _) => size
      case Directory(children, _) =>
        val directorySize = children.map(rec).sum
        if (directorySize >= minSize && directorySize < smallestDirectorySize) {
          smallestDirectorySize = directorySize
        }
        directorySize

    rec(tree)
    smallestDirectorySize
  }
}

sealed trait Tree

case class Leaf(size: Int, parent: Directory) extends Tree {
  override def toString: String = s"(size: $size)"
}

case class Directory(children: ListBuffer[Tree], parent: Directory) extends Tree {
  override def toString: String = s"children: ${children.mkString}"
}
