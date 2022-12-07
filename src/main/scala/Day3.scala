package ohner.dev

object Day3 extends Solution[Int] {

  def task1(input: List[String]): Int =
    input.map(line => {
      val (firstHalf, secondHalf) = line.splitAt(line.length / 2)
      val itemsInFirstHalf = firstHalf.toSet
      secondHalf.find(itemsInFirstHalf.contains).get
    })
      .map(toPriority)
      .sum

  def task2(input: List[String]): Int = {
    input.sliding(3, 3).map((lines) => {
      val itemsInBag1 = lines.head.toSet
      val itemsInBag2 = lines(1).toSet
      lines(2).find(char => itemsInBag1.contains(char) && itemsInBag2.contains(char)).get
    }).map(toPriority).sum
  }

  private def toPriority(char: Char) =
    if char.isUpper then char - 'A' + 27 else char - 'a' + 1
}
