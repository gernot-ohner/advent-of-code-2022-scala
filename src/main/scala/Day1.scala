package ohner.dev

object Day1 extends Solution[Int] {
  override def task1(input: List[String]): Int = {
    input.mkString(",")
      .split(",,")
      .map(elf => elf.split(",").map(_.toInt).sum)
      .max
  }

  override def task2(input: List[String]): Int = {
    input.mkString(",")
      .split(",,")
      .map(elf => elf.split(",").map(_.toInt).sum)
      .sorted(Ordering.Int.reverse)
      .take(3)
      .sum
  }
}
