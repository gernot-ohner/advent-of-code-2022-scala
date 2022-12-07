package ohner.dev

object Day6 extends Solution[Int] {
  override def task1(input: List[String]): Int = findPacket(input.mkString, 4)

  override def task2(input: List[String]): Int = findPacket(input.mkString, 14)

  private def findPacket(input: String, packetSize: Int): Int = {
    val intermediateResult = input.sliding(packetSize, 1).find(s => s.toSet.size == packetSize)
    input.indexOf(intermediateResult.get) + packetSize;
  }

}
