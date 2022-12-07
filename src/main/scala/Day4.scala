package ohner.dev

object Day4 extends Solution[Int]:

  override def task1(input: List[String]): Int =
    input.map(IntervalPair.fromString).count(_.haveCompleteOverlap)

  override def task2(input: List[String]): Int =
    input.map(IntervalPair.fromString).count(_.haveOverlap)



case class IntervalPair(i1: Interval, i2: Interval):
  def haveCompleteOverlap: Boolean =
    (i1.start <= i2.start && i1.end >= i2.end) ||
    (i2.start <= i1.start && i2.end >= i1.end)

  def haveOverlap: Boolean =
    (i1.start >= i2.start && i1.start <= i2.end) ||
    (i2.start >= i1.start && i2.start <= i1.end)

case object IntervalPair:
  private val regex = """(\d+)-(\d+),(\d+)-(\d+)""".r

  def fromString(s: String): IntervalPair = s match
    case regex(start1, end1, start2, end2) =>
      IntervalPair(Interval(start1.toInt, end1.toInt), Interval(start2.toInt, end2.toInt))



case class Interval(start: Int, end: Int)
