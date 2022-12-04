package day04

@main def main =

  val testData: List[String] = List(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  )

  val inputData: List[String] = Input.data.trim.split("\n").toList.map(_.trim)

  def readData(data: List[String]): List[(Range, Range)] =

    val row = raw"(\d+)-(\d+),(\d+)-(\d+)".r

    def readRow(s: String): Option[(Range, Range)] =
      s match
        case row(a, b, c, d) => Some((a.toInt to b.toInt) -> (c.toInt to d.toInt))
        case _               => None

    data.flatMap(readRow(_).toList)

  def countOfContainments(data: List[String]): Int =
    readData(data).count { case (r1, r2) =>
      r1.containsSlice(r2) || r2.containsSlice(r1)
    }

  def countOfOverlaps(data: List[String]): Int =
    readData(data).count { case (r1, r2) =>
      r1.intersect(r2).nonEmpty
    }

  println(s"Count of containments = ${countOfContainments(inputData)}")
  println(s"Count of overlaps = ${countOfOverlaps(inputData)}")
