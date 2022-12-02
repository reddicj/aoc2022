package day01

@main def main =

  val data: List[List[Int]] =
    Input.data.trim
      .split("\n\n")
      .toList
      .map(
        _.trim
          .split("\n")
          .toList
          .map(_.toInt)
      )

  val max: Int = data.map(_.sum).max
  println(s"Max: $max")

  val topThreeTotal: Int = data.map(_.sum).sorted.takeRight(3).sum
  println(s"Top 3 total: $topThreeTotal")
