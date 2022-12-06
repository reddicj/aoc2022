package day06

@main def main =

  val testData = List(
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  )

  val testData2 = List(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  )

  def take(s: String, countOfDistinctChars: Int): Option[String] =

    def isStart(x: String): Boolean =
      x.length == countOfDistinctChars && x.distinct == x

    def run(remaining: String, acc: String): Option[String] =
      val x = remaining.take(countOfDistinctChars)
      if x.length < countOfDistinctChars then None
      else if isStart(x) then Some(acc + x)
      else run(remaining.drop(1), acc + x.take(1))

    run(s, "")

  def countOfCharsUntilStart(s: String, countOfDistinctChars: Int): Option[Int] =
    take(s, countOfDistinctChars).map(_.length)

  // testData.foreach(x => println(countOfCharsUntilStart(x, 4)))
  // testData2.foreach(x => println(countOfCharsUntilStart(x, 14)))
  println(countOfCharsUntilStart(Input.data, 4))
  println(countOfCharsUntilStart(Input.data, 14))
