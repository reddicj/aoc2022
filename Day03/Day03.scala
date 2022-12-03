package day03

@main def main =

  val testData: List[String] = List(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  )

  val inputData: List[String] =
    Input.data.trim.split("\n").toList.map(_.trim)

  val priorities: Map[Char, Int] =
    (('a' to 'z').toList ::: ('A' to 'Z').toList).zipWithIndex.toMap.mapValues(_ + 1).toMap

  def split(s: String): (String, String) =
    s.splitAt(s.length / 2)

  def findDuplicate(s1: String, s2: String): Char =
    s1.intersect(s2).head

  def findItemType(s1: String, s2: String, s3: String): Char =
    s1.intersect(s2).intersect(s3).head

  def sumOfPriorities(data: List[String]): Int =
    data.map(split).map(findDuplicate).map(priorities).sum

  println(s"Sum of priorities = ${sumOfPriorities(inputData)}")

  def itemTypes(data: List[String]): List[Char] = data
    .grouped(3)
    .collect { case s1 :: s2 :: s3 :: Nil => findItemType(s1, s2, s3) }
    .toList

  println(s"Sum of priorities 2 = ${itemTypes(inputData).map(priorities).sum}")
