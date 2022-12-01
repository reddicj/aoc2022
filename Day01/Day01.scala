package day01

import scala.annotation.tailrec
import scala.util.Try

@main def main =

  val data: List[List[Int]] = read(Input.data)

  val max: Int = data.map(_.sum).max
  println(s"Max: $max")

  val topThreeTotal: Int = data.map(_.sum).sorted.takeRight(3).sum
  println(s"Top 3 total: $topThreeTotal")

end main

def read(input: String): List[List[Int]] =
  group(input.trim.split("\n").toList, "").map(_.map(_.toInt))

def group[A](as: List[A], delimiter: A): List[List[A]] =
  @tailrec
  def run(remaining: List[A], acc: List[List[A]]): List[List[A]] =
    if remaining.isEmpty then acc
    else
      val g      = remaining.takeWhile(_ != delimiter)
      val update = if g.nonEmpty then g :: acc else acc
      run(remaining.drop(g.size + 1), update)
  run(as, Nil).reverse
