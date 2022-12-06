package day05

enum CraneType:
  case CrateMover9000, CrateMover9001

final case class Movement(quantity: Int, from: Int, to: Int)

object Movement:

  private val pattern = raw"move (\d+) from (\d+) to (\d+)".r

  def read(data: String): List[Movement] =
    def readLine(s: String): Movement =
      s.trim match
        case pattern(quantity, from, to) => Movement(quantity.toInt, from.toInt, to.toInt)
        case _                           => throw new RuntimeException(s"Error reading: $s")
    data.trim.split("\n").map(readLine).toList

  def move(m: Movement, from: List[Char], to: List[Char], crane: CraneType): (List[Char], List[Char]) = (
    from.drop(m.quantity),
    crane match
      case CraneType.CrateMover9000 => from.take(m.quantity).reverse ::: to
      case CraneType.CrateMover9001 => from.take(m.quantity) ::: to
  )

extension (stacks: List[List[Char]])

  private def move(m: Movement, crane: CraneType): List[List[Char]] =
    val map: Map[Int, List[Char]] = stacks.zipWithIndex.map((stack, i) => i + 1 -> stack).toMap
    (for
      from        <- map.get(m.from)
      to          <- map.get(m.to)
      (from2, to2) = Movement.move(m, from, to, crane)
      updatedMap   = map ++ List(m.from -> from2, m.to -> to2)
    yield updatedMap.toList.sortBy(_._1).map(_._2)).getOrElse(stacks)

  def move(movements: List[Movement], crane: CraneType): List[List[Char]] =
    movements.foldLeft(stacks)(_.move(_, crane))

@main def main =

  def run(positionData: List[List[Char]], movementData: String, crane: CraneType): String =
    val movements: List[Movement] = Movement.read(movementData)
    positionData.move(movements, crane).flatMap(_.headOption).mkString

  println(run(TestData.positionData, TestData.movementData, CraneType.CrateMover9000))
  println(run(Input.positionData, Input.movementData, CraneType.CrateMover9000))
  println(run(TestData.positionData, TestData.movementData, CraneType.CrateMover9001))
  println(run(Input.positionData, Input.movementData, CraneType.CrateMover9001))
