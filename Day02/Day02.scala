package day02

enum RPS(val points: Int):
  case Rock     extends RPS(1)
  case Paper    extends RPS(2)
  case Scissors extends RPS(3)

  def score(other: RPS): Score =
    (this, other) match
      case (Rock, Paper)     => Score.Loss
      case (Rock, Scissors)  => Score.Win
      case (Paper, Rock)     => Score.Win
      case (Paper, Scissors) => Score.Loss
      case (Scissors, Rock)  => Score.Loss
      case (Scissors, Paper) => Score.Win
      case _                 => Score.Draw

  def getOtherForScore(result: Score): RPS =
    (this, result) match
      case (Rock, Score.Win)      => Paper
      case (Rock, Score.Loss)     => Scissors
      case (Paper, Score.Win)     => Scissors
      case (Paper, Score.Loss)    => Rock
      case (Scissors, Score.Win)  => Rock
      case (Scissors, Score.Loss) => Paper
      case (x, Score.Draw)        => x

object RPS:
  def fromData(s: String): Option[RPS] =
    s match
      case "A" | "X" => Some(Rock)
      case "B" | "Y" => Some(Paper)
      case "C" | "Z" => Some(Scissors)
      case _         => None

enum Score(val points: Int):
  case Win  extends Score(6)
  case Draw extends Score(3)
  case Loss extends Score(0)

object Score:
  def fromData(s: String): Option[Score] =
    s match
      case "X" => Some(Loss)
      case "Y" => Some(Draw)
      case "Z" => Some(Win)
      case _   => None

@main def main =

  val testData: List[(String, String)] =
    List(
      "A" -> "Y",
      "B" -> "X",
      "C" -> "Z"
    )

  val inputData: List[(String, String)] =

    def readRow(row: String): Option[(String, String)] =
      row.split(" ").toList match
        case h1 :: h2 :: _ => Some(h1 -> h2)
        case _             => None

    val rows: List[String] = Input.data.trim.split("\n").toList.map(_.trim)
    rows.map(readRow(_).get)

  def score(round: (String, String)): Int =
    val (col1, col2)     = round
    val opponentStrategy = RPS.fromData(col1).get
    val myStrategy       = RPS.fromData(col2).get
    myStrategy.points + myStrategy.score(opponentStrategy).points

  def score2(round: (String, String)): Int =
    val (col1, col2)     = round
    val opponentStrategy = RPS.fromData(col1).get
    val targetScore      = Score.fromData(col2).get
    val myStrategy       = opponentStrategy.getOtherForScore(targetScore)
    myStrategy.points + targetScore.points

  def totalScore(data: List[(String, String)]): Int  = data.map(score).sum
  def totalScore2(data: List[(String, String)]): Int = data.map(score2).sum

  println(s"Total score = ${totalScore(inputData)}")
  println(s"Total score 2 = ${totalScore2(inputData)}")
