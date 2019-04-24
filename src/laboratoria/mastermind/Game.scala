package laboratoria.mastermind

class Game {

  private final val in = io.StdIn

  /*
   * ABFF | xoo-
   * ABCDEF - colors
   * x -> correct, o -> among colors, - -> missed
   *
   */

  private var code: Seq[Int] = Seq()
  private var guessFeedback: Seq[Int] = Seq(0, 0, 0, 0)
  private var guess: Seq[Int] = Seq()

  def start(): Unit = {
    initialize()
  }

  private def initialize(): Unit = {
    def fillRandom(c: Int = 0, out: Seq[Int]= Seq()): Seq[Int] = c match {
      case 4 => out.reverse
      case _ => fillRandom(c+1, scala.util.Random.nextInt(6) +: out)
    }
    println("Your opponent is making up a code... ")
    code = fillRandom()
    println("Now you guess!")
    println("Insert your guess:")
    turn()
  }

  def turn(moves: Int = 0): Unit = {
    guessFeedback = Seq(0, 0, 0, 0)
    guess = convert(in.readLine())
    guessFeedback = increment(guessFeedback, guess.intersect(code).size)
    guessFeedback = increment(guessFeedback, countCorrect(guess, code))
    guessFeedback match {
      case Seq(2, 2, 2, 2) => end(true)
      case _ if moves + 1 > 10 =>
        println(convertToMove(moves, guess, guessFeedback))
        end(false)
      case _ =>
        println(convertToMove(moves, guess, guessFeedback))
        turn(moves + 1)
    }
  }

  private def convertToMove(moves: Int, guessed: Seq[Int], feedback: Seq[Int]): String = {
    def convertFeedback(feedback: Seq[Int], out: String = ""): String = feedback match {
      case Seq() => out
      case e +: s if e == 0 => convertFeedback(s, out + "-")
      case e +: s if e == 1 => convertFeedback(s, out + "o")
      case _ +: s => convertFeedback(s, out + "x")
    }
    moves + " : " +convertGuess(guess) + " | " + convertFeedback(feedback)
  }

  private def convertGuess(guessed: Seq[Int], out: String = ""): String = guessed match {
    case Seq() => out
    case e +: s => convertGuess(s, out + (e+65).toChar)
  }

  private def end(success: Boolean): Unit = {
    if(success) {
      println("Congratulations! You successfully guessed the code "+ convertGuess(guess) + "!")
    } else {
      println("Unfortunately you did nod succeed. The code was " + convertGuess(guess) +".")
      println("Good luck next time!")
    }
  }

  private def countCorrect(seq1: Seq[Int], seq2: Seq[Int], counter: Int = 0): Int = (seq1, seq2) match {
    case (Seq(), Seq()) => counter
    case (e1 +: s1, e2 +: s2) if e1 == e2 => countCorrect(s1, s2, counter+1)
    case (_ +: s1, _ +: s2) => countCorrect(s1, s2, counter)
  }

  private def increment(seq: Seq[Int], n: Int, out: Seq[Int] = Seq()): Seq[Int] = seq match {
    case s if n == 0 => (s ++ out).reverse
    case e +: r => increment(r, n-1, (e+1) +: out)
  }

  private def convert(str: String): Seq[Int] = {
    if(str.length != 4) Seq(0, 0, 0, 0)
    else {
      def aux(str: String, out: Seq[Int] = Seq()): Seq[Int] = str match {
        case "" => out.reverse
        case _ if str.head >= 65 && str.head <= 70 => aux(str.tail, (str.head - 65) +: out)
        case _ => aux(str.tail, 0 +: out)
      }
      aux(str)
    }
  }

}
