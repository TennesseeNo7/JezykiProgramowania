package inne

object Main extends App {

  val A = new Matrix(Seq(Seq(1, 2), Seq(3, 4), Seq(2, 2)))
  val B = new Matrix(Seq(Seq(0, 1), Seq(0, 0)))
  val C = new Matrix(Seq(Seq(1, 2, 3), Seq(4, 5, 6), Seq(7, 8, 9)))
  val EMPTY = new Matrix(Seq())
  val D = new Matrix(Seq(Seq(1, 2, 3), Seq(4, 5, 6), Seq(7, 8, 9)))
  val E = new Matrix(Seq(Seq(1, 2), Seq(3, 4), Seq(2, 2)))
  val F = new Matrix(Seq(Seq(1, 2), Seq(3, 5), Seq(2, 2)))
  val G = new Matrix(Seq(Seq(1, 2), Seq(5, 4), Seq(2, 2)))

  println(A == C)
  println(C == C)
  println(A == A)
  println(A == E)
  println(C == D)
  println(EMPTY == EMPTY)
  println(E == F)
  println(E == G)

}
