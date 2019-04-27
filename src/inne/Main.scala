package inne

object Main extends App {

  val A = new Matrix(Seq(Seq(1, 2), Seq(3, 4), Seq(2, 2)))
  val B = new Matrix(Seq(Seq(0, 1), Seq(0, 0)))

  println(A*B)

}
