package inne

object Main extends App {

  val A = new Matrix(Seq(Seq(1, 2), Seq(3, 4), Seq(2, 2)))
  val B = new Matrix(Seq(Seq(0, 1), Seq(0, 0)))
  val C = new Matrix(Seq(Seq(4, 2, 3), Seq(0, 2, 2), Seq(0, 0, -9)))
  val EMPTY = new Matrix(Seq())
  val D = new Matrix(Seq(Seq(1, 7, 3), Seq(5, 4, -5), Seq(3, -5, 6)))
  val DD = new Matrix(Seq(Seq(1, 0, 0), Seq(1, 4, 0), Seq(3, -5, 6)))
  val E = new Matrix(Seq(Seq(1, 0), Seq(0, 4), Seq(0, 0)))
  val F = new Matrix(Seq(Seq(1, 2), Seq(3, 5), Seq(2, 2)))
  val G = new Matrix(Seq(Seq(1, 2), Seq(5, 4), Seq(2, 2)))
  val H = C.transpose

  val mf = MatrixFactory

  println(mf.functionMatrix((3, 3), (i: Int, j: Int) => (i + j)*i*j))

}
