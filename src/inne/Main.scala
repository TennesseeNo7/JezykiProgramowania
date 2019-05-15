package inne

object Main extends App {

  val A = MatrixFactory.functionMatrix((6, 6), (i: Int, j: Int) => (i^2 + j^2)*(i + j)/(math.abs(i - j)+1))
  println(A.getArranged)
  println(A.isSymmetric)

}
