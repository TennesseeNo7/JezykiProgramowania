package inne

object MatrixFactory {

  /**
    * Creates new square matrix with main diagonal filled with ones and other values equal to zero
    * @param size - the size of the matrix
    * @return new identity matrix
    */
  def identityMatrix(size: Int): Matrix = {
    if(size < 1) throw new IllegalArgumentException("Matrix size cannot be less than 1")
    def fillRow(i: Int, j: Int = 0, out: Seq[Double] = Seq()): Seq[Double] = j match {
      case k if k == size => out.reverse
      case k if k == i => fillRow(i, j+1, 1.0 +: out)
      case _ => fillRow(i, j+1, 0.0 +: out)
    }
    def fill(i: Int = 0, out: Seq[Seq[Double]] = Seq()): Seq[Seq[Double]] = i match {
      case k if k == size => out.reverse
      case _ => fill(i+1, fillRow(i) +: out)
    }
    new Matrix(fill())
  }

  /**
    * Creates new matrix with values equal to zero
    * @param size - height by width size of the matrix
    * @return new zero matrix
    */
  def zeroMatrix(size: (Int, Int)): Matrix = {
    if(size._1 < 1 || size._2 < 1) throw new IllegalArgumentException("Matrix size cannot be less than 1")
    def fillRow(i: Int, j: Int = 0, out: Seq[Double] = Seq()): Seq[Double] = j match {
      case k if k == size._2 => out.reverse
      case _ => fillRow(i, j+1, 0.0 +: out)
    }
    def fill(i: Int = 0, out: Seq[Seq[Double]] = Seq()): Seq[Seq[Double]] = i match {
      case k if k == size._1 => out.reverse
      case _ => fill(i+1, fillRow(i) +: out)
    }
    new Matrix(fill())
  }

  /**
    * Creates new square matrix with values equal to zero
    * @param size - size of the matrix
    * @return new zero matrix
    */
  def zeroMatrix(size: Int): Matrix = {
    if(size < 1) throw new IllegalArgumentException("Matrix size cannot be less than 1")
    def fillRow(i: Int, j: Int = 0, out: Seq[Double] = Seq()): Seq[Double] = j match {
      case k if k == size => out.reverse
      case _ => fillRow(i, j+1, 0.0 +: out)
    }
    def fill(i: Int = 0, out: Seq[Seq[Double]] = Seq()): Seq[Seq[Double]] = i match {
      case k if k == size => out.reverse
      case _ => fill(i+1, fillRow(i) +: out)
    }
    new Matrix(fill())
  }

  def functionMatrix(size: (Int, Int), f: (Int, Int) => Double): Matrix = {
    if(size._1 < 0 || size._2 < 0) throw new IllegalArgumentException("Matrix size cannot be less than 0")
    def fillRow(i: Int, j: Int = 0, out: Seq[Double] = Seq()): Seq[Double] = j match {
      case k if k == size._2 => out.reverse
      case _ => fillRow(i, j+1, f(i, j) +: out)
    }
    def fill(i: Int = 0, out: Seq[Seq[Double]] = Seq()): Seq[Seq[Double]] = i match {
      case k if k == size._1 => out.reverse
      case _ => fill(i+1, fillRow(i) +: out)
    }
    new Matrix(fill())
  }

}
