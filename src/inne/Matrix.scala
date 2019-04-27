package inne

class Matrix(private var layout: Seq[Seq[Double]]) {

  private def checkSize(length: Int = layout.head.size, lay: Seq[Seq[Double]] = layout.tail): Unit = lay match {
    case Seq() =>
    case h +: _ if h.size != length =>
      throw new IllegalArgumentException("Columns of the matrix are not of equal size")
    case _ +: t => checkSize(length, t)
  }
  if(layout.size > 1) checkSize()

  if(layout.nonEmpty && layout.head.isEmpty) {
    layout = Seq(): Seq[Seq[Double]]
  }

  private val rows = layout.size
  private val columns = if(rows == 0) 0 else layout.head.size

  /**
    * Number of columns in this matrix
    */
  def width(): Int = columns

  /**
    * Number of rows in this matrix
    */
  def height(): Int = rows

  /**
    * Size of this matrix
    */
  def size(): (Int, Int) = (rows, columns)

  /**
    * Selects the (i, j)-th element of this matrix
    */
  def get(i: Int, j: Int): Double = layout(i)(j)

  /**
    * Selects the i-th row of this matrix
    */
  def getRow(i: Int): Seq[Double] = layout(i)

  /**
    * Selects the j-th column of this matrix
    */
  def getColumn(j: Int): Seq[Double] = {
    def generate(out: Seq[Double] = Seq(), index: Int = 0): Seq[Double] = index match {
      case i if i == rows => out.reverse
      case i => generate(layout(i)(j) +: out, i+1)
    }
    generate()
  }

  /**
    * Adds this and a given matrix
    */
  def +(m: Matrix): Matrix = {
    if(size == m.size()) {
      merge(m, _+_)
    } else {
      throw new IllegalArgumentException("Matrices are not of equal size")
    }
  }

  /**
    * Subtracts this and a given matrix
    */
  def -(m: Matrix): Matrix = {
    if(size == m.size()) {
      merge(m, _-_)
    } else {
      throw new IllegalArgumentException("Matrices are not of equal size")
    }
  }

  /**
    * Multiplies this and a given matrix
    */
  def *(m: Matrix): Matrix = {
    if(columns != m.height()) throw new IllegalArgumentException("The number of columns of the left matrix must be " +
      "the same as the number of rows of the right matrix")
    def value(i: Int, j: Int, k: Int = 0, n: Int = 0, out: Double = 0.0): Double = n match {
      case _ if k == width() => out
      case _ => value(i, j, k+1, n+1, out + get(i, k)*m.get(n, j))
    }
    def aux(i: Int = 0, out: Seq[Seq[Double]] = Seq())(j: Int = 0, temp: Seq[Double] = Seq()): Seq[Seq[Double]] = j match {
      case _ if i == height() => out.reverse
      case _ if j == m.width() => aux(i+1, temp.reverse +: out)()
      case _ => aux(i, out)(j+1, value(i, j) +: temp)
    }
    new Matrix(aux()())
  }

  /**
    * Multiplies this matrix by a scalar
    */
  def *(d: Double): Matrix = map((a: Double) => a*d)

  def transpose: Matrix = ???

  def submatrix(i: Int, j: Int): Matrix = ???

  def minor(i: Int, j: Int): Matrix = ???

  def cofactor(i: Int, j: Int): Matrix = ???

  def adjugate: Matrix = ???

  def invert: Matrix = ???

  def trace: Double = ???
  def determinant: Double = ???

  /**
    * Tests whether this matrix is empty
    */
  def isEmpty: Boolean = rows == 0

  /**
    * Tests whether this matrix is a square matrix
    */
  def isSquare: Boolean = rows == columns

  def isDiagonal: Boolean = ???
  def isLowerDiagonal: Boolean = ???
  def isUpperDiagonal: Boolean = ???
  def isSymmetric: Boolean = ???
  def isInvertible: Boolean = ???
  def isOrthogonal: Boolean = ???

  /**
    * Applies function to every element of this function
    */
  def map(f: Double => Double): Matrix = {
    new Matrix(layout.map( (s: Seq[Double]) => s.map( (d: Double) => f(d) ) ))
  }

  def deleteRow(i: Int): Matrix = ???
  def deleteColumn(j: Int): Matrix = ???

  /**
    * Merges two matrices into a new one by applying function to every element of each matrices
    */
  def merge(m: Matrix, f: (Double, Double) => Double): Matrix = {
    def aux(i: Int = 0, out: Seq[Seq[Double]] = Seq())(j: Int = 0, row: Seq[Double] = Seq()): Seq[Seq[Double]] = j match {
      case _ if i == rows => out.reverse
      case k if k == columns => aux(i+1, row.reverse +: out)()
      case _ => aux(i, out)(j+1, f(get(i, j), m.get(i, j)) +: row)
    }
    new Matrix(aux()())
  }

  /**
    * Folds the elements of this matrix using the specified associative binary operator
    */
  def fold[A](f: (A, Double) => A)(out: A): A = {
    def aux(i: Int = 0, out: A = out)(j: Int = 0): A = j match {
      case _ if i == rows => out
      case k if k == columns => aux(i+1, out)()
      case _ => aux(i, f(out, get(i, j)))(j+1)
    }
    aux()()
  }

  /**
    * Returns sequence made of the elements of this matrix
    */
  def toSeq: Seq[Seq[Double]] = layout

  /**
    * Converts this matrix to a String
    */
  override def toString: String = {
    def aux(i: Int = 0, out: String = ""): String = i match {
      case _ if i == rows => out.dropRight(1)
      case _ => aux(i+1, out + "(" + getRow(i).foldLeft("")((s: String, d: Double) => s + ", " + d).drop(2) + ")\n")
    }
    aux()
  }

}
