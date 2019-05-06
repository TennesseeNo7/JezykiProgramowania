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
    * @return width of this matrix
    */
  def width(): Int = columns

  /**
    * Number of rows in this matrix
    * @return height of this matrix
    */
  def height(): Int = rows

  /**
    * Size of this matrix
    * @return pair of numbers representing size of this matrix
    */
  def size(): (Int, Int) = (rows, columns)

  /**
    * Selects an element from this matrix
    * @param i - the index of row
    * @param j - index of column
    * @return element from (i, j) position of this matrix
    */
  def get(i: Int, j: Int): Double = layout(i)(j)

  /**
    * Selects the i-th row of this matrix
    * @param i - the index of row
    * @return i-th row
    */
  def getRow(i: Int): Seq[Double] = layout(i)

  /**
    * Selects the j-th column of this matrix
    * @param j - the index of column
    * @return j-th column
    */
  def getColumn(j: Int): Seq[Double] = {
    def generate(out: Seq[Double] = Seq(), index: Int = 0): Seq[Double] = index match {
      case i if i == rows => out.reverse
      case i => generate(layout(i)(j) +: out, i+1)
    }
    generate()
  }

  /**
    * Creates Seq made from elements from main diagonal of this matrix
    * @return sequence of elements from main diagonal
    */
  def getDiagonal: Seq[Double] = {
    def aux(i: Int = 0, out: Seq[Double] = Seq()): Seq[Double] = i match {
      case k if k == Math.min(rows, columns) => out.reverse
      case _ => aux(i+1, get(i, i) +: out)
    }
    aux()
  }

  /**
    * Creates Seq made from elements from anti-diagonal of this matrix
    * @return sequence of elements from anti-diagonal
    */
  def getAntiDiagonal: Seq[Double] = {
    if(!isSquare) throw new IllegalArgumentException("Matrix must be square")
    if(isEmpty) return Seq()
    def aux(i: Int = 0, j: Int = columns - 1, out: Seq[Double] = Seq()): Seq[Double] = i match {
      case k if k == rows => out.reverse
      case _ => aux(i+1, j-1, get(i, j) +: out)
    }
    aux()
  }

  /**
    * Adds this and a given matrix
    * @param m - any matrix
    * @return new matrix resulting from summing the two matrices
    */
  def +(m: Matrix): Matrix = {
    if(size == m.size()) {
      compose(m, _+_)
    } else {
      throw new IllegalArgumentException("Matrices are not of equal size")
    }
  }

  /**
    * Subtracts a given matrix from this given matrix
    * @param m - any matrix
    * @return new matrix resulting from subtracting the two matrices
    */
  def -(m: Matrix): Matrix = {
    if(size == m.size()) {
      compose(m, _-_)
    } else {
      throw new IllegalArgumentException("Matrices are not of equal size")
    }
  }

  /**
    * Multiplies this matrix by a given matrix
    * @param m - any matrix
    * @return new matrix resulting from multiplying the two matrices
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
    * @param d - any number
    * @return new matrix resulting from multiplying this matrix by the scalar
    */
  def *(d: Double): Matrix = map((a: Double) => a*d)

  /**
    * Changes the rows to the columns and columns to the rows
    * @return the transpose of this matrix
    */
  def transpose: Matrix = {
    def aux(j: Int = 0, out: Seq[Seq[Double]] = Seq()): Seq[Seq[Double]] = j match {
      case k if k == columns => out.reverse
      case k => aux(j+1, getColumn(k) +: out)
    }
    new Matrix(aux())
  }

  def ^(n: Int): Matrix = {
    if(!isSquare) throw new IllegalArgumentException("Only square matrices can be raised to a power")
    if(n < 0 && !isInvertible) throw new IllegalArgumentException("Matrix is not invertible")
    def aux(n: Int = n, out: Matrix = MatrixFactory.identityMatrix(width())): Matrix = n match {
      case k if k < -1 => aux(math.abs(n), this.invert)
      case 0 => out
      case _ => aux(n-1, out*this)
    }
    aux()
  }

  /**
    * Removes specified row and column from this matrix
    * @param i - the index of column
    * @param j - the index of row
    * @return matrix resulting from deleting the specified row and column
    */
  def submatrix(i: Int, j: Int): Matrix = this.deleteRow(i).deleteColumn(j)

  def toREF: Matrix = ???

  def toRREF: Matrix = ???

  // **** TODO
  def comatrix: Matrix = MatrixFactory.functionMatrix(size(), cofactor)

  def adjugate: Matrix = comatrix.transpose

  def invert: Matrix = adjugate*(1/determinant)

  def leftInvert: Matrix = ???

  def rightInvert: Matrix = ???
  // **** TODO

  /**
    * Sums every value from the main diagonal of this matrix
    * @return trace of this matrix or IllegalArgumentException if this matrix is empty
    */
  def trace: Double = {
    if (isEmpty) throw new IllegalArgumentException("Trace of empty matrix does not exist")
    else getDiagonal.sum
  }

  // **** TODO
  def minor(i: Int, j: Int): Double = ???

  def cofactor(i: Int, j: Int): Double = (if(i+j % 2 == 0) 1 else -1)*minor(i, j)

  def determinant: Double = ???

  def rank: Int = {
    if(isEmpty) 0
    else ???
  }
  // **** TODO

  /**
    * Compares this matrix to another one
    * @param m - any matrix
    * @return true if this matrix is the same size and has the same elements as the other matrix, false otherwise
    */
  def ==(m: Matrix):Boolean = {
    if(isEmpty && m.isEmpty) return true
    if(size() != m.size()) return false
    def check(i: Int = 0, j: Int = 0): Boolean = (i, j) match {
      case t if t == (rows, 0) => true
      case (a, b) if b == columns => check(a+1)
      case (a, b) if get(a, b) != m.get(a, b) => false
      case (a, b) => check(a, b+1)
    }
    check()
  }

  /**
    * Tests whether this matrix is empty
    * @return true if this matrix is empty, false otherwise
    */
  def isEmpty: Boolean = rows == 0

  /**
    * Tests whether this matrix has the same width and height
    * @return true if this matrix is a square matrix, false otherwise
    */
  def isSquare: Boolean = if(isEmpty) false else rows == columns

  /**
    * Tests whether every value of this matrix, except of values on the main diagonal which are not restricted,
    * is equal to 0.0
    * @return true if this matrix is a diagonal matrix, false otherwise
    */
  def isDiagonal: Boolean = {
    if(isEmpty) return false
    def check(i: Int = 0, j: Int = 0): Boolean = (i, j) match {
      case t if t == (rows, 0) => true
      case (a, b) if b == columns => check(a+1)
      case (a, b) if get(a, b) == 0 || a == b => check(a, b+1)
      case _ => false
    }
    check()
  }

  /**
    * Tests whether this matrix is either lower triangular or upper triangular
    * @return true if this matrix is a triangular matrix, false otherwise
    */
  def isTriangular: Boolean = isLowerTriangular || isUpperTriangular

  /**
    * Tests whether all elements above the main diagonal are equal to 0.0
    * @return true if this matrix is lower triangular, false otherwise
    */
  def isLowerTriangular: Boolean = {
    def aux(i: Int = 0, j: Int = 1): Boolean = i match {
      case k if k == height()-1 => true
      case _ if j == width() => aux(i+1, i+2)
      case _ if get(i, j) != 0 => false
      case _ => aux(i, j+1)
    }
    !isEmpty && isSquare && aux()
  }

  /**
    * Tests whether all elements below the main diagonal are equal to 0.0
    * @return true if this matrix is upper triangular, false otherwise
    */
  def isUpperTriangular: Boolean = {
    def aux(i: Int = 1, j: Int = 0): Boolean = i match {
      case k if k == height() => true
      case k if j == k => aux(i+1)
      case _ if get(i, j) != 0 => false
      case _ => aux(i, j+1)
    }
    !isEmpty && isSquare && aux()
  }

  /**
    * Test whether every value on the main diagonal is equal to 1.0 and every other element is equal to 0.0
    * @return true if this matrix is identity matrix, false otherwise
    */
  def isIdentity: Boolean = {
    if(isEmpty) return false
    if(!isSquare) return false
    def check(i: Int = 0, j: Int = 0): Boolean = (i, j) match {
      case t if t == (rows, 0) => true
      case (a, b) if b == columns => check(a+1)
      case (a, b) if a == b && get(a, b) == 1 => check(a, b+1)
      case (a, b) if a != b && get(a, b) == 0 => check(a, b+1)
      case _ => false
    }
    check()
  }

  /**
    * Test whether this matrix is symmetric along the main diagonal
    * @return true if this matrix is symmetric, false otherwise
    */
  def isSymmetric: Boolean = if(isEmpty || !isSquare) false else this == this.transpose

  /**
    * Tests whether this square matrix is not invertible.
    * @return true if this matrix is singular, false otherwise
    */
  def isSingular: Boolean = !isInvertible

  /**
    * Test whether this square matrix is both left invertible and right invertible.
    * @return true if this matrix is invertible, false otherwise
    */
  def isInvertible: Boolean = if(isEmpty || !isSquare) false else determinant != 0

  def isLeftInvertible: Boolean = rank == columns

  def isRightInvertible: Boolean = rank == rows

  def isREF: Boolean = ???

  def isRREF: Boolean = ???

  /**
    * Test whether this matrix is a square matrix whose columns and rows are orthogonal unit vectors.
    * @return true if this matrix is orthogonal, false otherwise
    */
  def isOrthogonal: Boolean = isInvertible && this.transpose == this.invert

  /**
    * Builds new matrix by applying function to all elements of this matrix
    * @param f - function to apply to each element
    * @return new matrix resulting from applying function f to each element of this matrix
    */
  def map(f: Double => Double): Matrix = {
    new Matrix(layout.map( (s: Seq[Double]) => s.map( (d: Double) => f(d) ) ))
  }

  /**
    * Deletes i-th row of this matrix
    * @param i - the index of the row
    * @return new matrix with i-th row deleted
    */
  def deleteRow(i: Int): Matrix = {
    def aux(m: Int = 0, out: Seq[Seq[Double]] = Seq()): Seq[Seq[Double]] = m match {
      case k if k == rows => out.reverse
      case k if k == i => aux(m+1, out)
      case _ => aux(m+1, getRow(m) +: out)
    }
    new Matrix(aux())
  }

  /**
    * Deletes j-th column of this matrix
    * @param j - the index of the column
    * @return new matrix with j-th column deleted
    */
  def deleteColumn(j: Int): Matrix = {
    def aux(i: Int = 0, out: Seq[Seq[Double]] = Seq())(m: Int = 0, temp: Seq[Double] = Seq()): Seq[Seq[Double]] = i match {
      case k if k == rows => out.reverse
      case _ if m == columns => aux(i+1, temp.reverse +: out)()
      case _ if m == j => aux(i, out)(m+1, temp)
      case _ => aux(i, out)(m+1, get(i, m) +: temp)
    }
    new Matrix(aux()())
  }

  /**
    * Creates new matrix made of elements formed by applying function to corresponding
    * elements of this and a given matrix
    * @param m - any matrix
    * @param f - the function
    * @return new matrix composed from this function and the two matrices
    */
  def compose(m: Matrix, f: (Double, Double) => Double): Matrix = {
    def aux(i: Int = 0, out: Seq[Seq[Double]] = Seq())(j: Int = 0, row: Seq[Double] = Seq()): Seq[Seq[Double]] = j match {
      case _ if i == rows => out.reverse
      case k if k == columns => aux(i+1, row.reverse +: out)()
      case _ => aux(i, out)(j+1, f(get(i, j), m.get(i, j)) +: row)
    }
    new Matrix(aux()())
  }

  /**
    * Folds the elements of this matrix using the specified associative binary operator
    * @param op - the binary operator
    * @param out - the start value
    */
  def fold[A](op: (A, Double) => A)(out: A): A = {
    def aux(i: Int = 0, out: A = out)(j: Int = 0): A = j match {
      case _ if i == rows => out
      case k if k == columns => aux(i+1, out)()
      case _ => aux(i, op(out, get(i, j)))(j+1)
    }
    aux()()
  }

  /**
    * Creates sequence made of sequences representing rows of this matrix
    * @return sequence of elements of this matrix
    */
  def toSeq: Seq[Seq[Double]] = layout

  /**
    * Creates two dimensional String representation of this matrix
    * @return two dimensional String representation of this matrix
    */
  def getArranged: String = {
    def aux(i: Int = 0, out: String = ""): String = i match {
      case _ if i == rows => out.dropRight(1)
      case _ => aux(i+1, out + "(" + getRow(i).foldLeft("")((s: String, d: Double) => s + ", " + d).drop(2) + ")\n")
    }
    if(isEmpty) "()" else aux()
  }

  /**
    * Converts this matrix to a String
    * @return String representation of this matrix
    */
  override def toString: String = {
    def aux(i: Int = 0, out: String = ""): String = i match {
      case _ if i == rows => out.dropRight(2)
      case _ => aux(i+1, out + "(" + getRow(i).foldLeft("")((s: String, d: Double) => s + ", " + d).drop(2) + "), ")
    }
    if(isEmpty) "Matrix[]" else "Matrix[" + aux() + "]"
  }

}
