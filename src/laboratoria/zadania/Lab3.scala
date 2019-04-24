package laboratoria.zadania

object Lab3 {

  // Zadanie 1
  def reverse(str: String): String = {
    if (str.length == 1) {
      str //return
    } else {
      "" + reverse(str.substring(1)) + str.charAt(0) //return
    }
  }

  //Zadanie2
  def palindrome(tab: Array[Int]): Boolean = {
    if (tab.length <= 1) return true
    if (tab(0) != tab(tab.length - 1)) return false
    palindrome(tab.takeRight(tab.length - 1).take(tab.length - 2)) //return
  }

  //Zadanie 3
  def count(str: String): Int = {
    def countAux(s: String, counter: Int = 0): Int = {
      if (str.length == 0) {
        counter //return
      } else {
        countAux(str.substring(1), counter + (if (96 < str.charAt(0) && str.charAt(0) < 123) 1 else 0)) //return
      }
    }
    countAux(str)
  }

  //Zadanie 4
  def prime(p: Int, div: Int = 2): Boolean = {
    def primeAux(p: Int, div: Int = 2): Boolean = {
      if (div == p) return true
      if (p % div == 0) {
        false //return
      } else {
        primeAux(p, div + 1) //return
      }
    }
    primeAux(p) //return
  }

  //Zadanie 5
  def fibonacci(n: Int, a: Int = 1, b: Int = 1): Int = {
    def fibonacciAux(n: Int, a: Int = 1, b: Int = 1): Int = {
      if (n == 0) b else fibonacciAux(n - 1, a + b, a) //return
    }
    fibonacciAux(n)
  }

  //Zadanie 6
  def printTriangle(n: Int): Unit = {
    def printTriangleAux(n: Int, values: Array[Int] = Array(1)): Unit = {
      if(values.length <= n) {
        printRow(values)
        println()
        printTriangleAux(n, recalculate(values) :+ 1)
      }
    }
    printTriangleAux(n)
  }

  def printRow(values: Array[Int]): Unit = {
    if(values.length != 0) {
      print(values.head)
      if(values.length != 1) print("   ")
      printRow(values.tail)
    }
  }

  def recalculate(values: Array[Int]): Array[Int] = {
    val temp = new Array[Int](values.length)
    temp(0) = 1
    var i = 1
    while(i < temp.length) {
      temp(i) = values(i) + values(i-1)
      i += 1
    }
    temp //return
  }

}
