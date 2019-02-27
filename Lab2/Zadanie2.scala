import io.StdIn

object Zadanie2 {

  def main(args: Array[String]) {

    println("Podaj pierwszą liczbę:")
    var a = StdIn.readLine()

    println("Podaj drugą liczbę:")
    var b = StdIn.readLine()

    // (a > b)
    if(b.length > a.length) {
      val temp = a
      a = b
      b = temp
    }

    a =  "0" + a
    while(a.length != b.length) {
      b = "0" + b
    }

    val operations = a.length + 1
    var i = 0

    var sum = ""
    var COUT = 0

    while(i < operations - 1) {

      val A : Int = ("" + a.charAt(operations - i - 2)).toInt
      val B : Int = ("" + b.charAt(operations - i - 2)).toInt

      val SUM = A+B+COUT
      if(SUM >= 10) {
        COUT = 1
      } else {
        COUT = 0
      }

      sum = (SUM%10) + sum

      i += 1
    }

    if(sum.charAt(0) == 48) {
      sum = sum.substring(1)
    }

    print("Suma tych liczb: ")
    println(sum)

  }

}


