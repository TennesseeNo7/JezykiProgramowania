package inne

object Main extends App {

  //val A: Matrix = MatrixFactory.functionMatrix((26, 26), (i: Int, j: Int) => (i^j + 3)%8)
  //println(A.getArranged)
/*
  val A = MatrixFactory.functionMatrix((1, 26), (_: Int, j: Int) => j).toSingleSec
  println(A)
  val A1 = A.map((a: Double) => (2*a + 3)%26)
  println(A1)
  val A2 = A.map((a: Double) => (math.pow(a, 11) + 4)%26)
  println(A2)
*/

  val text = "ccccccccccccccccccccccccc"
  println(code(text, 13))

  //h = 7
  // 11
  // (7^11 + 4) mod 26
  // h = 7
  // 13
  // (7^13 + 4) mod 26

  def code(text: String, key: Int):String = {

    def nextPrime(p: Int): Int = {
      def aux(p: Int): Int = {
        var x = 2
        while(true) {
          if(p == x) return p
          else if(p%x == 0) return aux(p+1)
          else x += 1
        }
        -1
      }
      aux(p+1)
    }

    var P = key

    def aux(text: String, out: String = ""): String = {
      if(text.isEmpty) { out }
      else {
        if(text.head == ' ') {
          aux(text.tail, out + " ")
        } else {
          P = nextPrime(P)
          println(P)
          aux(text.tail, out + ((math.pow(text.head - 'a', P) + 4)%26 + 'a').toChar)
        }
      }
    }
    aux(text)
  }

}
