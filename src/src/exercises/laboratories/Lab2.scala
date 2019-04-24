package src.exercises.laboratories

import io.StdIn

object Lab2 extends App {

  //Zadanie 1
  print("Podaj liczbę n:")

  var n = StdIn.readInt()

  var text = new Array[String](n)

  var i = 0

  while(i < n) {

    println("Podaj kolejny napis:")
    text(i) = StdIn.readLine()
    i += 1

  }

  var k = n

  do {
    i = 0
    while(i < k-1) {
      if(text(i).length > text(i+1).length) {
        //swap
        val temp = text(i)
        text(i) = text(i+1)
        text(i+1) = temp
      } else if(text(i).length == text(i+1).length) {
        if(text(i) > text(i+1)) {
          //swap lex
          val temp = text(i)
          text(i) = text(i+1)
          text(i+1) = temp
        }
      }
      i += 1
    }
    k -= 1
  } while(k > 1)

  i = 0
  while(i < n) {

    println(text(i))
    i += 1

  }

  //Zadanie 2
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
  i = 0

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

  //Zadanie 3
  /**
    * Zakładamy, że A=0, B=1, ... Z=24 - każdej literze przyporządkowujemy kod
    * Gdy szyfrujemy literę 'a' względem litery 'b' z klucza chcemy aby para (a, b) dała nam 'c' - zaszyfrowaną literę
    * Wtedy (a,b) = c = (a+b)%25
    * Na przykład (T, N) = G, czyli (19, 13)= 32%25 = 6 = G
    */
  val max = ('Z'-'A') + 1
  def code(a: Int, b:Int) : Int = return (a+b)%max

  println("Podaj słowo do zaszyfrowania:")
  var value = StdIn.readLine()

  println("Podaj klucz:")
  var key = StdIn.readLine()

  value = value.toUpperCase()
  key   = key.toUpperCase()

  //Delete white spaces
  i = 0
  while(i < value.length) {
    if(value.charAt(i) == ' ') {
      value = value.substring(0, i) + value.substring(i+1)
      i -= 1
    }
    i += 1
  }

  //extend key
  i = 0
  while(value.length - key.length != 0) {
    key = key + key.charAt(i)
    i += 1
    if(i == key.length) {
      i = 0
    }
  }

  var coded = ""

  //code letters
  i = 0
  while(i < value.length) {
    val a = value.charAt(i) - 65
    val b = key.charAt(i) - 65
    coded += (code(a, b)).toChar
    i += 1
  }

  println("Słowo po zaszyfrowaniu: " +coded)

}
