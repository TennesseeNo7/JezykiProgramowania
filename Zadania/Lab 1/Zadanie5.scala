import Zadanie4.number

import io.StdIn

object Zadanie5 {

  def testPrimality(number : Int): Boolean = {

    if(number == 2 || number == 3 || number == 5 || number == 7) { //4 pierwsze liczby pierwsze
      return true
    } else if(number < 11) { //liczby złożone mniejsze od 11
      return false
    }  else {
      val x = number%10
      if(x == 1 || x == 3 || x == 7 || x == 9) { // liczby pierwsze kończą się na 1 lub 3 lub 7 lub 9
        if(number%2 == 0 || number%3 == 0 || number%5 == 0 || number%7 == 0) {
          return false
        } else {

          var nextDivider = 11
          var counter = 2
          val limit = Math.sqrt(number) + 1

          while(nextDivider < limit) { // dzielenie liczby przez kolejne liczby, ktore mogą być pierwszymi od 11
            if(number%nextDivider == 0) {
              return false
            } else {
              if(counter == 3) {
                counter = 0
                nextDivider += 4
              } else {
                counter += 1
                nextDivider += 2
              }
            }
          }

        }
      } else {
        return false
      }
    }
    return true
  }

  def main(args: Array[String]) {

    println("Podaj liczbę parzystą: ")
    var x = StdIn.readInt()

    while(x%2 == 1) {
      println("Podaj liczbę PARZYSTĄ: ")
      x = StdIn.readInt()
    }

    var p = 0
    var q = 0

    if(x == 4) {
      p = 2
      q = 2
    } else {
      p = 3
      q = x - 3
      while(!testPrimality(p) || !testPrimality(q)) {
        p = p + 2
        q = q - 2
      }

    }

    println("Liczba jest sumą dwóch liczb pierwszych: " +x+ " = " +p+ " + " +q ) //Hipoteza Goldbacha prawdziwa dla n < 4*10^18

  }

}
