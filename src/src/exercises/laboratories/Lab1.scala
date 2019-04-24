package src.exercises.laboratories

import io.StdIn

object Lab1 extends App {

  //Zadanie 1
  println("Podaj liczbę cąłkowitą: ")

  var x = StdIn.readInt()
  var info = ""

  if(x % 2 == 0) {
    info = "parzysta"
  } else {
    info = "nieparzysta"
  }

  println("Liczba " +x+ " jest " +info)

  //Zadanie 2
  var positive = 0
  var negative = 0

  var next = 0.0

  do {
    print("Podaj liczbę rzeczywistą:")
    next = StdIn.readDouble()
    if (next > 0.0) {
      positive += 1
    } else if(next < 0.0) {
      negative += 1
    }
  } while (next != 0.0)

  println("Wpisałeś " +positive+ " liczb dodatnich i " +negative+ " liczb ujemnych")

  //Zadanie 3
  println("Podaj dwie liczby: ")

  var a = StdIn.readInt()
  var b = StdIn.readInt()

  var nwd = "NWD(" +a+ ", " +b+ ") = "

  var remainder = 1

  do {
    remainder = a%b
    a = b
    b = remainder
  } while(remainder != 0)

  print(nwd + a)

  //Zadanie 4
  print("Podaj liczbę naturalną: ")

  var number = StdIn.readInt()

  var isPrime = true

  if(number == 2 || number == 3 || number == 5 || number == 7) { //4 pierwsze liczby pierwsze
    isPrime = true
  } else if(number < 11) { //liczby złożone mniejsze od 11
    isPrime = false
  }  else {
    val x = number%10
    if(x == 1 || x == 3 || x == 7 || x == 9) { // liczby pierwsze kończą sięna 1 lub 3 lub 7 lub 9
      if(number%2 == 0 || number%3 == 0 || number%5 == 0 || number%7 == 0) {
        isPrime = false
      } else {

        var nextDivider = 11
        var counter = 2
        val limit = Math.sqrt(number) + 1

        while(nextDivider < limit) { // dzielenie liczby przez kolejne liczby, ktore mogą być pierwszymi od 11
          if(number%nextDivider == 0) {
            isPrime = false
            nextDivider = number // zamiast "break"
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
      isPrime = false;
    }
  }

  if(isPrime) {
    println("Liczba " +number+ " jest liczbą pierwszą")
  } else {
    println("Liczba " +number+ " nie jest liczbą pierwszą")
  }

  //Zadanie 5
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

  println("Podaj liczbę parzystą: ")
  x = StdIn.readInt()

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
