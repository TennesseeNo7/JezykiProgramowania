import io.StdIn

object Zadanie4  extends App {

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

}
