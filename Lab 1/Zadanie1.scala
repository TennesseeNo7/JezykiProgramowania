import io.StdIn

object Zadanie1 extends App {

  println("Podaj liczbę cąłkowitą: ")

  var x = StdIn.readInt()
  var info = ""

  if(x % 2 == 0) {
    info = "parzysta"
  } else {
    info = "nieparzysta"
  }

  println("Liczba " +x+ " jest " +info)

}
